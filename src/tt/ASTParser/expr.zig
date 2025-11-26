const Keyword = types.Keyword;
const EltRef = *const ASTElement;
const ParseFn = fn (*ASTParser) Error!EltRef;

fn parseList(p: *ASTParser, end: Keyword, require_commas: bool) Error![]EltRef {
    var list: std.ArrayListUnmanaged(EltRef) = .empty;
    while (true) {
        if (p.eof())
            return Error.MissingTerminal;
        if (p.nextKeywordIs(end)) {
            try p.advance();
            break;
        }
        const item = try parseExpr(p);
        try list.append(p.gpa, item);

        if (p.nextKeywordIs(.@",")) {
            try p.advance();
        } else if (require_commas) {
            return Error.MissingComma;
        }
    }
    return try list.toOwnedSlice(p.gpa);
}

fn parseObject(p: *ASTParser) Error!EltRef {
    var keys: std.ArrayListUnmanaged(EltRef) = .empty;
    var values: std.ArrayListUnmanaged(EltRef) = .empty;
    const state = p.state;
    try p.advance();

    while (true) {
        if (p.eof())
            return Error.MissingBrace;
        if (p.nextKeywordIs(.@"}")) {
            try p.advance();
            break;
        }

        const key = try parseExpr(p);
        switch (key.*.node) {
            .assign_stmt => |a| {
                try keys.append(p.gpa, a.lvalue);
                try values.append(p.gpa, a.rvalue);
            },
            else => {
                try keys.append(p.gpa, key);

                if (!p.nextKeywordIs(.@"=>"))
                    return Error.MissingFatArrow;
                try p.advance();

                try values.append(p.gpa, try parseExpr(p));
            },
        }

        if (p.nextKeywordIs(.@",")) {
            try p.advance();
        } else if (p.nextKeywordIs(.@"}")) {
            try p.advance();
            break;
        }
    }

    assert(keys.items.len == values.items.len);

    return try p.newNode(
        .{ .object = .{
            .keys = try keys.toOwnedSlice(p.gpa),
            .values = try values.toOwnedSlice(p.gpa),
        } },
        state.loc,
    );
}

fn allowed(allow: []const Keyword, kw: Keyword) bool {
    for (allow) |k| if (k == kw) return true;
    return false;
}

fn parseBinOp(p: *ASTParser, allow: []const Keyword, parseNext: ParseFn) Error!EltRef {
    var lhs = try parseNext(p);
    while (!p.eof()) {
        const state = p.state;
        switch (state.tok.?) {
            .keyword => |op| {
                if (allowed(allow, op)) {
                    try p.advance();
                    const rhs = try parseNext(p);
                    lhs = try p.newNode(
                        .{ .binary_op = .{ .op = op, .lhs = lhs, .rhs = rhs } },
                        state.loc,
                    );
                } else {
                    break;
                }
            },
            else => break,
        }
    }
    return lhs;
}

fn parseVar(p: *ASTParser) Error!EltRef {
    const state = p.state;
    switch (state.tok.?) {
        .keyword => |kw| {
            if (kw == .@"$") {
                try p.advance();
                const ref = try parseVar(p);
                return try p.newNode(.{ .ref = ref }, state.loc);
            }
            return Error.SyntaxError;
        },
        .symbol => |sym| {
            try p.advance();
            return try p.newNode(.{ .symbol = sym }, state.loc);
        },
        .int => |int| {
            try p.advance();
            return try p.newNode(.{ .int = int }, state.loc);
        },
        else => return Error.SyntaxError,
    }
}

fn parseCall(p: *ASTParser) Error!EltRef {
    var call = try parseVar(p);
    const state = p.state;
    while (p.nextKeywordIs(.@"(")) {
        try p.advance();
        const args = try parseList(p, .@")", true);
        call = try p.newNode(
            .{ .call = .{ .method = call, .args = args } },
            state.loc,
        );
    }
    return call;
}

fn parseRef(p: *ASTParser) Error!EltRef {
    return parseBinOp(p, &.{.@"."}, parseCall);
}

fn parenthesise(p: *const ASTParser, elt: EltRef) Error!EltRef {
    return switch (elt.*.node) {
        .assign_stmt => |a| try p.newNode(
            .{ .assign_expr = .{ .lvalue = a.lvalue, .rvalue = a.rvalue } },
            elt.loc,
        ),
        else => elt,
    };
}

fn parseSingleQuotedString(p: *ASTParser, str: []const u8) Error!EltRef {
    const state = p.state;
    try p.advance();

    var ss = StringScanner{ .str = str };
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(p.gpa);

    while (!ss.eof()) {
        const nc = ss.next();
        if (nc == '\\' and !ss.eof()) {
            const qc = ss.next();
            if (qc != '\\' and qc != '\'')
                try buf.append(p.gpa, nc);
            try buf.append(p.gpa, qc);
        } else {
            try buf.append(p.gpa, nc);
        }
    }

    return p.newNode(
        .{ .string = try buf.toOwnedSlice(p.gpa) },
        state.loc,
    );
}

fn parseDoubleQuotedLiteral(p: *ASTParser, literal: []const u8) Error!EltRef {
    const state = p.state;
    var ss = StringScanner{ .str = literal };
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(p.gpa);

    while (!ss.eof()) {
        const nc = ss.next();
        if (nc == '\\' and !ss.eof()) {
            const qc = ss.next();
            switch (qc) {
                '$', '\\', '\"' => try buf.append(p.gpa, qc),
                'a' => try buf.append(p.gpa, 0x07),
                'b' => try buf.append(p.gpa, 0x08),
                't' => try buf.append(p.gpa, 0x09),
                'n' => try buf.append(p.gpa, 0x0a),
                'f' => try buf.append(p.gpa, 0x0c),
                'r' => try buf.append(p.gpa, 0x0d),
                'e' => try buf.append(p.gpa, 0x1b),
                'o' => {
                    if (ss.available() < 3) return Error.BadString;
                    const cc = try std.fmt.parseInt(u8, ss.take(3), 8);
                    try buf.append(p.gpa, cc);
                },
                'x' => {
                    if (ss.available() < 2) return Error.BadString;
                    const cc = try std.fmt.parseInt(u8, ss.take(2), 16);
                    try buf.append(p.gpa, cc);
                },
                else => return Error.BadString,
            }
        } else {
            try buf.append(p.gpa, nc);
        }
    }

    return p.newNode(
        .{ .string = try buf.toOwnedSlice(p.gpa) },
        state.loc,
    );
}

fn parseInterpolation(p: *ASTParser, interp: []const u8) Error!EltRef {
    var node: ?EltRef = null;
    var pos: usize = 0;
    while (pos < interp.len) : (pos += 1) {
        const start = pos;
        while (pos < interp.len and interp[pos] != '.')
            pos += 1;
        if (pos == start) return Error.SyntaxError;
        const rhs = try p.newNode(
            .{ .symbol = interp[start..pos] },
            p.state.loc,
        );
        node = if (node) |lhs|
            try p.newNode(
                .{ .binary_op = .{ .op = .@".", .lhs = lhs, .rhs = rhs } },
                p.state.loc, // TODO that's not right
            )
        else
            rhs;
    }
    return node.?;
}

fn parseDoubleQuotedString(p: *ASTParser, str: []const u8) Error!EltRef {
    const state = p.state;
    try p.advance();
    var node: ?EltRef = null;
    var si = StringIter{ .ss = StringScanner{ .str = str } };

    while (si.next()) |tok| {
        const rhs = switch (tok) {
            .literal => |l| try parseDoubleQuotedLiteral(p, l),
            .interp => |i| try parseInterpolation(p, i),
        };
        node = if (node) |lhs|
            try p.newNode(
                .{ .binary_op = .{ .op = ._, .lhs = lhs, .rhs = rhs } },
                state.loc,
            )
        else
            rhs;
    }

    if (node) |n|
        return n;

    // Default empty string
    return p.newNode(
        .{ .string = "" },
        state.loc,
    );
}

fn parseAtom(p: *ASTParser) Error!EltRef {
    const state = p.state;
    switch (state.tok.?) {
        .keyword => |kw| {
            switch (kw) {
                .@"-", .NOT => |op| {
                    try p.advance();
                    const arg = try parseAtom(p);
                    return try p.newNode(
                        .{ .unary_op = .{ .op = op, .arg = arg } },
                        state.loc,
                    );
                },
                .@"(" => {
                    const res = try advanceAndParseExpr(p);
                    // TODO assign_stmt -> assign_expr
                    if (!p.nextKeywordIs(.@")"))
                        return Error.MissingParen;
                    try p.advance();
                    return parenthesise(p, res);
                },
                .@"$" => {
                    return try parseRef(p);
                },
                .@"[" => {
                    try p.advance();
                    const array = try parseList(p, .@"]", false);
                    return try p.newNode(
                        .{ .array = array },
                        state.loc,
                    );
                },
                .@"{" => {
                    return try parseObject(p);
                },
                else => return Error.SyntaxError,
            }
        },
        .int => |n| {
            const node = try p.newNode(.{ .int = n }, state.loc);
            try p.advance();
            return node;
        },
        .float => |n| {
            const node = try p.newNode(.{ .float = n }, state.loc);
            try p.advance();
            return node;
        },
        .symbol => {
            return try parseRef(p);
        },
        .sq_string => |s| {
            return try parseSingleQuotedString(p, s);
        },
        .dq_string => |s| {
            return try parseDoubleQuotedString(p, s);
        },
        else => return Error.SyntaxError,
    }
}

fn parseAssign(p: *ASTParser) Error!EltRef {
    const lvalue = try parseAtom(p);
    const state = p.state;
    if (p.nextKeywordIs(.@"=")) {
        const rvalue = try advanceAndParseExpr(p);
        return p.newNode(
            .{ .assign_stmt = .{ .lvalue = lvalue, .rvalue = rvalue } },
            state.loc,
        );
    }
    return lvalue;
}

fn parseMulDiv(p: *ASTParser) Error!EltRef {
    return try parseBinOp(
        p,
        &.{ .@"*", .@"/", .DIV, .MOD },
        parseAssign,
    );
}

fn parseAddSub(p: *ASTParser) Error!EltRef {
    return try parseBinOp(
        p,
        &.{ .@"+", .@"-", ._ },
        parseMulDiv,
    );
}

fn parseInequality(p: *ASTParser) Error!EltRef {
    return try parseBinOp(
        p,
        &.{ .@"<", .@"<=", .@">", .@">=", .@"==", .@"!=" },
        parseAddSub,
    );
}

fn parseAnd(p: *ASTParser) Error!EltRef {
    return try parseBinOp(p, &.{.AND}, parseInequality);
}

fn parseOr(p: *ASTParser) Error!EltRef {
    return try parseBinOp(p, &.{.OR}, parseAnd);
}

fn parseCond(p: *ASTParser) Error!EltRef {
    const cond = try parseOr(p);

    if (!p.nextKeywordIs(.@"?"))
        return cond;

    const state = p.state;

    const THEN = try advanceAndParseExpr(p);

    if (!p.nextKeywordIs(.@":"))
        return Error.MissingColon;

    const ELSE = try advanceAndParseExpr(p);

    return p.newNode(
        .{ .if_op = .{ .cond = cond, .THEN = THEN, .ELSE = ELSE } },
        state.loc,
    );
}

fn advanceAndParseExpr(p: *ASTParser) Error!EltRef {
    try p.advance();
    return try parseExpr(p);
}

pub fn parseExpr(p: *ASTParser) Error!EltRef {
    return try parseCond(p);
}

test "expr" {
    const cases = &[_]struct { src: []const u8, want: []const u8 }{
        .{ .src = "[% 1 %]", .want = "1" },
        .{ .src = "[% -1 %]", .want = "-1" },
        .{ .src = "[% (-(1)) %]", .want = "-1" },
        .{ .src = "[% 1 + 3.5 %]", .want = "(1 + 3.5)" },
        .{ .src = "[% 1 + 3 * 2 %]", .want = "(1 + (3 * 2))" },
        .{ .src = "[% (1 + 3) * 2 %]", .want = "((1 + 3) * 2)" },
        .{ .src = "[% foo %]", .want = "foo" },
        .{ .src = "[% foo.0 %]", .want = "foo.0" },
        .{ .src = "[% foo.0.1 %]", .want = "foo.0.1" },
        .{ .src = "[% $$foo %]", .want = "$$foo" },
        .{ .src = "[% foo.bar %]", .want = "foo.bar" },
        .{ .src = "[% $foo.bar %]", .want = "$foo.bar" },
        .{ .src = "[% foo.$bar %]", .want = "foo.$bar" },
        .{ .src = "[% !a || b && c %]", .want = "(NOT a OR (b AND c))" },
        .{ .src = "[% !(a || b && c) %]", .want = "NOT (a OR (b AND c))" },
        .{ .src = "[% a <> 1 %]", .want = "(a != 1)" },
        .{ .src = "[% foo(1, 2, 3) %]", .want = "foo(1, 2, 3)" },
        .{ .src = "[% bar(1/2) %]", .want = "bar((1 / 2))" },
        .{ .src = "[% bar(1/2)(!4) %]", .want = "bar((1 / 2))(NOT 4)" },
        .{ .src = "[% bar(1/2)(!pog(3)).foo %]", .want = "bar((1 / 2))(NOT pog(3)).foo" },
        .{ .src = "[% [ ] %]", .want = "[]" },
        .{ .src = "[% [1, -2, 3] %]", .want = "[1, -2, 3]" },
        .{ .src = "[% [1 -2 3] %]", .want = "[(1 - 2), 3]" },
        .{ .src = "[% [1 (-2) 3] %]", .want = "[1, -2, 3]" },
        .{ .src = "[% [a (3)] %]", .want = "[a(3)]" },
        .{ .src = "[% [a, (3)] %]", .want = "[a, 3]" },
        .{ .src = "[% [bar(1/2)(!pog(3)).foo] %]", .want = "[bar((1 / 2))(NOT pog(3)).foo]" },
        .{ .src = "[% a = 3 %]", .want = "a = 3" },
        .{ .src = "[% ((a = 3)) %]", .want = "(a = 3)" },
        .{ .src = "[% a = (b = 3) %]", .want = "a = (b = 3)" },
        .{ .src = "[% 'Hello' %]", .want = "\"Hello\"" },
        .{ .src = "[% \"Hello\" %]", .want = "\"Hello\"" },
        .{ .src = "[% \"Hello\\x00\" %]", .want = "\"Hello\\x00\"" },
        .{ .src = "[% \"Hello\\x41\" %]", .want = "\"HelloA\"" },
        .{ .src = "[% \"Hello $name\" %]", .want = "(\"Hello \" _ name)" },
        .{ .src = "[% \"Hello $name.first\" %]", .want = "(\"Hello \" _ name.first)" },
        .{ .src = "[% \"$name\" %]", .want = "name" },
        .{ .src = "[% \"$foo$bar$baz\" %]", .want = "((foo _ bar) _ baz)" },
        .{ .src = "[% a ? 0 : 1 %]", .want = "(a ? 0 : 1)" },
        .{ .src = "[% a ? b ? 1 : 2 : c ? 3 : 4 %]", .want = "(a ? (b ? 1 : 2) : (c ? 3 : 4))" },
        .{ .src = "[% {} %]", .want = "{}" },
        .{ .src = "[% {a => 1} %]", .want = "{a => 1}" },
        .{ .src = "[% {a = 1} %]", .want = "{a => 1}" },
        .{ .src = "[% {a => 1, b = 2} %]", .want = "{a => 1, b => 2}" },
        .{ .src = "[% {a = 1 b = 2} %]", .want = "{a => 1, b => 2}" },
        .{ .src = "[% {(a = 1) = 2} %]", .want = "{(a = 1) => 2}" },
        .{ .src = "[% header(title='Hello World') %]", .want = "header(title = \"Hello World\")" },
        .{
            .src = "[% header(title=\"Hello $name\") %]",
            .want = "header(title = (\"Hello \" _ name))",
        },
    };

    for (cases) |case| {
        var alloc = std.heap.ArenaAllocator.init(testing.allocator);
        defer alloc.deinit();
        const gpa = alloc.allocator();

        const iter = TokenIter.init(case.src);
        var parser = try ASTParser.init(gpa, iter);

        try testing.expectEqual(types.Token{ .start = .{} }, parser.state.tok.?);
        try parser.advance();
        const elt = try parser.parseExpr();
        try testing.expectEqual(types.Token{ .end = .{} }, parser.state.tok.?);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        var w = std.Io.Writer.Allocating.fromArrayList(gpa, &buf);
        defer w.deinit();
        try w.writer.print("{f}", .{elt});
        var output = w.toArrayList();
        defer output.deinit(gpa);
        // std.debug.print("{s}\n", .{output.items});
        try testing.expectEqualDeep(case.want, output.items);
    }
}

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const types = @import("../types.zig");

const TokenIter = @import("../TokenIter.zig");

const ASTParser = @import("../ASTParser.zig");
const Error = ASTParser.Error;
const ASTNode = @import("../node.zig").ASTNode;
const ASTElement = @import("../node.zig").ASTElement;

const StringScanner = @import("../StringScanner.zig");
const StringIter = @import("../StringIter.zig");
