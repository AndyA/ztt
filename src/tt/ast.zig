fn isInterp(chr: u8) bool {
    return std.ascii.isAlphanumeric(chr) or chr == '_' or chr == '.';
}

const ASTError = toker.TokerError || Allocator.Error || error{
    Overflow,
    MissingParen,
    MissingTerminal,
    MissingComma,
    MissingColon,
    BadString,
};

const EltRef = *const ASTElement;

const ParserState = struct {
    tok: ?Token = null,
    loc: Location = .{},

    fn eof(self: *const ParserState) bool {
        return self.tok == null;
    }
};

const StringToken = union(enum) {
    literal: []const u8,
    interp: []const u8,
};

const StringScanner = struct {
    const SS = @This();
    str: []const u8,
    pos: usize = 0,

    pub fn available(self: SS) usize {
        return self.str.len - self.pos;
    }

    pub fn eof(self: SS) bool {
        return self.available() == 0;
    }

    pub fn peek(self: SS) u8 {
        assert(!self.eof());
        return self.str[self.pos];
    }

    pub fn next(self: *SS) u8 {
        assert(!self.eof());
        defer self.pos += 1;
        return self.str[self.pos];
    }

    pub fn take(self: *SS, len: usize) []const u8 {
        assert(self.available() >= len);
        defer self.pos += len;
        return self.str[self.pos .. self.pos + len];
    }

    pub fn nextToken(self: *SS) ?StringToken {
        if (self.eof()) return null;
        const nc = self.next();
        if (nc == '$') {
            const start = self.pos;
            while (!self.eof() and isInterp(self.peek()))
                _ = self.next();

            return if (self.pos == start)
                .{ .literal = "$" }
            else
                .{ .interp = self.str[start..self.pos] };
        } else {
            const start = self.pos - 1;
            while (!self.eof() and self.peek() != '$') {
                if (self.next() == '\\' and !self.eof())
                    _ = self.next();
            }
            return .{ .literal = self.str[start..self.pos] };
        }
    }
};

pub const ASTParser = struct {
    const Self = @This();
    const ParseFn = fn (*Self) ASTError!EltRef;

    gpa: Allocator,
    iter: TokenIter,
    state: ParserState = .{},

    pub fn init(gpa: Allocator, iter: TokenIter) ASTError!Self {
        var self = Self{ .gpa = gpa, .iter = iter };
        try self.nextState();
        return self;
    }

    fn nextState(self: *Self) ASTError!void {
        const tok = self.iter.next();
        self.state.loc = self.iter.getTokenStart();
        self.state.tok = try tok;
    }

    fn eof(self: *const Self) bool {
        return self.state.eof();
    }

    fn advance(self: *Self) ASTError!void {
        assert(!self.eof());
        try self.nextState();
    }

    fn newNode(self: *const Self, node: ASTNode, loc: Location) Allocator.Error!*ASTElement {
        return try ASTElement.create(self.gpa, node, loc);
    }

    fn nextKeywordIs(self: *Self, want: Keyword) bool {
        if (self.state.tok) |tok| {
            return switch (tok) {
                .keyword => |kw| kw == want,
                else => false,
            };
        }
        return false;
    }

    fn allowed(allow: []const Keyword, kw: Keyword) bool {
        for (allow) |k| if (k == kw) return true;
        return false;
    }

    fn parseList(self: *Self, end: Keyword, require_commas: bool) ASTError![]EltRef {
        var list: std.ArrayListUnmanaged(EltRef) = .empty;
        while (true) {
            if (self.eof())
                return ASTError.MissingTerminal;
            if (self.nextKeywordIs(end)) {
                try self.advance();
                break;
            }
            const item = try self.parseExpr();
            try list.append(self.gpa, item);
            // TODO calling nextKeywordIs a lot here...
            if (self.nextKeywordIs(.@",")) {
                try self.advance();
            } else if (self.nextKeywordIs(end)) {
                try self.advance();
                break;
            } else if (require_commas) {
                return ASTError.MissingComma;
            }
        }
        return list.items;
    }

    fn parseBinOp(self: *Self, allow: []const Keyword, parseNext: ParseFn) ASTError!EltRef {
        var lhs = try parseNext(self);
        while (!self.eof()) {
            const state = self.state;
            switch (state.tok.?) {
                .keyword => |op| {
                    if (allowed(allow, op)) {
                        try self.advance();
                        const rhs = try parseNext(self);
                        lhs = try self.newNode(
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

    fn parseVar(self: *Self) ASTError!EltRef {
        const state = self.state;
        switch (state.tok.?) {
            .keyword => |kw| {
                if (kw == .@"$") {
                    try self.advance();
                    const ref = try self.parseVar();
                    return try self.newNode(.{ .ref = ref }, state.loc);
                }
                return ASTError.SyntaxError;
            },
            .symbol => |sym| {
                try self.advance();
                return try self.newNode(.{ .symbol = sym }, state.loc);
            },
            else => return ASTError.SyntaxError,
        }
    }

    fn parseCall(self: *Self) ASTError!EltRef {
        const state = self.state;
        var call = try self.parseVar();
        while (self.nextKeywordIs(.@"(")) {
            try self.advance();
            const args = try self.parseList(.@")", true);
            call = try self.newNode(
                .{ .call = .{ .method = call, .args = args } },
                state.loc,
            );
        }
        return call;
    }

    fn parseRef(self: *Self) ASTError!EltRef {
        return self.parseBinOp(&.{.@"."}, parseCall);
    }

    fn parenthesise(self: *const Self, elt: EltRef) ASTError!EltRef {
        return switch (elt.*.node) {
            .assign_stmt => |a| try self.newNode(
                .{ .assign_expr = .{ .lvalue = a.lvalue, .rvalue = a.rvalue } },
                elt.loc,
            ),
            else => elt,
        };
    }

    fn parseSingleQuotedString(self: *Self, str: []const u8) ASTError!EltRef {
        const state = self.state;
        try self.advance();

        var ss = StringScanner{ .str = str };
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.gpa);

        while (!ss.eof()) {
            const nc = ss.next();
            if (nc == '\\' and !ss.eof()) {
                const qc = ss.next();
                if (qc != '\\' and qc != '\'')
                    try buf.append(self.gpa, nc);
                try buf.append(self.gpa, qc);
            } else {
                try buf.append(self.gpa, nc);
            }
        }

        return self.newNode(
            .{ .string = try buf.toOwnedSlice(self.gpa) },
            state.loc,
        );
    }

    fn parseDoubleQuotedLiteral(self: *Self, literal: []const u8) ASTError!EltRef {
        const state = self.state;
        var ss = StringScanner{ .str = literal };
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.gpa);

        while (!ss.eof()) {
            const nc = ss.next();
            if (nc == '\\' and !ss.eof()) {
                const qc = ss.next();
                switch (qc) {
                    '$', '\\', '\"' => try buf.append(self.gpa, qc),
                    'a' => try buf.append(self.gpa, 0x07),
                    'b' => try buf.append(self.gpa, 0x08),
                    't' => try buf.append(self.gpa, 0x09),
                    'n' => try buf.append(self.gpa, 0x0a),
                    'f' => try buf.append(self.gpa, 0x0c),
                    'r' => try buf.append(self.gpa, 0x0d),
                    'e' => try buf.append(self.gpa, 0x1b),
                    'o' => {
                        if (ss.available() < 3) return ASTError.BadString;
                        const cc = try std.fmt.parseInt(u8, ss.take(3), 8);
                        try buf.append(self.gpa, cc);
                    },
                    'x' => {
                        if (ss.available() < 2) return ASTError.BadString;
                        const cc = try std.fmt.parseInt(u8, ss.take(2), 16);
                        try buf.append(self.gpa, cc);
                    },
                    else => return ASTError.BadString,
                }
            } else {
                try buf.append(self.gpa, nc);
            }
        }

        return self.newNode(
            .{ .string = try buf.toOwnedSlice(self.gpa) },
            state.loc,
        );
    }

    fn parseInterpolation(self: *Self, interp: []const u8) ASTError!EltRef {
        var node: ?EltRef = null;
        var pos: usize = 0;
        while (pos < interp.len) : (pos += 1) {
            const start = pos;
            while (pos < interp.len and interp[pos] != '.')
                pos += 1;
            if (pos == start) return ASTError.SyntaxError;
            const rhs = try self.newNode(
                .{ .symbol = interp[start..pos] },
                self.state.loc,
            );
            node = if (node) |lhs|
                try self.newNode(
                    .{ .binary_op = .{ .op = .@".", .lhs = lhs, .rhs = rhs } },
                    self.state.loc, // TODO that's not right
                )
            else
                rhs;
        }
        return node.?;
    }

    fn parseDoubleQuotedString(self: *Self, str: []const u8) ASTError!EltRef {
        const state = self.state;
        try self.advance();
        var node: ?EltRef = null;
        var ss = StringScanner{ .str = str };

        while (ss.nextToken()) |tok| {
            const rhs = switch (tok) {
                .literal => |l| try self.parseDoubleQuotedLiteral(l),
                .interp => |i| try self.parseInterpolation(i),
            };
            node = if (node) |lhs|
                try self.newNode(
                    .{ .binary_op = .{ .op = ._, .lhs = lhs, .rhs = rhs } },
                    state.loc,
                )
            else
                rhs;
        }

        if (node) |n|
            return n;

        // Default empty string
        return self.newNode(
            .{ .string = "" },
            state.loc,
        );
    }

    fn parseAtom(self: *Self) ASTError!EltRef {
        const state = self.state;
        switch (state.tok.?) {
            .keyword => |kw| {
                switch (kw) {
                    .@"-", .NOT => |op| {
                        try self.advance();
                        const arg = try self.parseAtom();
                        return try self.newNode(
                            .{ .unary_op = .{ .op = op, .arg = arg } },
                            state.loc,
                        );
                    },
                    .@"(" => {
                        const res = try self.advanceAndParseExpr();
                        // TODO assign_stmt -> assign_expr
                        if (!self.nextKeywordIs(.@")"))
                            return ASTError.MissingParen;
                        try self.advance();
                        return self.parenthesise(res);
                    },
                    .@"$" => {
                        return try self.parseRef();
                    },
                    .@"[" => {
                        try self.advance();
                        const array = try self.parseList(.@"]", false);
                        return try self.newNode(
                            .{ .array = array },
                            state.loc,
                        );
                    },
                    else => return ASTError.SyntaxError,
                }
            },
            .number => |n| {
                const node = try self.newNode(.{ .number = n }, state.loc);
                try self.advance();
                return node;
            },
            .symbol => {
                return try self.parseRef();
            },
            .sq_string => |s| {
                return try self.parseSingleQuotedString(s);
            },
            .dq_string => |s| {
                return try self.parseDoubleQuotedString(s);
            },
            else => return ASTError.SyntaxError,
        }
    }

    fn parseAssign(self: *Self) ASTError!EltRef {
        const lvalue = try self.parseAtom();
        const state = self.state;
        if (self.nextKeywordIs(.@"=")) {
            const rvalue = try self.advanceAndParseExpr();
            return self.newNode(
                .{ .assign_stmt = .{ .lvalue = lvalue, .rvalue = rvalue } },
                state.loc,
            );
        }
        return lvalue;
    }

    fn parseMulDiv(self: *Self) ASTError!EltRef {
        return try self.parseBinOp(
            &.{ .@"*", .@"/", .DIV, .MOD },
            parseAssign,
        );
    }

    fn parseAddSub(self: *Self) ASTError!EltRef {
        return try self.parseBinOp(
            &.{ .@"+", .@"-", ._ },
            parseMulDiv,
        );
    }

    fn parseInequality(self: *Self) ASTError!EltRef {
        return try self.parseBinOp(
            &.{ .@"<", .@"<=", .@">", .@">=", .@"==", .@"!=" },
            parseAddSub,
        );
    }

    fn parseAnd(self: *Self) ASTError!EltRef {
        return try self.parseBinOp(&.{.AND}, parseInequality);
    }

    fn parseOr(self: *Self) ASTError!EltRef {
        return try self.parseBinOp(&.{.OR}, parseAnd);
    }

    fn parseCond(self: *Self) ASTError!EltRef {
        const cond = try self.parseOr();

        if (!self.nextKeywordIs(.@"?"))
            return cond;

        const state = self.state;

        const THEN = try self.advanceAndParseExpr();

        if (!self.nextKeywordIs(.@":"))
            return ASTError.MissingColon;

        const ELSE = try self.advanceAndParseExpr();

        return self.newNode(
            .{ .if_op = .{ .cond = cond, .THEN = THEN, .ELSE = ELSE } },
            state.loc,
        );
    }

    pub fn parseExpr(self: *Self) ASTError!EltRef {
        return try self.parseCond();
    }

    pub fn advanceAndParseExpr(self: *Self) ASTError!EltRef {
        try self.advance();
        return try self.parseExpr();
    }
};

test "parseExpr" {
    const cases = &[_]struct { src: []const u8, want: []const u8 }{
        .{ .src = "[% 1 %]", .want = "1" },
        .{ .src = "[% -1 %]", .want = "-1" },
        .{ .src = "[% (-(1)) %]", .want = "-1" },
        .{ .src = "[% 1 + 3.5 %]", .want = "(1 + 3.5)" },
        .{ .src = "[% 1 + 3 * 2 %]", .want = "(1 + (3 * 2))" },
        .{ .src = "[% (1 + 3) * 2 %]", .want = "((1 + 3) * 2)" },
        .{ .src = "[% foo %]", .want = "foo" },
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
        .{ .src = "[% \"Hello\x41\" %]", .want = "\"HelloA\"" },
        .{ .src = "[% \"Hello $name\" %]", .want = "(\"Hello \" _ name)" },
        .{ .src = "[% \"Hello $name.first\" %]", .want = "(\"Hello \" _ name.first)" },
        .{ .src = "[% \"$name\" %]", .want = "name" },
        .{ .src = "[% \"$foo$bar$baz\" %]", .want = "((foo _ bar) _ baz)" },
        .{ .src = "[% a ? 0 : 1 %]", .want = "a ? 0 : 1" },
        .{ .src = "[% a ? b ? 1 : 2 : c ? 3 : 4 %]", .want = "a ? b ? 1 : 2 : c ? 3 : 4" },
    };

    for (cases) |case| {
        var alloc = std.heap.ArenaAllocator.init(testing.allocator);
        defer alloc.deinit();
        const gpa = alloc.allocator();

        const iter = TokenIter.init(case.src);
        var parser = try ASTParser.init(gpa, iter);

        try testing.expectEqual(Token{ .start = .{} }, parser.state.tok.?);
        try parser.advance();
        const elt = try parser.parseExpr();
        try testing.expectEqual(Token{ .end = .{} }, parser.state.tok.?);

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
const Allocator = std.mem.Allocator;
const Io = std.Io;

const toker = @import("./tokeniser.zig");
const Token = toker.Token;
const TokenIter = toker.TokenIter;
const Keyword = toker.Keyword;
const Location = toker.Location;

const ASTNode = @import("./node.zig").ASTNode;
const ASTElement = @import("./node.zig").ASTElement;
