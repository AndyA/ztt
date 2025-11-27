fn swallowStart(str: []const u8) []const u8 {
    var pos: usize = 0;
    while (pos < str.len and std.ascii.isWhitespace(str[pos]))
        pos += 1;
    return str[pos..];
}

fn swallowEnd(str: []const u8) []const u8 {
    var pos = str.len;
    while (pos > 0 and std.ascii.isWhitespace(str[pos - 1]))
        pos -= 1;
    return str[0..pos];
}

fn swallowWhite(str: []const u8, start: bool, end: bool) []const u8 {
    var s = str;
    if (start) s = swallowStart(s);
    if (end) s = swallowEnd(s);
    return s;
}

fn isSwallow(p: *const ASTParser) bool {
    if (p.eof()) return false;
    return switch (p.state.tok.?) {
        inline .start, .end => |tok| tok.swallow,
        else => false,
    };
}

fn isBlockEnd(p: *const ASTParser) bool {
    if (p.eof()) return false;
    return switch (p.state.tok.?) {
        .keyword => |kw| switch (kw) {
            .END, .ELSE, .ELSIF => true,
            else => false,
        },
        else => false,
    };
}

fn parseCompound(p: *ASTParser) Error!EltRef {
    var list: std.ArrayListUnmanaged(EltRef) = .empty;
    const start_state = p.state;

    while (true) {
        if (p.eof())
            return Error.UnexpectedEOF;
        if (p.state.tok.? != .end)
            return Error.SyntaxError;
        var swallow_start = isSwallow(p);
        try p.advance();
        while (!p.eof() and p.state.tok.? == .literal) {
            const state = p.state;
            try p.advance();
            const lit = swallowWhite(state.tok.?.literal, swallow_start, isSwallow(p));
            if (lit.len > 0) {
                const node = try p.newNode(
                    .{ .literal = lit },
                    state.loc,
                );
                try list.append(p.gpa, node);
            }
            swallow_start = false;
        }
        if (p.eof())
            return Error.UnexpectedEOF;
        if (p.state.tok.? != .start)
            return Error.SyntaxError;
        try p.advance();
        if (isBlockEnd(p))
            break;
        if (p.eof())
            return Error.UnexpectedEOF;
        try list.append(p.gpa, try parseStatement(p));
    }

    return try p.newNode(
        .{ .compound = try list.toOwnedSlice(p.gpa) },
        start_state.loc,
    );
}

fn wrapInCompound(p: *ASTParser, elt: EltRef) Error!EltRef {
    var list = try p.gpa.alloc(EltRef, 1);
    list[0] = elt;
    return try p.newNode(
        .{ .compound = list },
        elt.loc,
    );
}

fn parseIFBody(p: *ASTParser) Error!EltRef {
    const state = p.state;
    try p.advance();
    var cond = try expr.parseExpr(p);
    if (state.tok.?.keyword == .UNLESS)
        cond = try p.newNode(
            .{ .unary_op = .{ .op = .NOT, .arg = cond } },
            state.loc,
        );
    const THEN = try parseCompound(p);
    switch (p.state.tok.?.keyword) {
        .END => return try p.newNode(
            .{ .IF = .{ .cond = cond, .THEN = THEN } },
            state.loc,
        ),
        .ELSE => {
            try p.advance();
            const ELSE = try parseCompound(p);
            return try p.newNode(
                .{ .IF = .{ .cond = cond, .THEN = THEN, .ELSE = ELSE } },
                state.loc,
            );
        },
        .ELSIF => {
            const ELSE = try wrapInCompound(p, try parseIFBody(p));
            return try p.newNode(
                .{ .IF = .{ .cond = cond, .THEN = THEN, .ELSE = ELSE } },
                state.loc,
            );
        },
        else => return Error.SyntaxError,
    }
}

pub fn parseIF(p: *ASTParser) Error!EltRef {
    const IF = try parseIFBody(p);
    if (p.state.tok.?.keyword != .END)
        return Error.SyntaxError;
    try p.advance();
    return IF;
}

fn parseStatement(p: *ASTParser) Error!EltRef {
    switch (p.state.tok.?) {
        .keyword => |kw| switch (kw) {
            .IF, .UNLESS => return try parseIF(p),
            else => {},
        },
        else => {},
    }

    return try expr.parseExpr(p);
}

pub fn parseTemplate(p: *ASTParser) Error!EltRef {
    var list: std.ArrayListUnmanaged(EltRef) = .empty;
    const start_state = p.state;
    var swallow_start = false;

    while (!p.eof()) {
        const state = p.state;
        switch (state.tok.?) {
            .literal => |lit| {
                try p.advance();
                const node = try p.newNode(
                    .{ .literal = swallowWhite(lit, swallow_start, isSwallow(p)) },
                    state.loc,
                );
                try list.append(p.gpa, node);
                swallow_start = false;
            },
            .start => {
                try p.advance();
                const node = try parseStatement(p);
                if (p.eof() or p.state.tok.? != .end)
                    return Error.SyntaxError;
                swallow_start = isSwallow(p);
                try p.advance();
                try list.append(p.gpa, node);
            },
            else => {
                return Error.SyntaxError;
            },
        }
    }

    return try p.newNode(
        .{ .compound = try list.toOwnedSlice(p.gpa) },
        start_state.loc,
    );
}

test "template" {
    const cases = &[_]struct { src: []const u8, want: []const u8 }{
        .{ .src = 
        \\Hello
        , .want = 
        \\"Hello";
        \\
        },
        .{ .src = 
        \\Hello [% a = 1 %]
        , .want = 
        \\"Hello ";
        \\a = 1;
        \\
        },
        .{ .src = 
        \\[% "Hello "; a = 1 %]
        , .want = 
        \\"Hello ";
        \\a = 1;
        \\
        },
        .{ .src = 
        \\Hello [%- a = 1 -%] World
        , .want = 
        \\"Hello";
        \\a = 1;
        \\"World";
        \\
        },
        .{ .src = 
        \\[% IF a; "Hello"; END %]
        , .want = 
        \\IF a;
        \\    "Hello";
        \\END;
        \\
        },
        .{ .src = 
        \\[% UNLESS a %]Hello[% END %]
        , .want = 
        \\IF NOT a;
        \\    "Hello";
        \\END;
        \\
        },
        .{ .src = 
        \\[% IF (n = name); "Hello $n"; END %]
        , .want = 
        \\IF (n = name);
        \\    ("Hello " _ n);
        \\END;
        \\
        },
        .{ .src = 
        \\[% IF (n = name) -%]
        \\  [%- "Hello $n" -%]
        \\[%- END %]
        , .want = 
        \\IF (n = name);
        \\    ("Hello " _ n);
        \\END;
        \\
        },
        .{ .src = 
        \\[% IF name -%]
        \\  [%- "Hello $name" -%]
        \\[%- ELSE -%]
        \\  [%- "Who?" -%]
        \\[%- END %]
        , .want = 
        \\IF name;
        \\    ("Hello " _ name);
        \\ELSE;
        \\    "Who?";
        \\END;
        \\
        },
        .{ .src = 
        \\[% IF name -%]
        \\  [%- "Hello $name" -%]
        \\[%- ELSIF title -%]
        \\  [%- "Hi $title" -%]
        \\[%- END %]
        , .want = 
        \\IF name;
        \\    ("Hello " _ name);
        \\ELSE;
        \\    IF title;
        \\        ("Hi " _ title);
        \\    END;
        \\END;
        \\
        },
        .{ .src = 
        \\[% IF name -%]
        \\  [%- "Hello $name" -%]
        \\[%- ELSIF title -%]
        \\  [%- "Hi $title" -%]
        \\[%- ELSE -%]
        \\  [%- "Who?" -%]
        \\[%- END %]
        , .want = 
        \\IF name;
        \\    ("Hello " _ name);
        \\ELSE;
        \\    IF title;
        \\        ("Hi " _ title);
        \\    ELSE;
        \\        "Who?";
        \\    END;
        \\END;
        \\
        },
    };

    for (cases) |case| {
        var alloc = std.heap.ArenaAllocator.init(testing.allocator);
        defer alloc.deinit();
        const gpa = alloc.allocator();

        const iter = TokenIter.init(case.src);
        var parser = try ASTParser.init(gpa, iter);

        const elt = try parseTemplate(&parser);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        var w = std.Io.Writer.Allocating.fromArrayList(gpa, &buf);
        defer w.deinit();
        try w.writer.print("{f}", .{elt});
        var output = w.toArrayList();
        defer output.deinit(gpa);
        // std.debug.print("{f}\n", .{std.json.fmt(elt, .{})});
        // std.debug.print("got:\n<{s}>\n\n", .{output.items});
        // std.debug.print("want:\n<{s}>\n\n", .{case.want});
        try testing.expectEqualDeep(case.want, output.items);
    }
}

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const types = @import("../types.zig");
const Keyword = types.Keyword;

const TokenIter = @import("../TokenIter.zig");

const ASTNode = @import("../node.zig").ASTNode;
const ASTElement = @import("../node.zig").ASTElement;

const StringScanner = @import("../StringScanner.zig");
const StringIter = @import("../StringIter.zig");

const ASTParser = @import("../ASTParser.zig");
const Error = ASTParser.Error;
const EltRef = ASTParser.EltRef;
const expr = @import("./expr.zig");
