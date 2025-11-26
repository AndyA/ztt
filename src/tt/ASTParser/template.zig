const State = enum { LITERAL, BLOCK };

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

pub fn parseBlock(p: *ASTParser) Error!EltRef {
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
                const node = try parseBlock(p);
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
        .{ .block = try list.toOwnedSlice(p.gpa) },
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
        // std.debug.print(">> {any}\n", .{elt});
        // std.debug.print("++ {s} ++\n", .{case.want});
        // std.debug.print("-- {s} --\n", .{output.items});
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
