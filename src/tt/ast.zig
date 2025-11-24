const NodeRef = *const ASTNode;
pub const ASTNode = union(enum) {
    const Node = @This();

    literal: []const u8,
    string: []const u8,
    number: f64,
    array: []NodeRef,

    block: []NodeRef, // top level and block bodies

    symbol: []const u8,
    ref: NodeRef,
    call: struct { method: NodeRef, args: []NodeRef },

    assign_stmt: struct { lvalue: NodeRef, expr: NodeRef },
    assign_expr: struct { lvalue: NodeRef, expr: NodeRef },

    unary_op: struct { op: Keyword, arg: NodeRef },
    binary_op: struct { op: Keyword, lhs: NodeRef, rhs: NodeRef },

    IF: struct {
        cond: NodeRef, // expr
        THEN: NodeRef, // block
        ELSE: NodeRef, // block
    },

    fn formatList(w: *Io.Writer, list: []NodeRef) Io.Writer.Error!void {
        for (list, 0..) |item, index| {
            try w.print("{f}", .{item});
            if (index < list.len - 1) try w.print(", ", .{});
        }
    }

    pub fn format(self: Node, w: *Io.Writer) Io.Writer.Error!void {
        switch (self) {
            .number => |n| try w.print("{d}", .{n}),
            .symbol => |s| try w.print("{s}", .{s}),
            .ref => |r| try w.print("${f}", .{r}),
            .unary_op => |o| switch (o.op) {
                .NOT => try w.print("{s} {f}", .{ @tagName(o.op), o.arg }),
                else => try w.print("{s}{f}", .{ @tagName(o.op), o.arg }),
            },
            .binary_op => |o| {
                switch (o.op) {
                    .@"." => try w.print("{f}{s}{f}", .{ o.lhs, @tagName(o.op), o.rhs }),
                    else => try w.print("({f} {s} {f})", .{ o.lhs, @tagName(o.op), o.rhs }),
                }
            },
            .call => |c| {
                try w.print("{f}(", .{c.method});
                try formatList(w, c.args);
                try w.print(")", .{});
            },
            .array => |a| {
                try w.print("[", .{});
                try formatList(w, a);
                try w.print("]", .{});
            },
            else => unreachable,
        }
    }
};

const ASTError = toker.TokerError || error{
    OutOfMemory,
    MissingParen,
    MissingTerminal,
    MissingComma,
};

pub const ASTParser = struct {
    const Self = @This();

    gpa: Allocator,
    iter: toker.LocationTokenIter,
    current: ?toker.LocationToken = null,
    eof: bool = false,

    pub fn init(gpa: Allocator, iter: toker.LocationTokenIter) ASTError!Self {
        var self = Self{ .gpa = gpa, .iter = iter };
        try self.advance();
        return self;
    }

    fn advance(self: *Self) ASTError!void {
        if (self.eof) return error.UnexpectedEOF;
        self.current = try self.iter.next();
        if (self.current == null) self.eof = true;
    }

    fn newNode(self: *Self, proto: ASTNode) !*ASTNode {
        const node = try self.gpa.create(ASTNode);
        node.* = proto;
        return node;
    }

    fn nextKeywordIs(self: *Self, want: Keyword) bool {
        if (self.current) |current| {
            return switch (current.tok) {
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

    fn parseList(self: *Self, end: Keyword, require_commas: bool) ASTError![]NodeRef {
        var list: std.ArrayListUnmanaged(NodeRef) = .empty;
        while (true) {
            if (self.eof)
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

    const ParseFn = fn (*Self) ASTError!NodeRef;

    fn parseBinOp(self: *Self, allow: []const Keyword, parseNext: ParseFn) ASTError!NodeRef {
        var lhs = try parseNext(self);
        while (!self.eof) {
            switch (self.current.?.tok) {
                .keyword => |op| {
                    if (allowed(allow, op)) {
                        try self.advance();
                        const rhs = try parseNext(self);
                        lhs = try self.newNode(
                            .{ .binary_op = .{ .op = op, .lhs = lhs, .rhs = rhs } },
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

    fn parseVar(self: *Self) ASTError!NodeRef {
        switch (self.current.?.tok) {
            .keyword => |kw| {
                if (kw == .@"$") {
                    try self.advance();
                    const ref = try self.parseVar();
                    return try self.newNode(.{ .ref = ref });
                }
                return ASTError.SyntaxError;
            },
            .symbol => |sym| {
                try self.advance();
                return try self.newNode(.{ .symbol = sym });
            },
            else => return ASTError.SyntaxError,
        }
    }

    fn parseCall(self: *Self) ASTError!NodeRef {
        var call = try self.parseVar();
        while (self.nextKeywordIs(.@"(")) {
            try self.advance();
            const args = try self.parseList(.@")", true);
            call = try self.newNode(
                .{ .call = .{ .method = call, .args = args } },
            );
        }
        return call;
    }

    fn parseRef(self: *Self) ASTError!NodeRef {
        return self.parseBinOp(&.{.@"."}, parseCall);
    }

    fn parseAtom(self: *Self) ASTError!NodeRef {
        switch (self.current.?.tok) {
            .keyword => |kw| {
                switch (kw) {
                    .@"-", .NOT => |op| {
                        try self.advance();
                        const arg = try self.parseAtom();
                        return try self.newNode(
                            .{ .unary_op = .{ .op = op, .arg = arg } },
                        );
                    },
                    .@"(" => {
                        try self.advance();
                        const res = try self.parseExpr();
                        // TODO assign_stmt -> assign_expr
                        if (!self.nextKeywordIs(.@")"))
                            return ASTError.MissingParen;
                        try self.advance();
                        return res;
                    },
                    .@"$" => {
                        return try self.parseRef();
                    },
                    .@"[" => {
                        try self.advance();
                        const array = try self.parseList(.@"]", false);
                        return try self.newNode(.{ .array = array });
                    },
                    else => return ASTError.SyntaxError,
                }
            },
            .number => |n| {
                const node = try self.newNode(.{ .number = n });
                try self.advance();
                return node;
            },
            .symbol => {
                return try self.parseRef();
            },
            else => return ASTError.SyntaxError,
        }
    }

    fn parseAssign(self: *Self) ASTError!NodeRef {
        // TODO parse assignment
        return try self.parseAtom();
    }

    fn parseMulDiv(self: *Self) ASTError!NodeRef {
        return try self.parseBinOp(
            &.{ .@"*", .@"/", .DIV, .MOD },
            parseAssign,
        );
    }

    fn parseAddSub(self: *Self) ASTError!NodeRef {
        return try self.parseBinOp(
            &.{ .@"+", .@"-", ._ },
            parseMulDiv,
        );
    }

    fn parseInequality(self: *Self) ASTError!NodeRef {
        return try self.parseBinOp(
            &.{ .@"<", .@"<=", .@">", .@">=", .@"==", .@"!=" },
            parseAddSub,
        );
    }

    fn parseAnd(self: *Self) ASTError!NodeRef {
        return try self.parseBinOp(&.{.AND}, parseInequality);
    }

    fn parseOr(self: *Self) ASTError!NodeRef {
        return try self.parseBinOp(&.{.OR}, parseAnd);
    }

    pub fn parseExpr(self: *Self) ASTError!NodeRef {
        return try self.parseOr();
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
    };

    for (cases) |case| {
        var alloc = std.heap.ArenaAllocator.init(testing.allocator);
        defer alloc.deinit();
        const gpa = alloc.allocator();

        const iter = toker.LocationTokenIter.init("test", case.src);
        var parser = try ASTParser.init(gpa, iter);

        try testing.expectEqual(toker.Token{ .start = .{} }, parser.current.?.tok);
        try parser.advance();
        const node = try parser.parseExpr();
        try testing.expectEqual(toker.Token{ .end = .{} }, parser.current.?.tok);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        var w = std.Io.Writer.Allocating.fromArrayList(gpa, &buf);
        defer w.deinit();
        try w.writer.print("{f}", .{node});
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
const Keyword = toker.Keyword;
