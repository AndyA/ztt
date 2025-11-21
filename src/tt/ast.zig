pub const ASTNode = union(enum) {
    const Node = @This();

    literal: []const u8,
    string: []const u8,
    number: f64,

    block: []*const Node, // top level and block bodies

    symbol: []const u8,
    ref: *const Node,
    call: struct { method: *const Node, args: []*const Node },

    op1: struct { op: Keyword, arg: *const Node },
    op2: struct { op: Keyword, lhs: *const Node, rhs: *const Node },

    IF: struct {
        cond: *const Node, // expr
        THEN: *const Node, // block
        ELSE: *const Node, // block
    },

    pub fn format(self: Node, w: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .number => |n| try w.print("{d}", .{n}),
            .symbol => |s| try w.print("{s}", .{s}),
            .ref => |r| try w.print("${f}", .{r}),
            .op1 => |o| try w.print("{s}{f}", .{ @tagName(o.op), o.arg }),
            .op2 => |o| {
                switch (o.op) {
                    .@"." => try w.print("{f}.{f}", .{ o.lhs, o.rhs }),
                    else => try w.print("({f} {s} {f})", .{ o.lhs, @tagName(o.op), o.rhs }),
                }
            },
            else => unreachable,
        }
    }
};

const ParserError = toker.TokerError || error{
    OutOfMemory,
    MissingParen,
};

pub const ASTParser = struct {
    const Self = @This();

    gpa: Allocator,
    iter: toker.LocationTokenIter,
    current: ?toker.LocationToken = null,
    eof: bool = false,

    pub fn init(gpa: Allocator, iter: toker.LocationTokenIter) ParserError!Self {
        var self = Self{ .gpa = gpa, .iter = iter };
        try self.advance();
        return self;
    }

    fn advance(self: *Self) ParserError!void {
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

    const ParseFn = fn (*Self) ParserError!*const ASTNode;

    fn allowed(allow: []const Keyword, kw: Keyword) bool {
        for (allow) |k| if (k == kw) return true;
        return false;
    }

    fn parseBinOp(self: *Self, allow: []const Keyword, parseUp: ParseFn) ParserError!*const ASTNode {
        var lhs = try parseUp(self);
        while (!self.eof) {
            switch (self.current.?.tok) {
                .keyword => |op| {
                    if (allowed(allow, op)) {
                        try self.advance();
                        const rhs = try parseUp(self);
                        lhs = try self.newNode(.{ .op2 = .{
                            .op = op,
                            .lhs = lhs,
                            .rhs = rhs,
                        } });
                    } else {
                        break;
                    }
                },
                else => break,
            }
        }
        return lhs;
    }

    fn parseVar(self: *Self) ParserError!*const ASTNode {
        switch (self.current.?.tok) {
            .keyword => |kw| {
                if (kw == .@"$") {
                    try self.advance();
                    const ref = try self.parseVar();
                    return try self.newNode(.{ .ref = ref });
                }
                return ParserError.SyntaxError;
            },
            .symbol => |sym| {
                try self.advance();
                return try self.newNode(.{ .symbol = sym });
            },
            else => return ParserError.SyntaxError,
        }
    }

    fn parseCall(self: *Self) ParserError!*const ASTNode {
        // TODO
        return try self.parseVar();
    }

    fn parseRef(self: *Self) ParserError!*const ASTNode {
        return self.parseBinOp(&.{.@"."}, parseCall);
    }

    fn parseAtom(self: *Self) ParserError!*const ASTNode {
        switch (self.current.?.tok) {
            .keyword => |kw| {
                switch (kw) {
                    .@"-", .NOT => |op| {
                        try self.advance();
                        const arg = try self.parseAtom();
                        return try self.newNode(.{ .op1 = .{ .op = op, .arg = arg } });
                    },
                    .@"(" => {
                        try self.advance();
                        const res = try self.parseExpr();
                        if (!self.nextKeywordIs(.@")"))
                            return ParserError.MissingParen;
                        try self.advance();
                        return res;
                    },
                    .@"$" => {
                        return try self.parseRef();
                    },
                    else => return ParserError.SyntaxError,
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
            else => return ParserError.SyntaxError,
        }
    }

    fn parseMulDiv(self: *Self) ParserError!*const ASTNode {
        return try self.parseBinOp(&.{ .@"*", .@"/", .DIV, .MOD }, parseAtom);
    }

    fn parseAddSub(self: *Self) ParserError!*const ASTNode {
        return try self.parseBinOp(&.{ .@"+", .@"-" }, parseMulDiv);
    }

    pub fn parseExpr(self: *Self) ParserError!*const ASTNode {
        return try self.parseAddSub();
    }
};

test "parseExpr" {
    const cases = &[_]struct { src: []const u8, want: []const u8 }{
        .{ .src = "[% 1 %]", .want = "1" },
        .{ .src = "[% 1 + 3 %]", .want = "(1 + 3)" },
        .{ .src = "[% 1 + 3 * 2 %]", .want = "(1 + (3 * 2))" },
        .{ .src = "[% (1 + 3) * 2 %]", .want = "((1 + 3) * 2)" },
        .{ .src = "[% foo %]", .want = "foo" },
        .{ .src = "[% foo.bar %]", .want = "foo.bar" },
        .{ .src = "[% $foo.bar %]", .want = "$foo.bar" },
        .{ .src = "[% foo.$bar %]", .want = "foo.$bar" },
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
        try testing.expect(std.mem.eql(u8, case.want, output.items));
    }
}

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const toker = @import("./tokeniser.zig");
const Keyword = toker.Keyword;
