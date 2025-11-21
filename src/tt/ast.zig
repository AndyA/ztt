pub const ASTNode = union(enum) {
    const Node = @This();

    literal: []const u8,
    string: []const u8,
    number: f64,

    block: []*const Node, // top level and block bodies

    op1: struct {
        op: Keyword,
        arg: *const ASTNode,
    },

    op2: struct {
        op: Keyword,
        lhs: *const ASTNode,
        rhs: *const ASTNode,
    },

    IF: struct {
        cond: *const Node, // expr
        THEN: *const Node, // block
        ELSE: *const Node, // block
    },

    pub fn format(self: Node, w: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .number => |n| try w.print("{d}", .{n}),
            .op1 => |o| try w.print("{s}{f}", .{ @tagName(o.op), o.arg }),
            .op2 => |o| try w.print("({f} {s} {f})", .{ o.lhs, @tagName(o.op), o.rhs }),
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

    fn parseAtom(self: *Self) ParserError!*const ASTNode {
        const current = self.current.?.tok;
        switch (current) {
            .keyword => |kw| {
                switch (kw) {
                    .@"-" => |op| {
                        try self.advance();
                        const arg = try self.parseAtom();
                        return try self.newNode(.{ .op1 = .{ .op = op, .arg = arg } });
                    },
                    .@"(" => {
                        try self.advance();
                        const res = try self.parseExpr();
                        if (!self.nextKeywordIs(.@")"))
                            return error.MissingParen;
                        try self.advance();
                        return res;
                    },
                    else => return error.SyntaxError,
                }
            },
            .number => |n| {
                const node = try self.newNode(.{ .number = n });
                try self.advance();
                return node;
            },
            else => return error.SyntaxError,
        }
    }

    const ParseFn = fn (*Self) ParserError!*const ASTNode;

    fn allowed(comptime allow: []const Keyword, kw: Keyword) bool {
        for (allow) |k| if (k == kw) return true;
        return false;
    }

    fn parseBinOp(
        self: *Self,
        comptime allow: []const Keyword,
        comptime parseUp: ParseFn,
    ) ParserError!*const ASTNode {
        var lhs = try parseUp(self);
        while (!self.eof) {
            switch (self.current.?.tok) {
                .keyword => |kw| {
                    if (allowed(allow, kw)) {
                        try self.advance();
                        const rhs = try parseUp(self);
                        lhs = try self.newNode(.{ .op2 = .{
                            .op = kw,
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

    fn parseMultiply(self: *Self) ParserError!*const ASTNode {
        return try self.parseBinOp(&.{ .@"*", .@"/", .DIV, .MOD }, parseAtom);
    }

    fn parseAdd(self: *Self) ParserError!*const ASTNode {
        return try self.parseBinOp(&.{ .@"+", .@"-" }, parseMultiply);
    }

    pub fn parseExpr(self: *Self) ParserError!*const ASTNode {
        return try self.parseAdd();
    }
};

test ASTParser {
    var alloc = std.heap.ArenaAllocator.init(testing.allocator);
    defer alloc.deinit();
    const gpa = alloc.allocator();
    var iter = toker.LocationTokenIter.init("test", "[% -3 + 7 * 3 * (1 + 9) %]");
    _ = try iter.next();
    var parser = try ASTParser.init(gpa, iter);
    const node = try parser.parseExpr();
    std.debug.print("node: {f}\n", .{node});
    // _ = node;
}

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const toker = @import("./tokeniser.zig");
const Keyword = toker.Keyword;
