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

    assign_stmt: struct { lvalue: NodeRef, rvalue: NodeRef },
    assign_expr: struct { lvalue: NodeRef, rvalue: NodeRef },

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

    fn formatString(w: *Io.Writer, str: []const u8) Io.Writer.Error!void {
        for (str) |c| {
            switch (c) {
                0x00...0x1f, 0x7f, '\\', '$', '"' => {
                    switch (c) {
                        0x07 => try w.print("\\a", .{}),
                        0x08 => try w.print("\\b", .{}),
                        0x09 => try w.print("\\t", .{}),
                        0x0a => try w.print("\\n", .{}),
                        0x0c => try w.print("\\f", .{}),
                        0x0d => try w.print("\\r", .{}),
                        0x1b => try w.print("\\e", .{}),
                        '$', '\\', '"' => try w.print("\\{c}", .{c}),
                        else => try w.print("{x:0>2}", .{c}),
                    }
                },
                else => try w.print("{c}", .{c}),
            }
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
            .assign_stmt => |a| {
                try w.print("{f} = {f}", .{ a.lvalue, a.rvalue });
            },
            .assign_expr => |a| {
                try w.print("({f} = {f})", .{ a.lvalue, a.rvalue });
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
            .string => |s| {
                // TODO escape
                try w.print("\"", .{});
                try formatString(w, s);
                try w.print("\"", .{});
            },
            else => unreachable,
        }
    }

    pub fn create(gpa: Allocator, node: Node) Allocator.Error!*Node {
        const self = try gpa.create(ASTNode);
        self.* = node;
        return self;
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;

const toker = @import("./tokeniser.zig");
const Keyword = toker.Keyword;
