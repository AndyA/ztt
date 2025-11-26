pub const ASTNode = union(enum) {
    const Self = @This();
    const Keyword = types.Keyword;
    const EltRef = *const ASTElement;

    const Assign = struct {
        lvalue: EltRef,
        rvalue: EltRef,
    };

    const Cond = struct {
        cond: EltRef, // expr
        THEN: EltRef, // block
        ELSE: EltRef, // block
    };

    literal: []const u8,
    string: []const u8,
    float: f64,
    int: i64,
    array: []EltRef,
    object: struct {
        keys: []EltRef,
        values: []EltRef,
    },

    block: []EltRef, // top level and block bodies

    symbol: []const u8,
    ref: EltRef,
    call: struct { method: EltRef, args: []EltRef },
    assign_stmt: Assign,
    assign_expr: Assign,

    unary_op: struct { op: Keyword, arg: EltRef },
    binary_op: struct { op: Keyword, lhs: EltRef, rhs: EltRef },
    if_op: Cond,

    IF: Cond,

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
                        else => try w.print("\\x{x:0>2}", .{c}),
                    }
                },
                else => try w.print("{c}", .{c}),
            }
        }
    }

    fn formatList(w: *Io.Writer, list: []EltRef) Io.Writer.Error!void {
        for (list, 0..) |item, index| {
            try w.print("{f}", .{item});
            if (index < list.len - 1) try w.print(", ", .{});
        }
    }

    fn formatObject(w: *Io.Writer, keys: []EltRef, values: []EltRef) Io.Writer.Error!void {
        assert(keys.len == values.len);
        for (keys, values, 0..) |k, v, index| {
            try w.print("{f} => {f}", .{ k, v });
            if (index < keys.len - 1) try w.print(", ", .{});
        }
    }

    pub fn format(self: Self, w: *Io.Writer) Io.Writer.Error!void {
        switch (self) {
            inline .float, .int => |n| try w.print("{d}", .{n}),
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
            .object => |o| {
                try w.print("{{", .{});
                try formatObject(w, o.keys, o.values);
                try w.print("}}", .{});
            },
            .string => |s| {
                // TODO escape
                try w.print("\"", .{});
                try formatString(w, s);
                try w.print("\"", .{});
            },
            .if_op => |i| {
                try w.print("{f} ? {f} : {f}", .{ i.cond, i.THEN, i.ELSE });
            },
            else => unreachable,
        }
    }
};

pub const ASTElement = struct {
    const Self = @This();
    const Location = types.Location;

    node: ASTNode,
    loc: Location,

    pub fn create(gpa: Allocator, node: ASTNode, loc: Location) Allocator.Error!*Self {
        const self = try gpa.create(Self);
        self.* = .{ .node = node, .loc = loc };
        return self;
    }

    pub fn format(self: Self, w: *Io.Writer) Io.Writer.Error!void {
        try self.node.format(w);
    }
};

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Io = std.Io;

const types = @import("./types.zig");
