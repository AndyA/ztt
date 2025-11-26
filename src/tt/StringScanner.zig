const SS = @This();
str: []const u8,
pos: usize = 0,

const StringToken = union(enum) {
    literal: []const u8,
    interp: []const u8,
};

const SymbolToken = union(enum) {
    name: []const u8,
    index: i64,
    keyword: types.Keyword,
};

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

pub fn nextSymbolToken(self: *SS) ?SymbolToken {
    if (self.eof()) return null;
    const start = self.pos;
    const nc = self.next();
    return switch (nc) {
        '.' => .{ .keyword = .@"." },
        '$' => .{ .keyword = .@"$" },
        '0'...'9' => blk: {
            while (!self.eof() and std.ascii.isDigit(self.peek()))
                _ = self.next();
            const index = try std.fmt.parseInt(i64, self.str[start..self.pos]);
            break :blk .{ .index = index };
        },
        'a'...'z', 'A'...'Z', '_' => blk: {
            while (!self.eof() and ctype.isSymbol(self.peek()))
                _ = self.next();
            break :blk .{ .name = self.str[start..self.pos] };
        },
        else => unreachable,
    };
}

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;

const types = @import("./types.zig");
const ctype = @import("./ctype.zig");
