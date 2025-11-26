const Self = @This();
ss: StringScanner,

const StringToken = union(enum) {
    literal: []const u8,
    interp: []const u8,
};

fn isInterp(chr: u8) bool {
    return std.ascii.isAlphanumeric(chr) or chr == '_' or chr == '.';
}

pub fn next(self: *Self) ?StringToken {
    if (self.ss.eof()) return null;
    const nc = self.ss.next();
    if (nc == '$') {
        const start = self.ss.pos;
        while (!self.ss.eof() and isInterp(self.ss.peek()))
            _ = self.ss.next();

        return if (self.ss.pos == start)
            .{ .literal = "$" }
        else
            .{ .interp = self.ss.str[start..self.ss.pos] };
    } else {
        const start = self.ss.pos - 1;
        while (!self.ss.eof() and self.ss.peek() != '$') {
            if (self.ss.next() == '\\' and !self.ss.eof())
                _ = self.ss.next();
        }
        return .{ .literal = self.ss.str[start..self.ss.pos] };
    }
}

const std = @import("std");
const StringScanner = @import("./StringScanner.zig");
