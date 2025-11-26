pub fn isSymbol(chr: u8) bool {
    return std.ascii.isAlphanumeric(chr) or chr == '_';
}

pub fn isBareword(chr: u8) bool {
    return isSymbol(chr) or chr == '$' or chr == '.';
}

const std = @import("std");
