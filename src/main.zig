pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();
    var args = std.process.args();
    _ = args.skip();

    while (args.next()) |arg| {
        std.debug.print("Loading {s}\n", .{arg});
        const src = try std.fs.cwd().readFileAlloc(arg, gpa, .unlimited);
        defer gpa.free(src);
        std.debug.print("Loaded 0x{x} bytes\n", .{src.len});
    }
}

const std = @import("std");
const assert = std.debug.assert;

test {
    _ = @import("./tt/TokenIterator.zig");
    _ = @import("./tt/TokenIter.zig");
    _ = @import("./tt/ASTParser.zig");
}
