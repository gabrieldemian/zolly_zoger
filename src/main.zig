const std = @import("std");
const Io = std.Io;

const zolly_zoger = @import("zolly_zoger");

pub fn main(init: std.process.Init) !void {
    const arena: std.mem.Allocator = init.arena.allocator();
    // In order to do I/O operations need an `Io` instance.
    const io = init.io;
}
