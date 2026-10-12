const std = @import("std");
const print = std.debug.print;
const Io = std.Io;

const zolly_zoger = @import("zolly_zoger");

pub fn main(init: std.process.Init) !void {
    const arena: std.mem.Allocator = init.arena.allocator();
    _ = arena;
    const io = init.io;
    _ = io;

    zolly_zoger.bencode.RECURSION_LIMIT = 3;
    print("RECURSION_LIMIT {d}\n", .{zolly_zoger.bencode.RECURSION_LIMIT});
}
