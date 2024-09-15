const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT), 123);
    try chunk.write(constant, 123);
    try chunk.write(@intFromEnum(OpCode.OP_RETURN), 123);
    debug.dissassembleChunk(&chunk, "test chunk");
}
