const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const const1 = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(OpCode.CONSTANT), 123);
    try chunk.write(const1, 123);

    const const2 = try chunk.addConstant(3.4);
    try chunk.write(@intFromEnum(OpCode.CONSTANT), 123);
    try chunk.write(const2, 123);

    try chunk.write(@intFromEnum(OpCode.ADD), 123);

    const const3 = try chunk.addConstant(5.6);
    try chunk.write(@intFromEnum(OpCode.CONSTANT), 123);
    try chunk.write(const3, 123);

    try chunk.write(@intFromEnum(OpCode.DIVIDE), 123);
    try chunk.write(@intFromEnum(OpCode.NEGATE), 123);

    try chunk.write(@intFromEnum(OpCode.RETURN), 123);

    var vm = VM.init();
    _ = vm.interpret(&chunk);
    // debug.dissassembleChunk(&chunk, "test chunk");
}
