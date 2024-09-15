const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;

pub fn dissassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.data.items.len) {
        offset = dissassembleInstruction(chunk, offset);
    }
}

fn dissassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:4} ", .{offset});
    if (offset > 0 and (chunk.getLine(offset) == chunk.getLine(offset - 1))) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.getLine(offset).?});
    }

    const instruction = chunk.data.items[offset];
    switch (instruction) {
        @intFromEnum(OpCode.OP_RETURN) => return simpleInstruction("OP_RETURN", offset),
        @intFromEnum(OpCode.OP_CONSTANT) => return constantInstruction("OP_CONSTANT", chunk, offset),
        else => {
            std.debug.print("Unknown opcode {}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.data.items[offset + 1];
    std.debug.print("{s:<16} {d:4} '", .{ name, constant });
    printValue(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
