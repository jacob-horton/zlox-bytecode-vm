const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_value = @import("value.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

pub fn dissassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = dissassembleInstruction(chunk, offset);
    }
}

pub fn dissassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:4} ", .{offset});
    if (offset > 0 and (chunk.getLine(offset) == chunk.getLine(offset - 1))) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.getLine(offset).?});
    }

    const instruction = chunk.code.items[offset];
    switch (instruction) {
        @intFromEnum(OpCode.RETURN) => return simpleInstruction(@tagName(OpCode.RETURN), offset),
        @intFromEnum(OpCode.NEGATE) => return simpleInstruction(@tagName(OpCode.NEGATE), offset),
        @intFromEnum(OpCode.ADD) => return simpleInstruction(@tagName(OpCode.ADD), offset),
        @intFromEnum(OpCode.SUBTRACT) => return simpleInstruction(@tagName(OpCode.SUBTRACT), offset),
        @intFromEnum(OpCode.MULTIPLY) => return simpleInstruction(@tagName(OpCode.MULTIPLY), offset),
        @intFromEnum(OpCode.DIVIDE) => return simpleInstruction(@tagName(OpCode.DIVIDE), offset),
        @intFromEnum(OpCode.CONSTANT) => return constantInstruction(@tagName(OpCode.CONSTANT), chunk, offset),
        else => {
            std.debug.print("Unknown opcode {}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:4} '", .{ name, constant });
    zlox_value.printValue(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
