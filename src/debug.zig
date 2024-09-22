const std = @import("std");

const zlox_chunk = @import("chunk.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

pub fn dissassembleChunk(chunk: *Chunk) void {
    var offset: usize = 0;
    var prev_offset: usize = 0;
    while (offset < chunk.code.items.len) {
        const new_offset = disassembleInstruction(chunk, offset, prev_offset);
        prev_offset = offset;
        offset = new_offset;
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize, prev_offset: usize) usize {
    std.debug.print("{d:4} ", .{offset});
    if (offset > 0 and (chunk.getLine(offset) == chunk.getLine(prev_offset))) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{?d:4} ", .{chunk.getLine(offset)});
    }

    const instruction = chunk.code.items[offset];
    switch (instruction) {
        @intFromEnum(OpCode.RETURN) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.NIL) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.TRUE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.FALSE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.NEGATE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.PRINT) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.SET_GLOBAL) => return constantInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.GET_GLOBAL) => return constantInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.DEFINE_GLOBAL) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.POP) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.EQUAL) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.GREATER) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.LESS) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.ADD) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.SUBTRACT) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.MULTIPLY) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.DIVIDE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.NOT) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.CONSTANT) => return constantInstruction(instruction, chunk, offset),
        else => {
            std.debug.print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn getInstructionName(instruction: u8) []const u8 {
    return @tagName(@as(OpCode, @enumFromInt(instruction)));
}

fn constantInstruction(instruction: u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:4} '", .{ getInstructionName(instruction), constant });

    chunk.constants.items[constant].print();
    std.debug.print("'\n", .{});

    return offset + 2;
}

fn simpleInstruction(instruction: u8, offset: usize) usize {
    std.debug.print("{s}\n", .{getInstructionName(instruction)});
    return offset + 1;
}
