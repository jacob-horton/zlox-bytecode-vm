const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_object = @import("object.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const Function = zlox_object.Obj.Function;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

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
        @intFromEnum(OpCode.CALL) => return byteInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.CLOSURE) => {
            var new_offset = offset + 1;
            const constant = chunk.code.items[new_offset];
            new_offset += 1;

            const name = @tagName(@as(OpCode, @enumFromInt(instruction)));
            std.debug.print("{s:<16} {d:4} ", .{ name, constant });
            chunk.constants.items[constant].print();
            std.debug.print("\n", .{});

            const function = chunk.constants.items[constant].obj.as(Function);
            for (0..function.upvalue_count) |_| {
                const is_local = chunk.code.items[new_offset] > 0;
                new_offset += 1;

                const index = chunk.code.items[new_offset];
                new_offset += 1;

                std.debug.print("{d:4}      |                     {s} {d}\n", .{ new_offset - 2, if (is_local) "local" else "upvalue", index });
            }

            return new_offset;
        },
        @intFromEnum(OpCode.CLOSE_UPVALUE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.RETURN) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.NIL) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.TRUE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.FALSE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.NEGATE) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.PRINT) => return simpleInstruction(instruction, offset),
        @intFromEnum(OpCode.JUMP) => return jumpInstruction(instruction, 1, chunk, offset),
        @intFromEnum(OpCode.JUMP_IF_FALSE) => return jumpInstruction(instruction, 1, chunk, offset),
        @intFromEnum(OpCode.LOOP) => return jumpInstruction(instruction, -1, chunk, offset),
        @intFromEnum(OpCode.SET_UPVALUE) => return byteInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.GET_UPVALUE) => return byteInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.SET_LOCAL) => return byteInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.GET_LOCAL) => return byteInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.SET_PROPERTY) => return constantInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.GET_PROPERTY) => return constantInstruction(instruction, chunk, offset),
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
        @intFromEnum(OpCode.CLASS) => return constantInstruction(instruction, chunk, offset),
        @intFromEnum(OpCode.METHOD) => return constantInstruction(instruction, chunk, offset),
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
    var constant: isize = -1;
    if (offset + 1 >= chunk.code.items.len) constant = chunk.code.items[offset + 1];

    std.debug.print("{s:<16} {d:4} '", .{ getInstructionName(instruction), constant });

    if (constant >= 0) chunk.constants.items[@intCast(constant)].print();
    std.debug.print("'\n", .{});

    return offset + 2;
}

fn byteInstruction(instruction: u8, chunk: *Chunk, offset: usize) usize {
    var slot: isize = -1;
    if (offset + 1 >= chunk.code.items.len) slot = chunk.code.items[offset + 1];

    std.debug.print("{s:<16} {d:4}\n", .{ getInstructionName(instruction), slot });

    return offset + 2;
}

fn jumpInstruction(instruction: u8, sign: i2, chunk: *Chunk, offset: usize) usize {
    var jump = @as(i17, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];

    // Add 3 for 1 byte jump + 2 byte operand
    const jump_signed = sign * jump;
    const jump_location = @as(i65, offset) + 3 + jump_signed; // NOTE: this only makes sense for 64 bit architectures - any more will fail
    std.debug.print("{s:<16} {d:4} -> {d}\n", .{ getInstructionName(instruction), offset, jump_location });

    return offset + 3;
}

fn simpleInstruction(instruction: u8, offset: usize) usize {
    std.debug.print("{s}\n", .{getInstructionName(instruction)});
    return offset + 1;
}
