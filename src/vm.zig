const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_value = @import("value.zig");
const zlox_compiler = @import("kompiler.zig");
const zlox_common = @import("common.zig");
const zlox_debug = @import("debug.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const Value = zlox_value.Value;

const Compiler = zlox_compiler.Compiler;

const STACK_MAX = 256;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

fn add(a: Value, b: Value) Value {
    return a + b;
}

fn subtract(a: Value, b: Value) Value {
    return a - b;
}

fn multiply(a: Value, b: Value) Value {
    return a * b;
}

fn divide(a: Value, b: Value) Value {
    return a / b;
}

pub const VM = struct {
    chunk: ?*Chunk,
    // TODO: make this a pointer instead of an offset
    ip: usize,
    stack: [STACK_MAX]Value,
    stack_top: usize,
    compiler: Compiler,

    pub fn init() VM {
        return VM{
            .chunk = null,
            .ip = 0,
            .stack = undefined,
            .stack_top = 0,
            .compiler = Compiler.init(),
        };
    }

    pub fn interpret(self: *VM, allocator: std.mem.Allocator, source: []u8) !InterpretResult {
        var chunk = Chunk.init(allocator);
        defer chunk.deinit();

        if (!(try self.compiler.compile(source, &chunk))) {
            return InterpretResult.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        const result = self.run();
        return result;
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn peek(self: *VM) *Value {
        return &self.stack[self.stack_top - 1];
    }

    pub fn resetStack(self: *VM) void {
        self.stack_top = 0;
    }

    inline fn readByte(self: *VM) u8 {
        const byte = self.chunk.?.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    inline fn readConstant(self: *VM) f64 {
        return self.chunk.?.constants.items[self.readByte()];
    }

    fn binaryOp(self: *VM, op: fn (a: Value, b: Value) Value) void {
        const b = self.pop();
        const a = self.pop();
        self.push(op(a, b));
    }

    pub fn run(self: *VM) InterpretResult {
        while (true) {
            if (zlox_common.DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |slot| {
                    std.debug.print("[ ", .{});
                    zlox_value.printValue(self.stack[slot]);
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});

                _ = zlox_debug.dissassembleInstruction(self.chunk.?, self.ip);
            }

            const instruction = self.readByte();
            switch (instruction) {
                @intFromEnum(OpCode.CONSTANT) => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                @intFromEnum(OpCode.ADD) => self.binaryOp(add),
                @intFromEnum(OpCode.SUBTRACT) => self.binaryOp(subtract),
                @intFromEnum(OpCode.MULTIPLY) => self.binaryOp(multiply),
                @intFromEnum(OpCode.DIVIDE) => self.binaryOp(divide),
                @intFromEnum(OpCode.NEGATE) => self.peek().* = -self.peek().*,
                @intFromEnum(OpCode.RETURN) => {
                    zlox_value.printValue(self.pop());
                    std.debug.print("\n", .{});
                    return InterpretResult.OK;
                },
                else => {
                    std.debug.print("Unknown opcode {}\n", .{instruction});
                    return InterpretResult.COMPILE_ERROR;
                },
            }
        }
    }
};
