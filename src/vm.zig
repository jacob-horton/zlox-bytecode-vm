const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_common = @import("common.zig");
const zlox_compiler = @import("compiler.zig");
const zlox_debug = @import("debug.zig");
const zlox_object = @import("object.zig");
const zlox_value = @import("value.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const Compiler = zlox_compiler.Compiler;

const Obj = zlox_object.Obj;

const Value = zlox_value.Value;
const ValueType = zlox_value.ValueType;
const OperationError = zlox_value.OperationError;

const STACK_MAX = 256;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

const RuntimeError = error{
    ExpectOperandNumeric,
    ExpectBothOperandsNumeric,
    ExpectBothOperandsString,
    ExpectBothOperandsStringOrBothNumeric,
    ConcatenationError,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_MAX]Value,
    stack_top: usize,

    compiler: Compiler,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VM {
        return VM{
            .allocator = allocator,

            // These will get set up when calling `interpret()` before doing anything
            .chunk = undefined,
            .ip = undefined,

            // We define specific stack slots as we get to them, so undefined is fine
            .stack = undefined,
            .stack_top = 0,
            .compiler = Compiler.init(allocator),
        };
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!(try self.compiler.compile(source, &chunk))) {
            return InterpretResult.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;

        const result = self.run();
        return result;
    }

    fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM, distance: usize) *Value {
        return &self.stack[self.stack_top - 1 - distance];
    }

    fn resetStack(self: *VM) void {
        self.stack_top = 0;
    }

    fn runtimeError(self: *VM, err: RuntimeError) InterpretResult {
        switch (err) {
            RuntimeError.ExpectBothOperandsNumeric => std.debug.print("Operands must be numbers.", .{}),
            RuntimeError.ExpectBothOperandsString => std.debug.print("Operands must be strings.", .{}),
            RuntimeError.ExpectBothOperandsStringOrBothNumeric => std.debug.print("Operands must be both strings or both numbers.", .{}),
            RuntimeError.ExpectOperandNumeric => std.debug.print("Operand must be a number.", .{}),
            RuntimeError.ConcatenationError => std.debug.print("Failed to concatenate", .{}),
        }

        std.debug.print("\n", .{});

        const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr) - 1;
        std.debug.print("[line {d}] in script\n", .{self.chunk.getLine(instruction).?});
        self.resetStack();

        return InterpretResult.RUNTIME_ERROR;
    }

    inline fn readByte(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn binaryOp(self: *VM, op: fn (allocator: std.mem.Allocator, a: Value, b: Value) OperationError!Value) RuntimeError!void {
        const b = self.pop();
        const a = self.pop();
        self.push(op(self.allocator, a, b) catch |err| switch (err) {
            OperationError.ExpectBothNumeric => return RuntimeError.ExpectBothOperandsNumeric,
            OperationError.ExpectBothString => return RuntimeError.ExpectBothOperandsString,
            OperationError.ExpectBothStringOrBothNumeric => return RuntimeError.ExpectBothOperandsStringOrBothNumeric,
            OperationError.ConcatenationError => return RuntimeError.ConcatenationError,
        });
    }

    fn run(self: *VM) InterpretResult {
        while (true) {
            if (zlox_common.DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |slot| {
                    std.debug.print("[ ", .{});
                    self.stack[slot].print();
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});

                const offset = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr);
                _ = zlox_debug.disassembleInstruction(self.chunk, offset);
            }

            const instruction = self.readByte();
            switch (instruction) {
                @intFromEnum(OpCode.CONSTANT) => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                @intFromEnum(OpCode.NIL) => self.push(ValueType.nil),
                @intFromEnum(OpCode.TRUE) => self.push(Value{ .boolean = true }),
                @intFromEnum(OpCode.FALSE) => self.push(Value{ .boolean = false }),
                @intFromEnum(OpCode.EQUAL) => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value{ .boolean = Value.equals(a, b) });
                },
                @intFromEnum(OpCode.GREATER) => self.binaryOp(Value.greater) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.LESS) => self.binaryOp(Value.less) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.ADD) => self.binaryOp(Value.add) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.SUBTRACT) => self.binaryOp(Value.sub) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.MULTIPLY) => self.binaryOp(Value.mul) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.DIVIDE) => self.binaryOp(Value.div) catch |err| return self.runtimeError(err),
                @intFromEnum(OpCode.NOT) => self.push(Value{ .boolean = self.pop().isFalsey() }),
                @intFromEnum(OpCode.NEGATE) => {
                    switch (self.peek(0).*) {
                        .number => |*num| num.* = -num.*,
                        else => return self.runtimeError(RuntimeError.ExpectOperandNumeric),
                    }
                },
                @intFromEnum(OpCode.RETURN) => {
                    self.pop().print();
                    std.debug.print("\n", .{});
                    return InterpretResult.OK;
                },
                else => {
                    std.debug.print("Unknown opcode {d}\n", .{instruction});
                    return InterpretResult.COMPILE_ERROR;
                },
            }
        }
    }
};
