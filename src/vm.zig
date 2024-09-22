const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_common = @import("common.zig");
const zlox_compiler = @import("compiler.zig");
const zlox_debug = @import("debug.zig");
const zlox_object = @import("object.zig");
const zlox_table = @import("table.zig");
const zlox_value = @import("value.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const Compiler = zlox_compiler.Compiler;

const Obj = zlox_object.Obj;
const String = zlox_object.Obj.String;

const Table = zlox_table.Table;

const Value = zlox_value.Value;
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

    strings: Table,
    globals: Table,

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

            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.strings.deinit();
        self.globals.deinit();
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!(try Compiler.compile(self, source, &chunk))) {
            return .COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;

        const result = try self.run();
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

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) InterpretResult {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        const instruction = zlox_common.ptrOffset(u8, self.chunk.code.items.ptr, self.ip);
        std.debug.print("[line {d}] in script\n", .{self.chunk.getLine(instruction).?});
        self.resetStack();

        return .RUNTIME_ERROR;
    }

    fn runtimeErrorFromErr(self: *VM, err: RuntimeError) InterpretResult {
        const message = switch (err) {
            RuntimeError.ExpectBothOperandsNumeric => "Operands must be numbers.",
            RuntimeError.ExpectBothOperandsString => "Operands must be strings.",
            RuntimeError.ExpectBothOperandsStringOrBothNumeric => "Operands must be both strings or both numbers.",
            RuntimeError.ExpectOperandNumeric => "Operand must be a number.",
            RuntimeError.ConcatenationError => "Failed to concatenate",
        };

        return self.runtimeError("{s}", .{message});
    }

    inline fn readByte(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn readString(self: *VM) *String {
        return self.readConstant().obj.asString();
    }

    fn binaryOp(self: *VM, op: fn (vm: *VM, a: Value, b: Value) OperationError!Value) RuntimeError!void {
        const b = self.pop();
        const a = self.pop();
        self.push(op(self, a, b) catch |err| switch (err) {
            OperationError.ExpectBothNumeric => return RuntimeError.ExpectBothOperandsNumeric,
            OperationError.ExpectBothString => return RuntimeError.ExpectBothOperandsString,
            OperationError.ExpectBothStringOrBothNumeric => return RuntimeError.ExpectBothOperandsStringOrBothNumeric,
            OperationError.ConcatenationError => return RuntimeError.ConcatenationError,
        });
    }

    fn run(self: *VM) !InterpretResult {
        var prev_offset: usize = 0;

        while (true) {
            if (zlox_common.DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |slot| {
                    std.debug.print("[ ", .{});
                    self.stack[slot].print();
                    std.debug.print(" ]", .{});
                }

                std.debug.print("\n", .{});

                const offset = zlox_common.ptrOffset(u8, self.chunk.code.items.ptr, self.ip);
                _ = zlox_debug.disassembleInstruction(self.chunk, offset, prev_offset);
                prev_offset = offset;
            }

            const instruction = self.readByte();
            switch (instruction) {
                @intFromEnum(OpCode.CONSTANT) => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                @intFromEnum(OpCode.NIL) => self.push(.nil),
                @intFromEnum(OpCode.TRUE) => self.push(Value{ .boolean = true }),
                @intFromEnum(OpCode.FALSE) => self.push(Value{ .boolean = false }),
                @intFromEnum(OpCode.POP) => _ = self.pop(),
                @intFromEnum(OpCode.SET_GLOBAL) => {
                    const name = self.readString();
                    if (try self.globals.set(name, self.peek(0).*)) {
                        _ = self.globals.delete(name);
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                @intFromEnum(OpCode.GET_GLOBAL) => {
                    const name = self.readString();
                    var value: Value = undefined;
                    if (!self.globals.get(name, &value)) {
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    }

                    self.push(value);
                },
                @intFromEnum(OpCode.DEFINE_GLOBAL) => {
                    const name = self.readString();
                    _ = try self.globals.set(name, self.peek(0).*);
                    _ = self.pop();
                },
                @intFromEnum(OpCode.EQUAL) => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value{ .boolean = Value.equals(a, b) });
                },
                @intFromEnum(OpCode.GREATER) => self.binaryOp(Value.greater) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.LESS) => self.binaryOp(Value.less) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.ADD) => self.binaryOp(Value.add) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.SUBTRACT) => self.binaryOp(Value.sub) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.MULTIPLY) => self.binaryOp(Value.mul) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.DIVIDE) => self.binaryOp(Value.div) catch |err| return self.runtimeErrorFromErr(err),
                @intFromEnum(OpCode.NOT) => self.push(Value{ .boolean = self.pop().isFalsey() }),
                @intFromEnum(OpCode.NEGATE) => {
                    switch (self.peek(0).*) {
                        .number => |*num| num.* = -num.*,
                        else => return self.runtimeErrorFromErr(RuntimeError.ExpectOperandNumeric),
                    }
                },
                @intFromEnum(OpCode.PRINT) => {
                    self.pop().print();
                    std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.RETURN) => {
                    // Exit interpreter
                    return .OK;
                },
                else => {
                    std.debug.print("Unknown opcode {d}\n", .{instruction});
                    return .COMPILE_ERROR;
                },
            }
        }
    }
};
