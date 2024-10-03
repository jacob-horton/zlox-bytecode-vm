const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_common = @import("common.zig");
const zlox_compiler = @import("compiler.zig");
const zlox_debug = @import("debug.zig");
const zlox_gc = @import("gc.zig");
const zlox_native = @import("native.zig");
const zlox_object = @import("object.zig");
const zlox_table = @import("table.zig");
const zlox_value = @import("value.zig");

const OpCode = zlox_chunk.OpCode;

const Compiler = zlox_compiler.Compiler;

const GC = zlox_gc.GC;

const Obj = zlox_object.Obj;
const String = zlox_object.Obj.String;
const Closure = zlox_object.Obj.Closure;
const Function = zlox_object.Obj.Function;
const Native = zlox_object.Obj.Native;
const NativeFn = zlox_object.NativeFn;
const Upvalue = zlox_object.Obj.Upvalue;
const Class = zlox_object.Obj.Class;
const Instance = zlox_object.Obj.Instance;
const BoundMethod = zlox_object.Obj.BoundMethod;

const Table = zlox_table.Table;

const Value = zlox_value.Value;
const OperationError = zlox_value.OperationError;

// TODO: handle stack overflow if enough function calls use enough temporaries in addition to locals
const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * zlox_common.U8_COUNT;

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

const CallFrame = struct {
    closure: *Closure,
    ip: [*]u8,
    slots: [*]Value,

    fn readByte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    /// Reads 2 bytes as a u16
    fn readShort(self: *CallFrame) u16 {
        self.ip += 2;
        return (@as(u16, (self.ip - 2)[0]) << 8) | (self.ip - 1)[0];
    }

    fn readConstant(self: *CallFrame) Value {
        return self.closure.function.chunk.constants.items[self.readByte()];
    }

    fn readString(self: *CallFrame) *String {
        return self.readConstant().obj.as(String);
    }
};

pub const VM = struct {
    stack: [STACK_MAX]Value,
    stack_top: usize,

    frames: [FRAMES_MAX]CallFrame,
    frame_count: usize,

    strings: Table,
    open_upvalues: ?*Upvalue,
    globals: Table,

    gc: GC,
    allocator: std.mem.Allocator,
    objects: ?*Obj,

    // Needed for GC
    compiler: ?*Compiler,

    init_string: ?*String,

    pub fn init() VM {
        return VM{
            // These are defined in `setupAllocator`, which must be called after `init`
            .gc = undefined,
            .allocator = undefined,
            .globals = undefined,
            .strings = undefined,
            .init_string = null,

            .objects = null,
            .compiler = null,

            // This will get set up when calling `interpret()` before doing anything
            .frames = undefined,
            .frame_count = 0,

            // We define specific stack slots as we get to them, so undefined is fine
            .stack = undefined,
            .stack_top = 0,

            .open_upvalues = null,
        };
    }

    pub fn setupAllocator(self: *VM, backing_allocator: std.mem.Allocator) !void {
        self.gc = GC.init(self, backing_allocator);

        self.allocator = self.gc.allocator();
        self.strings = Table.init(self.allocator);
        self.globals = Table.init(self.allocator);
        self.init_string = try String.copyInit(self, "init", 4);

        try self.defineNative("clock", &zlox_native.clock);
    }

    pub fn deinit(self: *VM) void {
        self.strings.deinit();
        self.globals.deinit();
        self.init_string = null;
        if (self.objects) |objects| objects.deinitAll();

        self.gc.deinit();
    }

    pub fn interpret(self: *VM, source: []u8) !InterpretResult {
        var compiler = try Compiler.init(self, null, .SCRIPT);
        self.compiler = &compiler;

        const result = compiler.compile(source) catch null;

        if (result) |function| {
            self.push(Value{ .obj = &function.obj });
            const closure = try Closure.init(self, function);
            _ = self.pop();
            self.push(Value{ .obj = &closure.obj });

            _ = self.call(closure, 0);

            return try self.run();
        } else {
            return .COMPILE_ERROR;
        }
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
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

        var i = self.frame_count;
        while (i > 0) {
            i -= 1;

            const frame = &self.frames[i];
            const function = frame.closure.function;
            const instruction = zlox_common.ptrOffset(u8, function.chunk.code.items.ptr, frame.ip) - 1;

            std.debug.print("[line {d}] in ", .{function.chunk.getLine(instruction).?});
            if (function.name) |name| {
                std.debug.print("{s}()\n", .{name.chars});
            } else {
                std.debug.print("script\n", .{});
            }
        }

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

    fn binaryOp(self: *VM, op: fn (vm: *VM, a: Value, b: Value) OperationError!Value) RuntimeError!void {
        // Peek so we don't take off stack for gc (only needed for concat)
        const b = self.peek(0);
        const a = self.peek(1);

        const new_value = op(self, a.*, b.*) catch |err| switch (err) {
            OperationError.ExpectBothNumeric => return RuntimeError.ExpectBothOperandsNumeric,
            OperationError.ExpectBothString => return RuntimeError.ExpectBothOperandsString,
            OperationError.ExpectBothStringOrBothNumeric => return RuntimeError.ExpectBothOperandsStringOrBothNumeric,
            OperationError.ConcatenationError => return RuntimeError.ConcatenationError,
        };

        _ = self.pop();
        _ = self.pop();

        self.push(new_value);
    }

    fn defineNative(self: *VM, name: []const u8, function: NativeFn) !void {
        const str = try String.copyInit(self, name.ptr, name.len);
        self.push(Value{ .obj = &str.obj });
        const native = try Native.init(self, function);
        self.push(Value{ .obj = &native.obj });

        _ = try self.globals.set(self.stack[0].obj.as(String), self.stack[1]);

        _ = self.pop();
        _ = self.pop();
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !bool {
        switch (callee) {
            .obj => |obj| {
                switch (obj.type) {
                    .CLASS => {
                        const class = obj.as(Class);
                        const instance = try Instance.init(self, class);
                        self.stack[self.stack_top - arg_count - 1] = Value{ .obj = &instance.obj };

                        var initialiser: Value = undefined;
                        if (class.methods.get(self.init_string.?, &initialiser)) {
                            return self.call(initialiser.obj.as(Closure), arg_count);
                        } else if (arg_count != 0) {
                            _ = self.runtimeError("Expected 0 arguments but got {d}", .{arg_count});
                            return false;
                        }

                        return true;
                    },
                    .CLOSURE => return self.call(obj.as(Closure), arg_count),
                    .BOUND_METHOD => {
                        const bound = obj.as(BoundMethod);
                        self.stack[self.stack_top - arg_count - 1] = bound.receiver;
                        return self.call(bound.method, arg_count);
                    },
                    .NATIVE => {
                        const native = obj.as(Native);
                        const result = native.function(arg_count, @as([*]Value, &self.stack) + self.stack_top - arg_count);
                        self.stack_top -= arg_count + 1;
                        self.push(result);

                        return true;
                    },
                    else => {}, // Non-callable object type
                }
            },
            else => {}, // Non-callable value
        }

        _ = self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn call(self: *VM, closure: *Closure, arg_count: u8) bool {
        if (arg_count != closure.function.arity) {
            _ = self.runtimeError("Expected {d} arguments, but got {d}.", .{ closure.function.arity, arg_count });
            return false;
        }

        if (self.frame_count >= FRAMES_MAX) {
            _ = self.runtimeError("Stack overflow.", .{});
            return false;
        }

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = closure.function.chunk.code.items.ptr;
        frame.slots = @as([*]Value, &self.stack) + self.stack_top - arg_count - 1;
        return true;
    }

    fn captureUpvalue(self: *VM, local: *Value) !*Upvalue {
        var prev_upvalue: ?*Upvalue = null;
        var upvalue = self.open_upvalues;

        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue) |up| {
            if (up.location == local) {
                return up;
            }
        }

        const created_upvalue = try Upvalue.init(self, local);
        created_upvalue.next = upvalue;

        if (prev_upvalue) |up| {
            up.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            const upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn defineMethod(self: *VM, name: *String) !void {
        const method = self.peek(0).*;
        const class = self.peek(1).obj.as(Class);
        _ = try class.methods.set(name, method);
        _ = self.pop();
    }

    fn bindMethod(self: *VM, class: *Class, name: *String) !bool {
        var method: Value = undefined;

        if (!class.methods.get(name, &method)) {
            _ = self.runtimeError("Undefined property '{s}'", .{name.chars});
            return false;
        }

        const bound = try BoundMethod.init(self, self.peek(0).*, method.obj.as(Closure));

        _ = self.pop();
        self.push(Value{ .obj = &bound.obj });
        return true;
    }

    fn invoke(self: *VM, name: *String, arg_count: u8) !bool {
        const receiver = self.peek(arg_count).*;

        if (receiver != .obj or receiver.obj.type != .INSTANCE) {
            _ = self.runtimeError("Only instances have methods", .{});
            return false;
        }

        const instance = receiver.obj.as(Instance);

        var value: Value = undefined;
        if (instance.fields.get(name, &value)) {
            self.stack[self.stack_top - arg_count - 1] = value;
            return self.callValue(value, arg_count);
        }

        return try self.invokeFromClass(instance.class, name, arg_count);
    }

    fn invokeFromClass(self: *VM, class: *Class, name: *String, arg_count: u8) !bool {
        var method: Value = undefined;
        if (!class.methods.get(name, &method)) {
            _ = self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        }

        return self.call(method.obj.as(Closure), arg_count);
    }

    fn run(self: *VM) !InterpretResult {
        var frame = &self.frames[self.frame_count - 1];
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

                const offset = zlox_common.ptrOffset(u8, frame.closure.function.chunk.code.items.ptr, frame.ip);
                _ = zlox_debug.disassembleInstruction(&frame.closure.function.chunk, offset, prev_offset);
                prev_offset = offset;
            }

            const instruction = frame.readByte();
            switch (instruction) {
                @intFromEnum(OpCode.CONSTANT) => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                @intFromEnum(OpCode.NIL) => self.push(.nil),
                @intFromEnum(OpCode.TRUE) => self.push(Value{ .boolean = true }),
                @intFromEnum(OpCode.FALSE) => self.push(Value{ .boolean = false }),
                @intFromEnum(OpCode.POP) => _ = self.pop(),
                @intFromEnum(OpCode.SET_UPVALUE) => {
                    const slot = frame.readByte();
                    frame.closure.upvalues[slot].?.location.* = self.peek(0).*;
                },
                @intFromEnum(OpCode.GET_UPVALUE) => {
                    const slot = frame.readByte();
                    self.push(frame.closure.upvalues[slot].?.location.*);
                },
                @intFromEnum(OpCode.SET_LOCAL) => {
                    const slot = frame.readByte();
                    frame.slots[slot] = self.peek(0).*;
                },
                @intFromEnum(OpCode.GET_LOCAL) => {
                    const slot = frame.readByte();
                    self.push(frame.slots[slot]);
                },
                @intFromEnum(OpCode.SET_GLOBAL) => {
                    const name = frame.readString();
                    if (try self.globals.set(name, self.peek(0).*)) {
                        _ = self.globals.delete(name);
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                @intFromEnum(OpCode.GET_GLOBAL) => {
                    const name = frame.readString();
                    var value: Value = undefined;
                    if (!self.globals.get(name, &value)) {
                        return self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    }

                    self.push(value);
                },
                @intFromEnum(OpCode.DEFINE_GLOBAL) => {
                    const name = frame.readString();
                    _ = try self.globals.set(name, self.peek(0).*);
                    _ = self.pop();
                },
                @intFromEnum(OpCode.GET_PROPERTY) => {
                    const instance_val = self.peek(0);
                    if (instance_val.* != .obj or instance_val.obj.type != .INSTANCE) {
                        return self.runtimeError("Only instances have properties", .{});
                    }

                    const instance = instance_val.obj.as(Instance);
                    const name = frame.readString();

                    // NOTE: fields always shadow methods
                    var value: Value = undefined;
                    if (instance.fields.get(name, &value)) {
                        _ = self.pop(); // Pop instance
                        self.push(value);
                    } else {
                        if (!try self.bindMethod(instance.class, name)) {
                            return self.runtimeError("Undefined property '{s}'", .{name.chars});
                        }
                    }
                },
                @intFromEnum(OpCode.SET_PROPERTY) => {
                    const instance_val = self.peek(1);
                    if (instance_val.* != .obj or instance_val.obj.type != .INSTANCE) {
                        return self.runtimeError("Only instances have fields", .{});
                    }

                    const instance = instance_val.obj.as(Instance);
                    _ = try instance.fields.set(frame.readString(), self.peek(0).*);

                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
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
                @intFromEnum(OpCode.JUMP) => {
                    const offset = frame.readShort();
                    frame.ip += offset;
                },
                @intFromEnum(OpCode.JUMP_IF_FALSE) => {
                    const offset = frame.readShort();
                    if (self.peek(0).isFalsey()) frame.ip += offset;
                },
                @intFromEnum(OpCode.LOOP) => {
                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                @intFromEnum(OpCode.CALL) => {
                    const arg_count = frame.readByte();
                    if (!try self.callValue(self.peek(arg_count).*, arg_count)) {
                        return .RUNTIME_ERROR;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                @intFromEnum(OpCode.CLOSURE) => {
                    const function = frame.readConstant().obj.as(Function);
                    const closure = try Closure.init(self, function);
                    self.push(Value{ .obj = &closure.obj });

                    for (0..closure.upvalue_count) |i| {
                        const is_local = frame.readByte() > 0;
                        const index = frame.readByte();

                        if (is_local) {
                            closure.upvalues[0] = try self.captureUpvalue(&(frame.slots + index)[0]);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },
                @intFromEnum(OpCode.CLOSE_UPVALUE) => {
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                @intFromEnum(OpCode.RETURN) => {
                    const result = self.pop();
                    self.closeUpvalues(&frame.slots[0]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return .OK;
                    }

                    self.stack_top = zlox_common.ptrOffset(Value, &self.stack, frame.slots);
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                @intFromEnum(OpCode.CLASS) => {
                    const class = try Class.init(self, frame.readString());
                    self.push(Value{ .obj = &class.obj });
                },
                @intFromEnum(OpCode.METHOD) => try self.defineMethod(frame.readString()),
                else => {
                    std.debug.print("Unknown opcode {d}\n", .{instruction});
                    return .COMPILE_ERROR;
                },
                @intFromEnum(OpCode.INVOKE) => {
                    const method = frame.readString();
                    const arg_count = frame.readByte();
                    if (!try self.invoke(method, arg_count)) {
                        return .RUNTIME_ERROR;
                    }

                    frame = &self.frames[self.frame_count - 1];
                },
                @intFromEnum(OpCode.SUPER_INVOKE) => {
                    const method = frame.readString();
                    const arg_count = frame.readByte();
                    const superclass = self.pop().obj.as(Class);
                    if (!try self.invokeFromClass(superclass, method, arg_count)) {
                        return .RUNTIME_ERROR;
                    }

                    frame = &self.frames[self.frame_count - 1];
                },
                @intFromEnum(OpCode.INHERIT) => {
                    const superclass_val = self.peek(1).*;
                    if (superclass_val != .obj or superclass_val.obj.type != .CLASS) {
                        return self.runtimeError("Superclass must be a class.", .{});
                    }

                    const superclass = superclass_val.obj.as(Class);
                    const subclass = self.peek(0).obj.as(Class);

                    try subclass.methods.addAll(&superclass.methods);
                    _ = self.pop(); // Subclass
                },
                @intFromEnum(OpCode.GET_SUPER) => {
                    const name = frame.readString();
                    const superclass = self.pop().obj.as(Class);

                    if (!try self.bindMethod(superclass, name)) {
                        return .RUNTIME_ERROR;
                    }
                },
            }
        }
    }
};
