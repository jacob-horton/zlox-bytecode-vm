const std = @import("std");

const zlox_object = @import("object.zig");
const zlox_vm = @import("vm.zig");

const Obj = zlox_object.Obj;
const ObjType = zlox_object.ObjType;
const String = zlox_object.Obj.String;

const VM = zlox_vm.VM;

pub const ValueArray = std.ArrayList(Value);

pub const OperationError = error{
    ExpectBothNumeric,
    ExpectBothString,
    ExpectBothStringOrBothNumeric,
    ConcatenationError,
};

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    obj: *Obj,
    nil,

    pub fn isFalsey(self: Value) bool {
        return self == .nil or (self == .boolean and !self.boolean);
    }

    pub fn isObjType(self: Value, typ: ObjType) bool {
        return self == .obj and self.obj.type == typ;
    }

    pub fn print(self: Value) void {
        switch (self) {
            .number => |n| std.debug.print("{d}", .{n}),
            .boolean => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
            .obj => |o| o.print(),
            .nil => std.debug.print("nil", .{}),
        }
    }

    pub fn equals(a: Value, b: Value) bool {
        if (@as(std.meta.Tag(Value), a) != @as(std.meta.Tag(Value), b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .number => a.number == b.number,
            // Pointer comparison because strings are interned
            .obj => a.obj == b.obj,
            .nil => true,
        };
    }

    fn check_numeric(a: Value, b: Value) OperationError!void {
        if (a != .number or b != .number) {
            return OperationError.ExpectBothNumeric;
        }
    }

    fn check_string(a: Value, b: Value) OperationError!void {
        if (a != .obj or
            b != .obj or
            a.obj.type != .STRING or
            b.obj.type != .STRING)
        {
            return OperationError.ExpectBothString;
        }
    }

    fn concatenate(vm: *VM, a: *String, b: *String) !Value {
        const len = a.chars.len + b.chars.len;
        const chars = try vm.allocator.alloc(u8, len);
        @memcpy(chars[0..a.chars.len], a.chars);
        @memcpy(chars[a.chars.len..len], b.chars);

        const str = try String.takeInit(vm, chars);
        return Value{ .obj = &str.obj };
    }

    pub fn add(vm: *VM, a: Value, b: Value) OperationError!Value {
        // Check if both numeric or both string
        Value.check_numeric(a, b) catch {
            Value.check_string(a, b) catch return OperationError.ExpectBothStringOrBothNumeric;
        };

        switch (a) {
            .number => return Value{ .number = a.number + b.number },
            .obj => return Value.concatenate(
                vm,
                a.obj.asString(),
                b.obj.asString(),
            ) catch {
                return OperationError.ConcatenationError;
            },
            else => unreachable,
        }
    }

    pub fn sub(_: *VM, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number - b.number };
    }

    pub fn mul(_: *VM, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number * b.number };
    }

    pub fn div(_: *VM, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number / b.number };
    }

    pub fn less(_: *VM, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number < b.number };
    }

    pub fn greater(_: *VM, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number > b.number };
    }
};
