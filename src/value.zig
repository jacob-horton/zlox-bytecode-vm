const std = @import("std");

const zlox_common = @import("common.zig");
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

pub const Value = blk: {
    if (zlox_common.NAN_BOXING) {
        const SIGN_BIT: u64 = 0x8000000000000000;
        const QNAN: u64 = 0x7ffc000000000000;

        const TAG_NIL = 0b01;
        const TAG_FALSE = 0b10;
        const TAG_TRUE = 0b11;

        const NIL_VAL = QNAN | TAG_NIL;
        const TRUE_VAL = QNAN | TAG_TRUE;
        const FALSE_VAL = QNAN | TAG_FALSE;

        break :blk struct {
            value: u64,

            pub fn initBool(boolean: bool) Value {
                return Value{ .value = if (boolean) TRUE_VAL else FALSE_VAL };
            }

            pub fn initNumber(num: f64) Value {
                return Value{ .value = @bitCast(num) };
            }

            pub fn initObject(obj: *Obj) Value {
                // NOTE: this only works if the first 2 bytes (16 bits) are 0s in the ptr (which seems to be the case) i.e. ptrs only use the low 48 bits
                return Value{ .value = SIGN_BIT | QNAN | @intFromPtr(obj) };
            }

            pub fn initNil() Value {
                return Value{ .value = NIL_VAL };
            }

            pub fn asBool(self: Value) bool {
                return self.value == TRUE_VAL;
            }

            pub fn asNumber(self: Value) f64 {
                return @bitCast(self.value);
            }

            pub fn asObject(self: Value) *Obj {
                return @ptrFromInt(self.value & ~(SIGN_BIT | QNAN));
            }

            pub fn isBool(self: Value) bool {
                return self.value == TRUE_VAL or self.value == FALSE_VAL;
            }

            pub fn isNumber(self: Value) bool {
                return (self.value & QNAN) != QNAN;
            }

            pub fn isObj(self: Value) bool {
                return (self.value & (QNAN | SIGN_BIT) == (QNAN | SIGN_BIT));
            }

            pub fn isNil(self: Value) bool {
                return self.value == NIL_VAL;
            }

            pub fn isFalsey(self: Value) bool {
                return self.isNil() or (self.isBool() and !self.asBool());
            }

            pub fn isObjType(self: Value, typ: ObjType) bool {
                return self.isObj() and self.asObject().type == typ;
            }

            pub fn print(self: Value) void {
                if (self.isNumber()) {
                    std.debug.print("{d}", .{self.asNumber()});
                } else if (self.isBool()) {
                    std.debug.print("{s}", .{if (self.asBool()) "true" else "false"});
                } else if (self.isObj()) {
                    self.asObject().print();
                } else if (self.isNil()) {
                    std.debug.print("nil", .{});
                } else unreachable;
            }

            pub fn equals(a: Value, b: Value) bool {
                // This is to account for NaN equality
                if (a.isNumber() and b.isNumber()) {
                    return a.asNumber() == b.asNumber();
                }

                return a.value == b.value;
            }

            fn checkNumeric(a: Value, b: Value) OperationError!void {
                if (!a.isNumber() or !b.isNumber()) {
                    return OperationError.ExpectBothNumeric;
                }
            }

            fn checkString(a: Value, b: Value) OperationError!void {
                if (!a.isObjType(.STRING) or !b.isObjType(.STRING)) {
                    return OperationError.ExpectBothString;
                }
            }

            fn concatenate(vm: *VM, a: *String, b: *String) !Value {
                const len = a.chars.len + b.chars.len;
                const chars = try vm.allocator.alloc(u8, len);
                @memcpy(chars[0..a.chars.len], a.chars);
                @memcpy(chars[a.chars.len..len], b.chars);

                const str = try String.takeInit(vm, chars);
                return Value.initObject(&str.obj);
            }

            pub fn add(vm: *VM, a: Value, b: Value) OperationError!Value {
                // Check if both numeric or both string
                Value.checkNumeric(a, b) catch {
                    Value.checkString(a, b) catch return OperationError.ExpectBothStringOrBothNumeric;
                };

                if (a.isNumber()) {
                    return Value.initNumber(a.asNumber() + b.asNumber());
                }

                if (a.isObjType(.STRING)) {
                    return Value.concatenate(
                        vm,
                        a.asObject().as(String),
                        b.asObject().as(String),
                    ) catch {
                        return OperationError.ConcatenationError;
                    };
                }

                unreachable;
            }

            pub fn sub(_: *VM, a: Value, b: Value) OperationError!Value {
                try Value.checkNumeric(a, b);
                return Value.initNumber(a.asNumber() - b.asNumber());
            }

            pub fn mul(_: *VM, a: Value, b: Value) OperationError!Value {
                try Value.checkNumeric(a, b);
                return Value.initNumber(a.asNumber() * b.asNumber());
            }

            pub fn div(_: *VM, a: Value, b: Value) OperationError!Value {
                try Value.checkNumeric(a, b);
                return Value.initNumber(a.asNumber() / b.asNumber());
            }

            pub fn less(_: *VM, a: Value, b: Value) OperationError!Value {
                try Value.checkNumeric(a, b);
                return Value.initBool(a.asNumber() < b.asNumber());
            }

            pub fn greater(_: *VM, a: Value, b: Value) OperationError!Value {
                try Value.checkNumeric(a, b);
                return Value.initBool(a.asNumber() > b.asNumber());
            }
        };
    }

    // Not using NaN boxing
    break :blk union(enum) {
        boolean: bool,
        number: f64,
        obj: *Obj,
        nil,

        pub fn initBool(boolean: bool) Value {
            return Value{ .boolean = boolean };
        }

        pub fn initNumber(num: f64) Value {
            return Value{ .number = num };
        }

        pub fn initObject(obj: *Obj) Value {
            return Value{ .obj = obj };
        }

        pub fn initNil() Value {
            return .nil;
        }

        pub fn asBool(self: Value) bool {
            return self.boolean;
        }

        pub fn asNumber(self: Value) f64 {
            return self.number;
        }

        pub fn asObject(self: Value) *Obj {
            return self.obj;
        }

        pub fn isBool(self: Value) bool {
            return self == .boolean;
        }

        pub fn isNumber(self: Value) bool {
            return self == .number;
        }

        pub fn isObj(self: Value) bool {
            return self == .obj;
        }

        pub fn isNil(self: Value) bool {
            return self == .nil;
        }

        pub fn isFalsey(self: Value) bool {
            return self.isNil() or (self.isBool() and !self.asBool());
        }

        pub fn isObjType(self: Value, typ: ObjType) bool {
            return self.isObj() and self.asObject().type == typ;
        }

        pub fn print(self: Value) void {
            switch (self) {
                .number => std.debug.print("{d}", .{self.asNumber()}),
                .boolean => std.debug.print("{s}", .{if (self.asBool()) "true" else "false"}),
                .obj => self.asObject().print(),
                .nil => std.debug.print("nil", .{}),
            }
        }

        pub fn equals(a: Value, b: Value) bool {
            if (@as(std.meta.Tag(Value), a) != @as(std.meta.Tag(Value), b)) return false;

            return switch (a) {
                .number => a.asNumber() == b.asNumber(),
                .boolean => a.asBool() == b.asBool(),
                .obj => a.asObject() == b.asObject(),
                .nil => true,
            };
        }

        fn checkNumeric(a: Value, b: Value) OperationError!void {
            if (!a.isNumber() or !b.isNumber()) {
                return OperationError.ExpectBothNumeric;
            }
        }

        fn checkString(a: Value, b: Value) OperationError!void {
            if (!a.isObjType(.STRING) or !b.isObjType(.STRING)) {
                return OperationError.ExpectBothString;
            }
        }

        fn concatenate(vm: *VM, a: *String, b: *String) !Value {
            const len = a.chars.len + b.chars.len;
            const chars = try vm.allocator.alloc(u8, len);
            @memcpy(chars[0..a.chars.len], a.chars);
            @memcpy(chars[a.chars.len..len], b.chars);

            const str = try String.takeInit(vm, chars);
            return Value.initObject(&str.obj);
        }

        pub fn add(vm: *VM, a: Value, b: Value) OperationError!Value {
            // Check if both numeric or both string
            Value.checkNumeric(a, b) catch {
                Value.checkString(a, b) catch return OperationError.ExpectBothStringOrBothNumeric;
            };

            if (a.isNumber()) {
                return Value.initNumber(a.asNumber() + b.asNumber());
            }

            if (a.isObjType(.STRING)) {
                return Value.concatenate(
                    vm,
                    a.asObject().as(String),
                    b.asObject().as(String),
                ) catch {
                    return OperationError.ConcatenationError;
                };
            }

            unreachable;
        }

        pub fn sub(_: *VM, a: Value, b: Value) OperationError!Value {
            try Value.checkNumeric(a, b);
            return Value.initNumber(a.asNumber() - b.asNumber());
        }

        pub fn mul(_: *VM, a: Value, b: Value) OperationError!Value {
            try Value.checkNumeric(a, b);
            return Value.initNumber(a.asNumber() * b.asNumber());
        }

        pub fn div(_: *VM, a: Value, b: Value) OperationError!Value {
            try Value.checkNumeric(a, b);
            return Value.initNumber(a.asNumber() / b.asNumber());
        }

        pub fn less(_: *VM, a: Value, b: Value) OperationError!Value {
            try Value.checkNumeric(a, b);
            return Value.initBool(a.asNumber() < b.asNumber());
        }

        pub fn greater(_: *VM, a: Value, b: Value) OperationError!Value {
            try Value.checkNumeric(a, b);
            return Value.initBool(a.asNumber() > b.asNumber());
        }
    };
};
