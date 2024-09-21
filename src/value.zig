const std = @import("std");

const zlox_object = @import("object.zig");

const Obj = zlox_object.Obj;
const ObjType = zlox_object.ObjType;
const String = zlox_object.Obj.String;

pub const ValueArray = std.ArrayList(Value);

pub const OperationError = error{
    ExpectBothNumeric,
    ExpectBothString,
    ExpectBothStringOrBothNumeric,
    ConcatenationError,
};

pub const ValueType = enum {
    boolean,
    number,
    obj,
    nil,
};

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    obj: *Obj,
    nil: void,

    pub fn isFalsey(self: Value) bool {
        return self == ValueType.nil or (self == ValueType.boolean and !self.boolean);
    }

    pub fn isObjType(self: Value, typ: ObjType) bool {
        return self == ValueType.obj and self.obj.type == typ;
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
        if (@as(ValueType, a) != @as(ValueType, b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .number => a.number == b.number,
            .obj => {
                const aStr = a.obj.asString();
                const bStr = b.obj.asString();

                return std.mem.eql(u8, aStr.chars, bStr.chars);
            },
            .nil => true,
        };
    }

    fn check_numeric(a: Value, b: Value) OperationError!void {
        if (a != ValueType.number or b != ValueType.number) {
            return OperationError.ExpectBothNumeric;
        }
    }

    fn check_string(a: Value, b: Value) OperationError!void {
        if (a != ValueType.obj or
            b != ValueType.obj or
            a.obj.type != ObjType.STRING or
            b.obj.type != ObjType.STRING)
        {
            return OperationError.ExpectBothString;
        }
    }

    fn concatenate(allocator: std.mem.Allocator, a: *String, b: *String) !Value {
        const len = a.chars.len + b.chars.len;
        const chars = try allocator.alloc(u8, len + 1);
        @memcpy(chars[0..a.chars.len], a.chars);
        @memcpy(chars[a.chars.len..len], b.chars);
        chars[len] = 0;

        const str = try String.init(allocator, chars);
        return Value{ .obj = &str.obj };
    }

    pub fn add(allocator: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        // Check if both numeric or both string
        Value.check_numeric(a, b) catch {
            Value.check_string(a, b) catch return OperationError.ExpectBothStringOrBothNumeric;
        };

        switch (a) {
            .number => return Value{ .number = a.number + b.number },
            .obj => return Value.concatenate(
                allocator,
                a.obj.asString(),
                b.obj.asString(),
            ) catch {
                return OperationError.ConcatenationError;
            },
            else => unreachable,
        }
    }

    pub fn sub(_: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number - b.number };
    }

    pub fn mul(_: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number * b.number };
    }

    pub fn div(_: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number / b.number };
    }

    pub fn less(_: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number < b.number };
    }

    pub fn greater(_: std.mem.Allocator, a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number > b.number };
    }
};
