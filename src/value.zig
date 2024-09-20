const std = @import("std");

pub const ValueArray = std.ArrayList(Value);

pub const ValueType = enum {
    boolean,
    number,
    nil,
};

pub const OperationError = error{
    OperandsNotNumeric,
};

pub const Value = union(ValueType) {
    boolean: bool,
    number: f64,
    nil: void,

    pub fn isFalsey(self: Value) bool {
        return self == ValueType.nil or (self == ValueType.boolean and !self.boolean);
    }

    pub fn print(self: Value) void {
        switch (self) {
            .number => |n| std.debug.print("{d}", .{n}),
            .boolean => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
        }
    }

    pub fn equals(a: Value, b: Value) bool {
        if (@as(ValueType, a) != @as(ValueType, b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .number => a.number == b.number,
            .nil => true,
        };
    }

    fn check_numeric(a: Value, b: Value) OperationError!void {
        if (a != ValueType.number or b != ValueType.number) {
            return OperationError.OperandsNotNumeric;
        }
    }

    pub fn add(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number + b.number };
    }

    pub fn sub(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number - b.number };
    }

    pub fn mul(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number * b.number };
    }

    pub fn div(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .number = a.number / b.number };
    }

    pub fn less(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number < b.number };
    }

    pub fn greater(a: Value, b: Value) OperationError!Value {
        try Value.check_numeric(a, b);
        return Value{ .boolean = a.number > b.number };
    }
};
