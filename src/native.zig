const std = @import("std");

const zlox_value = @import("value.zig");

const Value = zlox_value.Value;

pub fn clock(_: u8, _: [*]Value) Value {
    return Value.initNumber((@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0));
}
