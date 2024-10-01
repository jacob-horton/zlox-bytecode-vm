const std = @import("std");

pub const DEBUG_TRACE_EXECUTION = false;
pub const DEBUG_PRINT_CODE = false;
pub const DEBUG_STRESS_GC = false;
pub const DEBUG_LOC_GC = false;

pub const U8_COUNT: u9 = std.math.maxInt(u8) + 1;

/// Gets the offset between two pointers, in number of items (rather than number of bytes)
pub fn ptrOffset(comptime T: type, start: [*]T, end: [*]T) usize {
    return (@intFromPtr(end) - @intFromPtr(start)) / @sizeOf(T);
}
