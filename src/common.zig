pub const DEBUG_TRACE_EXECUTION = true;
pub const DEBUG_PRINT_CODE = true;

/// Gets the offset between two pointers, in number of items (rather than number of bytes)
pub fn ptrOffset(comptime T: type, start: [*]T, end: [*]T) usize {
    return (@intFromPtr(end) - @intFromPtr(start)) / @sizeOf(T);
}
