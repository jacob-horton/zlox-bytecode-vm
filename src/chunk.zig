const std = @import("std");

const zlox_value = @import("value.zig");

const Value = zlox_value.Value;
const ValueArray = zlox_value.ValueArray;

pub const OpCode = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    SET_GLOBAL,
    GET_GLOBAL,
    SET_LOCAL,
    GET_LOCAL,
    DEFINE_GLOBAL,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    RETURN,
};

const RleItem = struct {
    val: usize,
    count: usize,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(RleItem),
    constants: ValueArray,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(RleItem).init(allocator),
            .constants = ValueArray.init(allocator),
        };
    }

    fn addLine(self: *Chunk, line: usize) !void {
        if (self.lines.items.len == 0) {
            try self.lines.append(RleItem{ .val = line, .count = 1 });
            return;
        }

        var last: *RleItem = &self.lines.items[self.lines.items.len - 1];
        if (last.val == line) {
            last.count += 1;
            return;
        }

        try self.lines.append(RleItem{ .val = line, .count = 1 });
    }

    pub fn getLine(self: *Chunk, offset: usize) ?usize {
        // Decode the RLE
        var counter: usize = 0;
        var current_line: ?usize = null;

        for (self.lines.items) |line| {
            if (offset < counter) {
                break;
            }

            counter += line.count;
            current_line = line.val;
        }

        if (counter <= offset) {
            return null;
        }

        return current_line;
    }

    pub fn write(self: *Chunk, data: u8, line: usize) !void {
        try self.addLine(line);

        return self.code.append(data);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }

    pub fn deinit(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }
};
