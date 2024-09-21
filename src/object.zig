const std = @import("std");

const zlox_value = @import("value.zig");

pub const ObjType = enum {
    STRING,
};

pub const Obj = struct {
    type: ObjType,

    // TODO: take VM as argument here and in deinit to allow for linked list of objs
    pub fn init(allocator: std.mem.Allocator, comptime T: type, typ: ObjType) !*Obj {
        const ptr = try allocator.create(T);
        ptr.obj = Obj{
            .type = typ,
        };

        return &ptr.obj;
    }

    pub fn print(self: *Obj) void {
        switch (self.type) {
            ObjType.STRING => {
                const str = self.asString();
                std.debug.print("{s}", .{str.chars});
            },
        }
    }

    pub fn asString(self: *Obj) *String {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub const String = struct {
        obj: Obj,
        chars: []const u8,

        /// Takes the chars, without copying
        pub fn init(allocator: std.mem.Allocator, chars: []const u8) !*String {
            const str = (try Obj.init(allocator, String, ObjType.STRING)).asString();
            str.chars = chars;

            return str;
        }

        /// Copies the chars to the heap before initialising
        pub fn copyInit(allocator: std.mem.Allocator, chars: [*]const u8, length: usize) !*String {
            const heap_chars = try allocator.alloc(u8, length);
            @memcpy(heap_chars, chars);

            return String.init(allocator, heap_chars);
        }
    };
};
