const std = @import("std");

const zlox_value = @import("value.zig");
const zlox_vm = @import("vm.zig");

const VM = zlox_vm.VM;

pub const ObjType = enum {
    STRING,
};

pub const Obj = struct {
    type: ObjType,

    // TODO: store linked list of objs and deinit free them with VM
    pub fn init(vm: *VM, comptime T: type, typ: ObjType) !*Obj {
        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .type = typ,
        };

        return &ptr.obj;
    }

    pub fn print(self: *Obj) void {
        switch (self.type) {
            .STRING => {
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
        hash: u32,

        fn init(vm: *VM, chars: []const u8, str_hash: u32) !*String {
            const str = (try Obj.init(vm, String, .STRING)).asString();

            str.chars = chars;
            str.hash = str_hash;
            _ = try vm.strings.set(str, .nil);

            return str;
        }

        /// Copies the chars to the heap before initialising
        pub fn copyInit(vm: *VM, chars: [*]const u8, length: usize) !*String {
            const str_hash = String.hash(chars[0..length]);
            const interned = vm.strings.findString(chars, length, str_hash);
            if (interned) |i| return i;

            const heap_chars = try vm.allocator.alloc(u8, length);
            @memcpy(heap_chars, chars);

            return String.init(vm, heap_chars, str_hash);
        }

        /// Takes the chars, without copying
        pub fn takeInit(vm: *VM, chars: []const u8) !*String {
            const str_hash = String.hash(chars);
            const interned = vm.strings.findString(chars.ptr, chars.len, str_hash);
            if (interned) |i| {
                vm.allocator.free(chars);
                return i;
            }

            return String.init(vm, chars, str_hash);
        }

        fn hash(chars: []const u8) u32 {
            var result: u32 = 2166136261;

            for (chars) |char| {
                result ^= char;
                result += 16777619;
            }

            return result;
        }
    };
};
