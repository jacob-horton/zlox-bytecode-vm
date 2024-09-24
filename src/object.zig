const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_value = @import("value.zig");
const zlox_vm = @import("vm.zig");

const Chunk = zlox_chunk.Chunk;

const Value = zlox_value.Value;

const VM = zlox_vm.VM;

pub const ObjType = enum {
    STRING,
    FUNCTION,
    NATIVE,
};

pub const NativeFn = *const fn (arg_count: u8, args: [*]Value) Value;

pub const Obj = struct {
    type: ObjType,
    // vm: *VM,

    // TODO: store linked list of objs and deinit them with VM
    pub fn init(vm: *VM, comptime T: type, typ: ObjType) !*Obj {
        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .type = typ,
            // .vm = vm,
        };

        return &ptr.obj;
    }

    pub fn print(self: *Obj) void {
        // TODO: define `print` on all sub-structs, then call `as` with dynamically generated type from enum. then no switch neded
        switch (self.type) {
            .STRING => {
                const str = self.as(String);
                std.debug.print("{s}", .{str.chars});
            },
            .FUNCTION => {
                self.as(Function).print();
            },
            .NATIVE => {
                self.as(Native).print();
            },
        }
    }

    pub fn deinit(self: *Obj) void {
        switch (self.type) {
            .STRING => self.as(String).deinit(),
            .FUNCTION => self.as(Function).deinit(),
            .NATIVE => self.as(Native).deinit(),
        }
    }

    pub fn as(self: *Obj, comptime T: type) *T {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub const String = struct {
        obj: Obj,
        chars: []const u8,
        hash: u32,

        fn init(vm: *VM, chars: []const u8, str_hash: u32) !*String {
            const str = (try Obj.init(vm, String, .STRING)).as(String);

            str.chars = chars;
            str.hash = str_hash;
            _ = try vm.strings.set(str, .nil);

            return str;
        }

        pub fn deinit(self: *String) void {
            _ = self;
            // self.obj.vm.allocator.destroy(self);
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

    pub const Function = struct {
        obj: Obj,
        arity: usize,
        chunk: Chunk,
        name: ?*String,

        pub fn init(vm: *VM) !*Function {
            const fun = (try Obj.init(vm, Function, .FUNCTION)).as(Function);
            fun.arity = 0;
            fun.name = null;
            fun.chunk = Chunk.init(vm.allocator);

            return fun;
        }

        pub fn deinit(self: *Function) void {
            self.chunk.deinit();
            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(self: *Function) void {
            if (self.name) |name| {
                std.debug.print("<fn {s}>", .{name.chars});
            } else {
                std.debug.print("<script>", .{});
            }
        }
    };

    pub const Native = struct {
        obj: Obj,
        function: NativeFn,

        pub fn init(vm: *VM, function: NativeFn) !*Native {
            const native = (try Obj.init(vm, Native, .NATIVE)).as(Native);
            native.function = function;

            return native;
        }

        pub fn deinit(self: *Native) void {
            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(_: *Native) void {
            std.debug.print("<native fn>", .{});
        }
    };
};
