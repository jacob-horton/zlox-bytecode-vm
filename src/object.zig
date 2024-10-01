const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_common = @import("common.zig");
const zlox_table = @import("table.zig");
const zlox_value = @import("value.zig");
const zlox_vm = @import("vm.zig");

const Chunk = zlox_chunk.Chunk;

const Table = zlox_table.Table;

const Value = zlox_value.Value;

const VM = zlox_vm.VM;

pub const ObjType = enum {
    STRING,
    CLOSURE,
    FUNCTION,
    NATIVE,
    UPVALUE,
    CLASS,
    INSTANCE,
};

pub const NativeFn = *const fn (arg_count: u8, args: [*]Value) Value;

pub const Obj = struct {
    type: ObjType,
    vm: *VM,
    next: ?*Obj,
    is_marked: bool,

    pub fn init(vm: *VM, comptime T: type, typ: ObjType) !*Obj {
        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .type = typ,
            .vm = vm,
            .next = vm.objects,
            .is_marked = false,
        };

        vm.objects = &ptr.obj;

        if (zlox_common.DEBUG_LOC_GC) {
            std.debug.print("{*} allocated {d} bytes\n", .{ ptr, @sizeOf(T) });
        }

        return &ptr.obj;
    }

    pub fn print(self: *Obj) void {
        switch (self.type) {
            .STRING => self.as(String).print(),
            .CLOSURE => self.as(Closure).print(),
            .FUNCTION => self.as(Function).print(),
            .NATIVE => self.as(Native).print(),
            .UPVALUE => self.as(Upvalue).print(),
            .CLASS => self.as(Class).print(),
            .INSTANCE => self.as(Instance).print(),
        }
    }

    pub fn deinit(self: *Obj) void {
        switch (self.type) {
            .STRING => self.as(String).deinit(),
            .CLOSURE => self.as(Closure).deinit(),
            .FUNCTION => self.as(Function).deinit(),
            .NATIVE => self.as(Native).deinit(),
            .UPVALUE => self.as(Upvalue).deinit(),
            .CLASS => self.as(Class).deinit(),
            .INSTANCE => self.as(Instance).deinit(),
        }
    }

    pub fn deinitAll(self: *Obj) void {
        if (self.next) |next| {
            next.deinitAll();
        }

        self.deinit();
    }

    pub fn as(self: *Obj, T: type) *T {
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

            vm.push(Value{ .obj = &str.obj });
            _ = try vm.strings.set(str, .nil);
            _ = vm.pop();

            return str;
        }

        pub fn deinit(self: *String) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.obj.vm.allocator.free(self.chars);
            self.obj.vm.allocator.destroy(self);
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

        pub fn print(self: String) void {
            std.debug.print("{s}", .{self.chars});
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalues: []?*Upvalue,
        upvalue_count: usize,

        /// Does not own `function`
        pub fn init(vm: *VM, function: *Function) !*Closure {
            const closure = (try Obj.init(vm, Closure, .CLOSURE)).as(Closure);
            closure.function = function;
            closure.upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalue_count);
            closure.upvalue_count = function.upvalue_count;

            for (0..function.upvalue_count) |i| {
                closure.upvalues[i] = null;
            }

            return closure;
        }

        pub fn deinit(self: *Closure) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.obj.vm.allocator.free(self.upvalues);
            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(self: Closure) void {
            self.function.print();
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: usize,
        upvalue_count: usize,
        chunk: Chunk,
        name: ?*String,

        pub fn init(vm: *VM) !*Function {
            const fun = (try Obj.init(vm, Function, .FUNCTION)).as(Function);
            fun.arity = 0;
            fun.upvalue_count = 0;
            fun.name = null;
            fun.chunk = Chunk.init(vm.allocator);

            return fun;
        }

        pub fn deinit(self: *Function) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.chunk.deinit();
            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(self: Function) void {
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
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(_: Native) void {
            std.debug.print("<native fn>", .{});
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        /// Does not own slot
        pub fn init(vm: *VM, slot: *Value) !*Upvalue {
            const upvalue = (try Obj.init(vm, Upvalue, .UPVALUE)).as(Upvalue);
            upvalue.location = slot;
            upvalue.next = null;
            upvalue.closed = .nil;

            return upvalue;
        }

        pub fn deinit(self: *Upvalue) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(_: Upvalue) void {
            std.debug.print("upvalue", .{});
        }
    };

    pub const Class = struct {
        obj: Obj,
        name: *String,

        /// Does not own name
        pub fn init(vm: *VM, name: *String) !*Class {
            const class = (try Obj.init(vm, Class, .CLASS)).as(Class);
            class.name = name;

            return class;
        }

        pub fn deinit(self: *Class) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(self: Class) void {
            std.debug.print("{s}", .{self.name.chars});
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: Table,

        /// Does not own class
        pub fn init(vm: *VM, class: *Class) !*Instance {
            const instance = (try Obj.init(vm, Instance, .INSTANCE)).as(Instance);
            instance.class = class;
            instance.fields = Table.init(vm.allocator);

            return instance;
        }

        pub fn deinit(self: *Instance) void {
            if (zlox_common.DEBUG_LOC_GC) {
                std.debug.print("{*} freed\n", .{self});
            }

            self.fields.deinit(); // NOTE: does not free entries - they may be referenced elsewhere. GC will handle
            self.obj.vm.allocator.destroy(self);
        }

        pub fn print(self: Instance) void {
            std.debug.print("{s} instance", .{self.class.name.chars});
        }
    };
};
