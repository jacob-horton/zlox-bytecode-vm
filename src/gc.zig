const std = @import("std");

const zlox_common = @import("common.zig");
const zlox_compiler = @import("compiler.zig");
const zlox_object = @import("object.zig");
const zlox_table = @import("table.zig");
const zlox_value = @import("value.zig");
const zlox_vm = @import("vm.zig");

const Compiler = zlox_compiler.Compiler;

const Obj = zlox_object.Obj;
const Upvalue = zlox_object.Obj.Upvalue;
const Function = zlox_object.Obj.Function;
const Closure = zlox_object.Obj.Closure;
const Class = zlox_object.Obj.Class;
const Instance = zlox_object.Obj.Instance;
const BoundMethod = zlox_object.Obj.BoundMethod;

const Table = zlox_table.Table;

const Value = zlox_value.Value;
const ValueArray = zlox_value.ValueArray;

const VM = zlox_vm.VM;

const GC_HEAP_GROW_FACTOR = 2;

pub const GC = struct {
    backing_allocator: std.mem.Allocator,
    vm: *VM,

    gray_stack: std.ArrayList(*Obj),

    bytes_allocated: usize,
    next_collection: usize,

    const Self = @This();

    pub fn init(vm: *VM, backing_allocator: std.mem.Allocator) Self {
        return Self{
            .backing_allocator = backing_allocator,
            .vm = vm,

            .bytes_allocated = 0,
            .next_collection = 1024 * 1024, // 1MB

            .gray_stack = std.ArrayList(*Obj).init(backing_allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.gray_stack.deinit();
    }

    fn collectGarbage(self: *Self) !void {
        const before = self.bytes_allocated;
        if (zlox_common.DEBUG_LOC_GC) {
            std.debug.print("-- gc begin\n", .{});
        }

        try self.markRoots();
        try self.traceReferences();
        removeWhite(&self.vm.strings);
        self.sweep();

        self.next_collection = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (zlox_common.DEBUG_LOC_GC) {
            std.debug.print("-- gc end\n", .{});
            std.debug.print("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - self.bytes_allocated, before, self.bytes_allocated, self.next_collection });
        }
    }

    fn removeWhite(table: *Table) void {
        const entries_count = if (table.entries) |entries| entries.len else 0;
        for (0..entries_count) |i| {
            const entry = &table.entries.?[i];
            if (entry.key) |key| {
                if (!key.obj.is_marked) {
                    _ = table.delete(key);
                }
            }
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Obj = null;
        var object: ?*Obj = self.vm.objects;

        while (object != null) {
            if (object.?.is_marked) {
                object.?.is_marked = false;
                previous = object.?;
                object = object.?.next;
            } else {
                const unreached = object.?;
                object = object.?.next;

                if (previous) |prev| {
                    prev.next = object;
                } else {
                    self.vm.objects = object;
                }

                unreached.deinit();
            }
        }
    }

    fn traceReferences(self: *Self) !void {
        while (self.gray_stack.items.len > 0) {
            const obj = self.gray_stack.pop();
            try self.blackenObject(obj);
        }
    }

    fn blackenObject(self: *Self, obj: *Obj) !void {
        if (zlox_common.DEBUG_LOC_GC) {
            std.debug.print("{*} blacken ", .{obj});
            obj.print();
            std.debug.print("\n", .{});
        }

        switch (obj.type) {
            .NATIVE, .STRING => {},
            .INSTANCE => {
                const instance = obj.as(Instance);
                try self.markObject(&instance.class.obj);
                try self.markTable(&instance.fields);
            },
            .CLASS => {
                const class = obj.as(Class);
                try self.markObject(&class.name.obj);
                try self.markTable(&class.methods);
            },
            .BOUND_METHOD => {
                const bound = obj.as(BoundMethod);
                try self.markValue(bound.receiver);
                try self.markObject(&bound.method.obj);
            },
            .UPVALUE => try self.markValue(obj.as(Upvalue).closed),
            .FUNCTION => {
                const function = obj.as(Function);
                if (function.name) |name| try self.markObject(&name.obj);
                try self.markArray(&function.chunk.constants);
            },
            .CLOSURE => {
                const closure = obj.as(Closure);
                try self.markObject(&closure.function.obj);

                for (0..closure.upvalue_count) |i| {
                    if (closure.upvalues[i]) |upvalue| try self.markObject(&upvalue.obj);
                }
            },
        }
    }

    fn markRoots(self: *Self) !void {
        for (0..self.vm.stack_top) |i| {
            try self.markValue(self.vm.stack[i]);
        }

        for (0..self.vm.frame_count) |i| {
            try self.markObject(&self.vm.frames[i].closure.obj);
        }

        var next = self.vm.open_upvalues;
        while (next) |upvalue| {
            try self.markObject(&upvalue.obj);
            next = upvalue.next;
        }

        try self.markTable(&self.vm.globals);
        try self.markCompilerRoots();
        if (self.vm.init_string) |init_string| try self.markObject(&init_string.obj);
    }

    fn markCompilerRoots(self: *Self) !void {
        var next: ?*Compiler = if (self.vm.compiler) |comp| comp.parser.current_compiler else null;
        while (next) |compiler| {
            if (!compiler.is_init_finished) break;
            try self.markObject(&compiler.function.obj);
            next = compiler.enclosing;
        }
    }

    fn markArray(self: *Self, array: *ValueArray) !void {
        for (array.items) |value| {
            try self.markValue(value);
        }
    }

    fn markTable(self: *Self, table: *Table) !void {
        if (table.entries) |entries| {
            for (entries) |*entry| {
                if (entry.key) |key| try self.markObject(&key.obj);
                try self.markValue(entry.value);
            }
        }
    }

    fn markValue(self: *Self, value: Value) !void {
        if (value == .obj) try self.markObject(value.obj);
    }

    fn markObject(self: *Self, object: *Obj) !void {
        if (object.is_marked) return;

        if (zlox_common.DEBUG_LOC_GC) {
            std.debug.print("{*} mark ({any}) ", .{ object, object.type });
            object.print();
            std.debug.print("\n", .{});
        }

        object.is_marked = true;
        try self.gray_stack.append(object);
    }

    fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated += len;

        if (zlox_common.DEBUG_STRESS_GC or self.bytes_allocated > self.next_collection) {
            self.collectGarbage() catch |err| {
                // TODO: handle properly
                std.debug.print("Garbage collection error {}", .{err});
                std.process.exit(70);
            };
        }

        return self.backing_allocator.rawAlloc(len, log2_ptr_align, ret_addr);
    }

    fn resize(
        ctx: *anyopaque,
        old_mem: []u8,
        log2_old_align_u8: u8,
        new_size: usize,
        ret_addr: usize,
    ) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated += new_size - old_mem.len;

        if ((new_size > old_mem.len and zlox_common.DEBUG_STRESS_GC) or self.bytes_allocated > self.next_collection) {
            self.collectGarbage() catch |err| {
                // TODO: handle properly
                std.debug.print("Garbage collection error {}", .{err});
                std.process.exit(70);
            };
        }

        return self.backing_allocator.rawResize(old_mem, log2_old_align_u8, new_size, ret_addr);
    }

    fn free(
        ctx: *anyopaque,
        old_mem: []u8,
        log2_old_align_u8: u8,
        ret_addr: usize,
    ) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated -= old_mem.len;

        return self.backing_allocator.rawFree(old_mem, log2_old_align_u8, ret_addr);
    }

    pub fn allocator(self: *GC) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }
};
