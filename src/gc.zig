const std = @import("std");

pub const GC = struct {
    backing_allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(backing_allocator: std.mem.Allocator) Self {
        return Self{
            .backing_allocator = backing_allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
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
        return self.backing_allocator.rawResize(old_mem, log2_old_align_u8, new_size, ret_addr);
    }

    fn free(
        ctx: *anyopaque,
        old_mem: []u8,
        log2_old_align_u8: u8,
        ret_addr: usize,
    ) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
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
