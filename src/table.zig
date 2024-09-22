const std = @import("std");

const zlox_object = @import("object.zig");
const zlox_value = @import("value.zig");

const String = zlox_object.Obj.String;

const Value = zlox_value.Value;

const MAX_LOAD: f32 = 0.75;

pub const Table = struct {
    allocator: std.mem.Allocator,
    count: usize,
    capacity: usize,
    entries: ?[]Entry,

    pub fn init(allocator: std.mem.Allocator) Table {
        return Table{
            .allocator = allocator,
            .count = 0,
            .capacity = 0,
            .entries = null,
        };
    }

    pub fn deinit(self: *Table) void {
        if (self.entries) |entries| {
            self.allocator.free(entries);
        }
    }

    pub fn set(self: *Table, key: *String, value: Value) !bool {
        if (@as(f32, @floatFromInt(self.count + 1)) > @as(f32, @floatFromInt(self.capacity)) * MAX_LOAD) {
            try self.growCapacity();
        }

        const entry = self.findEntry(key).?;
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value == .nil) self.count += 1;

        entry.key = key;
        entry.value = value;

        return is_new_key;
    }

    fn findEntry(self: *Table, key: *String) ?*Entry {
        if (self.entries == null) return null;

        var index = key.hash % self.capacity;

        while (true) {
            const entry = &self.entries.?[index];
            var tombstone: ?*Entry = null;

            if (entry.key == null) {
                if (entry.value == .nil) {
                    // Empty entry
                    return tombstone orelse entry;
                } else {
                    // Found a tombstone
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                // Found the key
                return entry;
            }

            index = (index + 1) % self.capacity;
        }
    }

    fn growCapacity(self: *Table) !void {
        const new_capacity = if (self.capacity < 8) 8 else self.capacity * 2;
        const entries = try self.allocator.alloc(Entry, new_capacity);

        // Set default values
        for (entries) |*entry| {
            entry.key = null;
            entry.value = .nil;
        }

        self.count = 0;

        // Re-insert all entries
        if (self.entries) |old_entries| {
            for (old_entries) |entry| {
                if (entry.key) |entry_key| {
                    const dest = self.findEntry(entry_key).?;
                    dest.key = entry_key;
                    dest.value = entry.value;
                    self.count += 1;
                }
            }

            self.allocator.free(old_entries);
        }

        self.entries = entries;
        self.capacity = new_capacity;
    }

    fn addAll(self: *Table, from: *Table) void {
        if (from.entries) |from_entries| {
            for (from_entries) |entry| {
                if (entry.key == null) continue;

                self.set(entry.key, entry.value);
            }
        }
    }

    pub fn get(self: *Table, key: *String, value: *Value) bool {
        const entry = self.findEntry(key);
        if (entry == null or entry.?.key == null) {
            return false;
        }

        value.* = entry.?.value;
        return true;
    }

    pub fn delete(self: *Table, key: *String) bool {
        const entry = self.findEntry(key);
        if (entry == null or entry.?.key == null) {
            return false;
        }

        entry.?.key = null;
        entry.?.value = Value{ .boolean = true };
        return true;
    }

    // TODO: take []const u8 instead
    pub fn findString(self: *Table, key: [*]const u8, len: usize, hash: u32) ?*String {
        if (self.entries == null) return null;

        var index = hash % self.capacity;

        while (true) {
            const entry = &self.entries.?[index];

            if (entry.key) |entry_key| {
                if (entry_key.chars.len == len and entry_key.hash == hash and std.mem.eql(u8, entry_key.chars, key[0..len])) {
                    return entry.key;
                }
            } else {
                if (entry.value == .nil) {
                    // Stop if we find an empty non-tombstone entry
                    return null;
                }
            }

            index = (index + 1) % self.capacity;
        }
    }
};

pub const Entry = struct {
    key: ?*String,
    value: Value,
};