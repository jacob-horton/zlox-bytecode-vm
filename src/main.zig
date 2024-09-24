const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_vm = @import("vm.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const VM = zlox_vm.VM;

fn repl(vm: *VM) !void {
    var input: [1024]u8 = undefined;
    const stdin = std.io.getStdIn().reader();

    while (true) {
        std.debug.print("> ", .{});

        const line = (stdin.readUntilDelimiter(&input, '\n')) catch {
            std.debug.print("\n", .{});
            return;
        };

        _ = try vm.interpret(line);
    }
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    // TODO: remove 1MB restriction
    const mb = (1 << 10) << 10;
    return std.fs.cwd().readFileAlloc(allocator, path, mb);
}

fn runFile(vm: *VM, allocator: std.mem.Allocator, path: []const u8) !void {
    const contents = try readFile(allocator, path);
    defer allocator.free(contents);

    switch (try vm.interpret(contents)) {
        .OK => {},
        .COMPILE_ERROR => std.process.exit(65),
        .RUNTIME_ERROR => std.process.exit(70),
    }
}

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();

    // TODO: get rid of arena once sorted objects
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer {
        arena.deinit();
        _ = gpa.deinit();
    }

    const allocator = arena.allocator();

    var vm = try VM.init(allocator);

    if (args.next()) |file_name| {
        if (args.next() != null) {
            std.debug.print("Usage: clox [path]\n", .{});
            return std.process.exit(64);
        }

        try runFile(&vm, allocator, file_name);
    } else {
        try runFile(&vm, allocator, "./test.lox");
        // try repl(&vm);
    }
}
