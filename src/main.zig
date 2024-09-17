const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const InterpretResult = @import("vm.zig").InterpretResult;

fn repl() !void {
    var input: [1024]u8 = undefined;
    const stdin = std.io.getStdIn().reader();

    while (true) {
        std.debug.print("> ", .{});

        const line = (stdin.readUntilDelimiter(&input, '\n')) catch {
            std.debug.print("\n", .{});
            return;
        };

        _ = VM.interpret(line);
    }
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const mb = (1 << 10) << 10;
    return std.fs.cwd().readFileAlloc(allocator, path, mb);
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const contents = try readFile(allocator, path);
    defer allocator.free(contents);

    switch (VM.interpret(contents)) {
        InterpretResult.OK => {},
        InterpretResult.COMPILE_ERROR => std.process.exit(65),
        InterpretResult.RUNTIME_ERROR => std.process.exit(70),
    }
}

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    // var vm = VM.init();

    if (args.next()) |file_name| {
        if (args.next() != null) {
            std.debug.print("Usage: clox [path]\n", .{});
            return std.process.exit(64);
        }

        try runFile(allocator, file_name);
    } else {
        try repl();
    }

    // debug.dissassembleChunk(&chunk, "test chunk");
}
