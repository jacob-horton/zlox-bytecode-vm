const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const TokenType = @import("scanner.zig").TokenType;

pub fn compile(source: []u8) void {
    var scanner = Scanner.init(source);

    var line: isize = -1;
    while (true) {
        const token = scanner.scanToken();

        if (token.line != line) {
            std.debug.print("{d:4} ", .{token.line});
            line = @intCast(token.line);
        } else {
            std.debug.print("   | ", .{});
        }

        std.debug.print("{s:<16} {s}\n", .{ @tagName(token.type), token.start[0..token.length] });

        if (token.type == TokenType.EOF) break;
    }
}
