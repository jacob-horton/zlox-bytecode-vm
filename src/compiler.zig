const std = @import("std");

const zlox_scanner = @import("scanner.zig");
const zlox_chunk = @import("chunk.zig");
const zlox_value = @import("value.zig");
const zlox_common = @import("common.zig");
const zlox_debug = @import("debug.zig");

const Scanner = zlox_scanner.Scanner;
const Token = zlox_scanner.Token;
const TokenType = zlox_scanner.TokenType;

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const Value = zlox_value.Value;

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
};

// Implicitly assigns integers ascendingly, so goes from lowest to highest precedence
const Precedence = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

const ParseFn = *const fn (self: *Compiler) anyerror!void;
const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = Precedence.NONE,
};

const rules = blk: {
    const num_token_types = @typeInfo(TokenType).Enum.fields.len;

    // Rules for each token type. Default is { .infix = null, .prefix = null, .precedence = Precedence.NONE }
    var _rules = [_]ParseRule{ParseRule{}} ** num_token_types;

    _rules[@intFromEnum(TokenType.LEFT_PAREN)] = ParseRule{ .prefix = Compiler.grouping };
    _rules[@intFromEnum(TokenType.BANG)] = ParseRule{ .prefix = &Compiler.unary };
    _rules[@intFromEnum(TokenType.BANG_EQUAL)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.EQUALITY };
    _rules[@intFromEnum(TokenType.EQUAL_EQUAL)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.EQUALITY };
    _rules[@intFromEnum(TokenType.GREATER)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.COMPARISON };
    _rules[@intFromEnum(TokenType.GREATER_EQUAL)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.COMPARISON };
    _rules[@intFromEnum(TokenType.LESS)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.COMPARISON };
    _rules[@intFromEnum(TokenType.LESS_EQUAL)] = ParseRule{ .infix = &Compiler.binary, .precedence = Precedence.COMPARISON };
    _rules[@intFromEnum(TokenType.MINUS)] = ParseRule{ .prefix = &Compiler.unary, .infix = Compiler.binary, .precedence = Precedence.TERM };
    _rules[@intFromEnum(TokenType.PLUS)] = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.TERM };
    _rules[@intFromEnum(TokenType.SLASH)] = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.FACTOR };
    _rules[@intFromEnum(TokenType.STAR)] = ParseRule{ .infix = Compiler.binary, .precedence = Precedence.FACTOR };
    _rules[@intFromEnum(TokenType.NUMBER)] = ParseRule{ .prefix = Compiler.number };
    _rules[@intFromEnum(TokenType.FALSE)] = ParseRule{ .prefix = Compiler.literal };
    _rules[@intFromEnum(TokenType.TRUE)] = ParseRule{ .prefix = Compiler.literal };
    _rules[@intFromEnum(TokenType.NIL)] = ParseRule{ .prefix = Compiler.literal };

    break :blk _rules;
};

fn getRule(typ: TokenType) *const ParseRule {
    return &rules[@intFromEnum(typ)];
}

pub const Compiler = struct {
    parser: Parser,
    scanner: Scanner,
    compiling_chunk: *Chunk,

    pub fn init() Compiler {
        return Compiler{
            .parser = Parser{
                .had_error = false,
                .panic_mode = false,
                // These will get set up when we `advance()` which is kicked off in `compile()` before doing anything
                .previous = undefined,
                .current = undefined,
            },
            // These will get set up when we `compile()` i.e. before doing anything
            .scanner = undefined,
            .compiling_chunk = undefined,
        };
    }

    pub fn compile(self: *Compiler, source: []u8, chunk: *Chunk) !bool {
        self.scanner = Scanner.init(source);
        self.compiling_chunk = chunk;

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.advance();
        try self.expression();
        self.consume(TokenType.EOF, "Expect end of expression.");

        try self.end();

        return !self.parser.had_error;
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(Precedence.ASSIGNMENT);
    }

    fn grouping(self: *Compiler) !void {
        try self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
    }

    fn number(self: *Compiler) !void {
        const token = self.parser.previous;
        const value = try std.fmt.parseFloat(f64, token.start[0..token.length]);
        try self.emitConstant(Value{ .number = value });
    }

    fn unary(self: *Compiler) !void {
        const op_type = self.parser.previous.type;

        // Compile the operand
        try self.parsePrecedence(Precedence.UNARY);

        // Emit the op instruction
        switch (op_type) {
            TokenType.MINUS => try self.emitByte(@intFromEnum(OpCode.NEGATE)),
            TokenType.BANG => try self.emitByte(@intFromEnum(OpCode.NOT)),
            else => unreachable,
        }
    }

    fn binary(self: *Compiler) !void {
        const op_type = self.parser.previous.type;
        const rule = getRule(op_type);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (op_type) {
            TokenType.BANG_EQUAL => try self.emitBytes(@intFromEnum(OpCode.EQUAL), @intFromEnum(OpCode.NOT)),
            TokenType.EQUAL_EQUAL => try self.emitByte(@intFromEnum(OpCode.EQUAL)),
            TokenType.GREATER => try self.emitByte(@intFromEnum(OpCode.GREATER)),
            TokenType.GREATER_EQUAL => try self.emitBytes(@intFromEnum(OpCode.LESS), @intFromEnum(OpCode.NOT)),
            TokenType.LESS => try self.emitByte(@intFromEnum(OpCode.LESS)),
            TokenType.LESS_EQUAL => try self.emitBytes(@intFromEnum(OpCode.GREATER), @intFromEnum(OpCode.NOT)),
            TokenType.PLUS => try self.emitByte(@intFromEnum(OpCode.ADD)),
            TokenType.MINUS => try self.emitByte(@intFromEnum(OpCode.SUBTRACT)),
            TokenType.STAR => try self.emitByte(@intFromEnum(OpCode.MULTIPLY)),
            TokenType.SLASH => try self.emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => unreachable,
        }
    }

    fn literal(self: *Compiler) !void {
        switch (self.parser.previous.type) {
            TokenType.FALSE => try self.emitByte(@intFromEnum(OpCode.FALSE)),
            TokenType.TRUE => try self.emitByte(@intFromEnum(OpCode.TRUE)),
            TokenType.NIL => try self.emitByte(@intFromEnum(OpCode.NIL)),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
        self.advance();
        const rule = getRule(self.parser.previous.type);
        if (rule.*.prefix) |prefix_rule| {
            try prefix_rule(self);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
            self.advance();
            // TODO: handle null infix?
            const infix_rule = getRule(self.parser.previous.type).infix.?;
            try infix_rule(self);
        }
    }

    fn advance(self: *Compiler) void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = self.scanner.scanToken();
            const current_token = self.parser.current;

            if (current_token.type != TokenType.ERROR) break;

            self.errAtCurrent(current_token.start[0..current_token.length]);
        }
    }

    fn consume(self: *Compiler, typ: TokenType, message: []const u8) void {
        if (self.parser.current.type == typ) {
            self.advance();
            return;
        }

        self.errAtCurrent(message);
    }

    fn end(self: *Compiler) !void {
        try self.emitReturn();

        if (zlox_common.DEBUG_PRINT_CODE and self.parser.had_error) {
            zlox_debug.dissassembleChunk(self.currentChunk(), "code");
        }
    }

    fn emitConstant(self: *Compiler, value: Value) !void {
        try self.emitBytes(@intFromEnum(OpCode.CONSTANT), try self.makeConstant(value));
    }

    fn emitReturn(self: *Compiler) !void {
        try self.emitByte(@intFromEnum(OpCode.RETURN));
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return self.compiling_chunk;
    }

    fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.currentChunk().write(byte, self.parser.previous.line);
    }

    fn makeConstant(self: *Compiler, value: Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constant);
    }

    fn errAtCurrent(self: *Compiler, message: []const u8) void {
        self.errAt(&self.parser.previous, message);
    }

    fn err(self: *Compiler, message: []const u8) void {
        self.errAt(&self.parser.previous, message);
    }

    fn errAt(self: *Compiler, token: *Token, message: []const u8) void {
        if (self.parser.panic_mode) return;
        self.parser.panic_mode = true;

        std.debug.print("[line {d}] Error", .{token.*.line});

        if (token.*.type == TokenType.EOF) {
            std.debug.print(" at end", .{});
        } else if (token.*.type != TokenType.ERROR) {
            std.debug.print(" at '{s}'", .{token.*.start[0..token.*.length]});
        }

        std.debug.print(" {s}\n", .{message});
        self.parser.had_error = true;
    }
};
