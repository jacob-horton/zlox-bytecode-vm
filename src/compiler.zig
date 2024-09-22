const std = @import("std");

const zlox_chunk = @import("chunk.zig");
const zlox_common = @import("common.zig");
const zlox_debug = @import("debug.zig");
const zlox_object = @import("object.zig");
const zlox_scanner = @import("scanner.zig");
const zlox_value = @import("value.zig");
const zlox_vm = @import("vm.zig");

const Chunk = zlox_chunk.Chunk;
const OpCode = zlox_chunk.OpCode;

const String = zlox_object.Obj.String;

const Scanner = zlox_scanner.Scanner;
const Token = zlox_scanner.Token;
const TokenType = zlox_scanner.TokenType;

const Value = zlox_value.Value;

const VM = zlox_vm.VM;

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

const ParseFn = *const fn (self: *Parser, can_assign: bool) anyerror!void;
const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
};

const rules = blk: {
    const num_token_types = @typeInfo(TokenType).Enum.fields.len;

    // Rules for each token type. Default is { .infix = null, .prefix = null, .precedence = .NONE }
    var _rules = [_]ParseRule{ParseRule{}} ** num_token_types;

    _rules[@intFromEnum(TokenType.LEFT_PAREN)] = ParseRule{ .prefix = Parser.grouping };
    _rules[@intFromEnum(TokenType.BANG)] = ParseRule{ .prefix = &Parser.unary };
    _rules[@intFromEnum(TokenType.BANG_EQUAL)] = ParseRule{ .infix = &Parser.binary, .precedence = .EQUALITY };
    _rules[@intFromEnum(TokenType.EQUAL_EQUAL)] = ParseRule{ .infix = &Parser.binary, .precedence = .EQUALITY };
    _rules[@intFromEnum(TokenType.GREATER)] = ParseRule{ .infix = &Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.GREATER_EQUAL)] = ParseRule{ .infix = &Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.LESS)] = ParseRule{ .infix = &Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.LESS_EQUAL)] = ParseRule{ .infix = &Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.IDENTIFIER)] = ParseRule{ .prefix = &Parser.variable, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.MINUS)] = ParseRule{ .prefix = &Parser.unary, .infix = Parser.binary, .precedence = .TERM };
    _rules[@intFromEnum(TokenType.PLUS)] = ParseRule{ .infix = Parser.binary, .precedence = .TERM };
    _rules[@intFromEnum(TokenType.SLASH)] = ParseRule{ .infix = Parser.binary, .precedence = .FACTOR };
    _rules[@intFromEnum(TokenType.STAR)] = ParseRule{ .infix = Parser.binary, .precedence = .FACTOR };
    _rules[@intFromEnum(TokenType.STRING)] = ParseRule{ .prefix = Parser.string };
    _rules[@intFromEnum(TokenType.NUMBER)] = ParseRule{ .prefix = Parser.number };
    _rules[@intFromEnum(TokenType.FALSE)] = ParseRule{ .prefix = Parser.literal };
    _rules[@intFromEnum(TokenType.TRUE)] = ParseRule{ .prefix = Parser.literal };
    _rules[@intFromEnum(TokenType.NIL)] = ParseRule{ .prefix = Parser.literal };

    break :blk _rules;
};

fn getRule(typ: TokenType) *const ParseRule {
    return &rules[@intFromEnum(typ)];
}

pub const Compiler = struct {
    compiling_chunk: *Chunk,
    parser: *Parser,

    pub fn compile(vm: *VM, source: []u8, chunk: *Chunk) !bool {
        var compiler = Compiler{ .compiling_chunk = chunk, .parser = undefined };

        var scanner = Scanner.init(source);
        var parser = Parser.init(vm, &compiler, &scanner);
        compiler.parser = &parser;

        try parser.parse();
        try compiler.end();

        return !parser.had_error;
    }

    fn end(self: *Compiler) !void {
        try self.emitReturn();

        if (zlox_common.DEBUG_PRINT_CODE and self.parser.had_error) {
            zlox_debug.dissassembleChunk(self.currentChunk());
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
            self.parser.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constant);
    }
};

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    scanner: *Scanner,
    compiler: *Compiler,
    vm: *VM,

    pub fn init(vm: *VM, compiler: *Compiler, scanner: *Scanner) Parser {
        var parser = Parser{
            .had_error = false,
            .panic_mode = false,

            // These will get set up when we `advance()`
            .previous = undefined,
            .current = undefined,

            .vm = vm,
            .scanner = scanner,
            .compiler = compiler,
        };

        parser.advance();
        return parser;
    }

    pub fn parse(self: *Parser) !void {
        while (!self.match(.EOF)) {
            try self.declaration();
        }
    }

    fn match(self: *Parser, typ: TokenType) bool {
        if (!self.check(typ)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        return self.current.type == typ;
    }

    fn declaration(self: *Parser) !void {
        if (self.match(.VAR)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) self.synchronise();
    }

    fn synchronise(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.type != .EOF) {
            if (self.previous.type == .SEMICOLON) return;

            switch (self.current.type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            self.advance();
        }
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (self.match(.EQUAL)) {
            try self.expression();
        } else {
            try self.compiler.emitByte(@intFromEnum(OpCode.NIL));
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        try self.compiler.emitBytes(@intFromEnum(OpCode.DEFINE_GLOBAL), global);
    }

    fn parseVariable(self: *Parser, error_message: []const u8) !u8 {
        self.consume(.IDENTIFIER, error_message);
        return try self.identifierConstant(&self.previous);
    }

    fn identifierConstant(self: *Parser, name: *Token) !u8 {
        const str = try String.copyInit(self.vm, name.start, name.length);
        return try self.compiler.makeConstant(Value{ .obj = &str.obj });
    }

    fn statement(self: *Parser) !void {
        if (self.match(.PRINT)) {
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        try self.compiler.emitByte(@intFromEnum(OpCode.PRINT));
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after expression.");
        try self.compiler.emitByte(@intFromEnum(OpCode.POP));
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.ASSIGNMENT);
    }

    fn grouping(self: *Parser, _: bool) !void {
        try self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    fn number(self: *Parser, _: bool) !void {
        const token = self.previous;
        const value = try std.fmt.parseFloat(f64, token.start[0..token.length]);
        try self.compiler.emitConstant(Value{ .number = value });
    }

    fn string(self: *Parser, _: bool) !void {
        const str = try String.copyInit(self.vm, self.previous.start + 1, self.previous.length - 2);
        try self.compiler.emitConstant(Value{ .obj = &str.obj });
    }

    fn variable(self: *Parser, can_assign: bool) !void {
        try self.namedVariable(&self.previous, can_assign);
    }

    fn namedVariable(self: *Parser, name: *Token, can_assign: bool) !void {
        const arg = try self.identifierConstant(name);

        if (can_assign and self.match(.EQUAL)) {
            try self.expression();
            try self.compiler.emitBytes(@intFromEnum(OpCode.SET_GLOBAL), arg);
        } else {
            try self.compiler.emitBytes(@intFromEnum(OpCode.GET_GLOBAL), arg);
        }
    }

    fn unary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;

        // Compile the operand
        try self.parsePrecedence(.UNARY);

        // Emit the op instruction
        switch (op_type) {
            .MINUS => try self.compiler.emitByte(@intFromEnum(OpCode.NEGATE)),
            .BANG => try self.compiler.emitByte(@intFromEnum(OpCode.NOT)),
            else => unreachable,
        }
    }

    fn binary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;
        const rule = getRule(op_type);

        // +1 to make it left associative
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (op_type) {
            .BANG_EQUAL => try self.compiler.emitBytes(@intFromEnum(OpCode.EQUAL), @intFromEnum(OpCode.NOT)),
            .EQUAL_EQUAL => try self.compiler.emitByte(@intFromEnum(OpCode.EQUAL)),
            .GREATER => try self.compiler.emitByte(@intFromEnum(OpCode.GREATER)),
            .GREATER_EQUAL => try self.compiler.emitBytes(@intFromEnum(OpCode.LESS), @intFromEnum(OpCode.NOT)),
            .LESS => try self.compiler.emitByte(@intFromEnum(OpCode.LESS)),
            .LESS_EQUAL => try self.compiler.emitBytes(@intFromEnum(OpCode.GREATER), @intFromEnum(OpCode.NOT)),
            .PLUS => try self.compiler.emitByte(@intFromEnum(OpCode.ADD)),
            .MINUS => try self.compiler.emitByte(@intFromEnum(OpCode.SUBTRACT)),
            .STAR => try self.compiler.emitByte(@intFromEnum(OpCode.MULTIPLY)),
            .SLASH => try self.compiler.emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => unreachable,
        }
    }

    fn literal(self: *Parser, _: bool) !void {
        switch (self.previous.type) {
            .FALSE => try self.compiler.emitByte(@intFromEnum(OpCode.FALSE)),
            .TRUE => try self.compiler.emitByte(@intFromEnum(OpCode.TRUE)),
            .NIL => try self.compiler.emitByte(@intFromEnum(OpCode.NIL)),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();
        const rule = getRule(self.previous.type);
        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);

        if (rule.*.prefix) |prefix_rule| {
            try prefix_rule(self, can_assign);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            // TODO: handle null infix?
            const infix_rule = getRule(self.previous.type).infix.?;
            try infix_rule(self, can_assign);
        }

        if (can_assign and self.match(.EQUAL)) {
            // self.err("Invalid assignment target.");
        }
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            const current_token = self.current;

            if (current_token.type != .ERROR) break;

            self.errAtCurrent(current_token.start[0..current_token.length]);
        }
    }

    fn consume(self: *Parser, typ: TokenType, message: []const u8) void {
        if (self.current.type == typ) {
            self.advance();
            return;
        }

        self.errAtCurrent(message);
    }

    fn errAtCurrent(self: *Parser, message: []const u8) void {
        self.errAt(&self.previous, message);
    }

    fn err(self: *Parser, message: []const u8) void {
        self.errAt(&self.previous, message);
    }

    fn errAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;

        std.debug.print("[line {d}] Error", .{token.*.line});

        if (token.*.type == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.*.type != .ERROR) {
            std.debug.print(" at '{s}'", .{token.*.start[0..token.*.length]});
        }

        std.debug.print(" {s}\n", .{message});
        self.had_error = true;
    }
};
