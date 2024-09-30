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
const Function = zlox_object.Obj.Function;

const Scanner = zlox_scanner.Scanner;
const Token = zlox_scanner.Token;
const TokenType = zlox_scanner.TokenType;

const Value = zlox_value.Value;

const VM = zlox_vm.VM;

pub const FunctionType = enum {
    FUNCTION,
    SCRIPT,
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

    _rules[@intFromEnum(TokenType.LEFT_PAREN)] = ParseRule{ .prefix = Parser.grouping, .infix = Parser.call, .precedence = .CALL };
    _rules[@intFromEnum(TokenType.BANG)] = ParseRule{ .prefix = Parser.unary };
    _rules[@intFromEnum(TokenType.BANG_EQUAL)] = ParseRule{ .infix = Parser.binary, .precedence = .EQUALITY };
    _rules[@intFromEnum(TokenType.EQUAL_EQUAL)] = ParseRule{ .infix = Parser.binary, .precedence = .EQUALITY };
    _rules[@intFromEnum(TokenType.GREATER)] = ParseRule{ .infix = Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.GREATER_EQUAL)] = ParseRule{ .infix = Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.LESS)] = ParseRule{ .infix = Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.LESS_EQUAL)] = ParseRule{ .infix = Parser.binary, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.IDENTIFIER)] = ParseRule{ .prefix = Parser.variable, .precedence = .COMPARISON };
    _rules[@intFromEnum(TokenType.MINUS)] = ParseRule{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM };
    _rules[@intFromEnum(TokenType.PLUS)] = ParseRule{ .infix = Parser.binary, .precedence = .TERM };
    _rules[@intFromEnum(TokenType.SLASH)] = ParseRule{ .infix = Parser.binary, .precedence = .FACTOR };
    _rules[@intFromEnum(TokenType.STAR)] = ParseRule{ .infix = Parser.binary, .precedence = .FACTOR };
    _rules[@intFromEnum(TokenType.STRING)] = ParseRule{ .prefix = Parser.string };
    _rules[@intFromEnum(TokenType.NUMBER)] = ParseRule{ .prefix = Parser.number };
    _rules[@intFromEnum(TokenType.AND)] = ParseRule{ .infix = Parser.and_, .precedence = Precedence.AND };
    _rules[@intFromEnum(TokenType.OR)] = ParseRule{ .infix = Parser.or_, .precedence = Precedence.OR };
    _rules[@intFromEnum(TokenType.FALSE)] = ParseRule{ .prefix = Parser.literal };
    _rules[@intFromEnum(TokenType.TRUE)] = ParseRule{ .prefix = Parser.literal };
    _rules[@intFromEnum(TokenType.NIL)] = ParseRule{ .prefix = Parser.literal };

    break :blk _rules;
};

fn getRule(typ: TokenType) *const ParseRule {
    return &rules[@intFromEnum(typ)];
}

pub const Local = struct {
    name: Token,
    depth: ?usize,
    is_captured: bool,
};

pub const Upvalue = struct {
    index: u8,
    is_local: bool,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Function,
    type: FunctionType,

    locals: [zlox_common.U8_COUNT]Local,
    local_count: usize,
    scope_depth: usize,

    upvalues: [zlox_common.U8_COUNT]Upvalue,

    vm: *VM,
    parser: *Parser,

    pub fn init(vm: *VM, enclosing: ?*Compiler, typ: FunctionType) !Compiler {
        var compiler = Compiler{
            .local_count = 0,
            .scope_depth = 0,
            .locals = undefined,

            .upvalues = undefined,

            .function = undefined,
            .type = typ,

            .vm = vm,
            .enclosing = enclosing,

            // Set up when calling `compile()`
            .parser = undefined,
        };

        compiler.function = try Function.init(vm);

        const local = &compiler.locals[compiler.local_count];
        compiler.local_count += 1;

        local.depth = 0;
        local.name.start = "";
        local.name.length = 0;
        local.is_captured = false;

        return compiler;
    }

    // TODO: errors instead of optionals
    pub fn compile(self: *Compiler, source: []u8) !?*Function {
        const scanner = Scanner.init(source);
        var parser = Parser.init(self.vm, self, scanner);
        self.parser = &parser;

        try self.parser.parse();

        const function = try self.end();
        return if (self.parser.had_error) null else function;
    }

    pub fn end(self: *Compiler) !*Function {
        try self.parser.end();

        const function = self.function;
        if (zlox_common.DEBUG_PRINT_CODE and self.parser.had_error) {
            zlox_debug.disassembleChunk(self.parser.currentChunk(), if (self.function.name) |name| name.chars else "<script>");
        }

        return function;
    }
};

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    scanner: Scanner,
    current_compiler: ?*Compiler,
    vm: *VM,

    pub fn init(vm: *VM, compiler: *Compiler, scanner: Scanner) Parser {
        var parser = Parser{
            .had_error = false,
            .panic_mode = false,

            // These will get set up when we `advance()`
            .previous = undefined,
            .current = undefined,

            .vm = vm,
            .scanner = scanner,
            .current_compiler = compiler,
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
        } else if (self.match(.FUN)) {
            try self.funDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) self.synchronise();
    }

    fn funDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect function name.");
        self.markInitialised();
        try self.function(.FUNCTION);
        try self.defineVariable(global);
    }

    fn function(self: *Parser, typ: FunctionType) !void {
        var compiler = try Compiler.init(self.vm, self.current_compiler, typ);
        compiler.parser = self;
        self.current_compiler = &compiler;

        if (typ != .SCRIPT) {
            self.current_compiler.?.function.name = try String.copyInit(self.vm, self.previous.start, self.previous.length);
        }

        try self.beginScope();
        self.consume(.LEFT_PAREN, "Expect '(' after function name.");
        if (!self.check(.RIGHT_PAREN)) {
            var first_arg = true;
            while (first_arg or self.match(.COMMA)) {
                first_arg = false;

                self.current_compiler.?.function.arity += 1;
                if (self.current_compiler.?.function.arity > 255) {
                    self.errAtCurrent("Can't have more than 255 parameters.");
                }

                const constant = try self.parseVariable("Expect parameter name.");
                try self.defineVariable(constant);
            }
        }

        self.consume(.RIGHT_PAREN, "Expect ')' after function parameters.");
        self.consume(.LEFT_BRACE, "Expect '{' before function body.");
        try self.block();

        const fun = try self.current_compiler.?.end();
        try self.emitBytes(@intFromEnum(OpCode.CLOSURE), try self.makeConstant(Value{ .obj = &fun.obj }));

        for (0..fun.upvalue_count) |i| {
            try self.emitByte(if (compiler.upvalues[i].is_local) 1 else 0);
            try self.emitByte(compiler.upvalues[i].index);
        }
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
            try self.emitByte(@intFromEnum(OpCode.NIL));
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (self.current_compiler.?.scope_depth > 0) {
            self.markInitialised();
            return;
        }
        try self.emitBytes(@intFromEnum(OpCode.DEFINE_GLOBAL), global);
    }

    fn markInitialised(self: *Parser) void {
        if (self.current_compiler.?.scope_depth == 0) return;
        self.current_compiler.?.locals[self.current_compiler.?.local_count - 1].depth = self.current_compiler.?.scope_depth;
    }

    fn parseVariable(self: *Parser, error_message: []const u8) !u8 {
        self.consume(.IDENTIFIER, error_message);

        self.declareVariable();

        // Don't need name of locals in constants table - they're looked up by index in stack
        if (self.current_compiler.?.scope_depth > 0) return 0;

        return try self.identifierConstant(&self.previous);
    }

    fn identifierConstant(self: *Parser, name: *const Token) !u8 {
        const str = try String.copyInit(self.vm, name.start, name.length);
        return try self.makeConstant(Value{ .obj = &str.obj });
    }

    fn declareVariable(self: *Parser) void {
        // Skip global scope
        if (self.current_compiler.?.scope_depth == 0) return;

        const name = &self.previous;
        var i = self.current_compiler.?.local_count;
        while (i > 0) {
            i -= 1;

            const local = &self.current_compiler.?.locals[i];
            if (local.depth) |depth| {
                if (depth < self.current_compiler.?.scope_depth) break;
            }

            if (Token.identifiersEqual(name, &local.name)) {
                self.err("Already a variabel with this name in scope.");
            }
        }

        self.addLocal(name.*);
    }

    fn addLocal(self: *Parser, name: Token) void {
        if (self.current_compiler.?.local_count >= zlox_common.U8_COUNT) {
            self.err("Too many local variables in function.");
            return;
        }

        const local = &self.current_compiler.?.locals[self.current_compiler.?.local_count];
        self.current_compiler.?.local_count += 1;

        local.name = name;
        local.depth = null;
        local.is_captured = false;
    }

    fn and_(self: *Parser, _: bool) !void {
        // Check if previous value (left hand side of `and`) was false
        // Jump past the second value evaluation if it was false (we know the result will be false, so short-circuit)
        const end_jump = try self.emitJump(.JUMP_IF_FALSE);

        // Discard left hand side if true - the result will be the right hand side
        try self.emitByte(@intFromEnum(OpCode.POP));
        try self.parsePrecedence(.AND);

        try self.patchJump(end_jump);
    }

    fn or_(self: *Parser, _: bool) !void {
        // Check if previous value (left hand side of `or`) was false
        // If it's false, we continue to evaluate the right hand side and return that as the result
        // If it's true, we skip past the right hand side
        // Since we only have JUMP_IF_FALSE, we can convert it to JUMP_IF_TRUE by jumping past the next unconditional jump
        // NOTE: this is not the optimal way to implement this - just a demo of how we can use existing instructions
        const else_jump = try self.emitJump(.JUMP_IF_FALSE);
        const end_jump = try self.emitJump(.JUMP);

        try self.patchJump(else_jump);
        try self.emitByte(@intFromEnum(OpCode.POP));

        try self.parsePrecedence(.OR);
        try self.patchJump(end_jump);
    }

    fn statement(self: *Parser) !void {
        if (self.match(.PRINT)) {
            try self.printStatement();
        } else if (self.match(.LEFT_BRACE)) {
            try self.beginScope();
            try self.block();
            try self.endScope();
        } else if (self.match(.IF)) {
            try self.ifStatement();
        } else if (self.match(.WHILE)) {
            try self.whileStatement();
        } else if (self.match(.FOR)) {
            try self.forStatement();
        } else if (self.match(.RETURN)) {
            try self.returnStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn returnStatement(self: *Parser) !void {
        if (self.current_compiler.?.type == .SCRIPT) {
            self.err("Can't return from top level code");
        }

        if (self.match(.SEMICOLON)) {
            try self.emitReturn();
        } else {
            try self.expression();
            self.consume(.SEMICOLON, "Expect ';' after return value.");
            try self.emitByte(@intFromEnum(OpCode.RETURN));
        }
    }

    fn forStatement(self: *Parser) anyerror!void {
        try self.beginScope();
        self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");

        // Declaration
        if (self.match(.SEMICOLON)) {
            // No initialiser
        } else if (self.match(.VAR)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loop_start = self.currentChunk().code.items.len;
        var exit_jump: ?usize = null;

        // Condition
        if (!self.match(.SEMICOLON)) {
            try self.expression();
            self.consume(.SEMICOLON, "Expect ';' after loop condition");

            // Jump out of loop if condition is false
            exit_jump = try self.emitJump(.JUMP_IF_FALSE);

            // Pop the for condition
            try self.emitByte(@intFromEnum(OpCode.POP));
        }

        // Increment
        if (!self.match(.RIGHT_PAREN)) {
            // Jump to body (past increment), then jump back to increment
            // We do this because we can't compile this clause after the statement in a single-pass compiler
            const body_jump = try self.emitJump(.JUMP);
            const increment_start = self.currentChunk().code.items.len;

            try self.expression();

            try self.emitByte(@intFromEnum(OpCode.POP));
            self.consume(.RIGHT_PAREN, "Expect ')' after loop condition");

            // Go back to condition
            try self.emitLoop(loop_start);
            // Change loop start to where increment instruction begins
            loop_start = increment_start;
            // Jump past increment to body
            try self.patchJump(body_jump);
        }

        try self.statement();
        // Jump to either increment or start of loop
        try self.emitLoop(loop_start);

        // If we hve a condition clause, patch jump and pop the for condition
        if (exit_jump) |jump| {
            try self.patchJump(jump);

            // Pop the for condition
            try self.emitByte(@intFromEnum(OpCode.POP));
        }

        try self.endScope();
    }

    fn whileStatement(self: *Parser) anyerror!void {
        const loop_start = self.currentChunk().code.items.len;
        self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after condition");

        const exit_jump = try self.emitJump(.JUMP_IF_FALSE);

        // Pop the while condition at beginning of while body
        try self.emitByte(@intFromEnum(OpCode.POP));
        try self.statement();
        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);

        // Pop the while condition after the body
        try self.emitByte(@intFromEnum(OpCode.POP));
    }

    fn ifStatement(self: *Parser) anyerror!void {
        self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after condition");

        const then_jump = try self.emitJump(.JUMP_IF_FALSE);

        // Pop condition at beginning of `then` branch
        try self.emitByte(@intFromEnum(OpCode.POP));
        try self.statement();

        // Jump past the `else` statement if executing the `then` statement
        const else_jump = try self.emitJump(.JUMP);

        try self.patchJump(then_jump);

        // Pop condition at beginning of `else` branch (or straight after if statement if no else)
        try self.emitByte(@intFromEnum(OpCode.POP));

        if (self.match(.ELSE)) try self.statement();
        try self.patchJump(else_jump);
    }

    fn emitLoop(self: *Parser, loop_start: usize) !void {
        try self.emitByte(@intFromEnum(OpCode.LOOP));

        const offset = self.currentChunk().code.items.len -% loop_start +% 2;
        if (offset > std.math.maxInt(u16)) self.err("Loop body too large.");

        try self.emitByte(@intCast((offset >> 8) & 0xff));
        try self.emitByte(@intCast(offset & 0xff));
    }

    fn emitJump(self: *Parser, instruction: OpCode) !usize {
        try self.emitByte(@intFromEnum(instruction));
        try self.emitByte(0xff);
        try self.emitByte(0xff);

        return self.currentChunk().code.items.len - 2;
    }

    fn patchJump(self: *Parser, offset: usize) !void {
        // Called right before emitting next instruction, so current offset is jump location
        // -2 to adjust for the bytecode for the jump offset itself
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) self.err("Too much code to jump over.");

        self.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn block(self: *Parser) anyerror!void {
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            try self.declaration();
        }

        self.consume(.RIGHT_BRACE, "Expect '}' after block.");
    }

    fn beginScope(self: *Parser) !void {
        self.current_compiler.?.scope_depth += 1;
    }

    fn shouldPopFromScope(self: *Parser) bool {
        if (self.current_compiler.?.local_count == 0) return false;

        const local = self.current_compiler.?.locals[self.current_compiler.?.local_count - 1];
        if (local.depth) |depth| {
            return depth > self.current_compiler.?.scope_depth;
        }

        return false;
    }

    fn endScope(self: *Parser) !void {
        self.current_compiler.?.scope_depth -= 1;

        while (self.shouldPopFromScope()) {
            if (self.current_compiler.?.locals[self.current_compiler.?.local_count - 1].is_captured) {
                try self.emitByte(@intFromEnum(OpCode.CLOSE_UPVALUE));
            } else {
                try self.emitByte(@intFromEnum(OpCode.POP));
            }

            self.current_compiler.?.local_count -= 1;
        }
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        try self.emitByte(@intFromEnum(OpCode.PRINT));
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.SEMICOLON, "Expect ';' after expression.");
        try self.emitByte(@intFromEnum(OpCode.POP));
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
        try self.emitConstant(Value{ .number = value });
    }

    fn string(self: *Parser, _: bool) !void {
        const str = try String.copyInit(self.vm, self.previous.start + 1, self.previous.length - 2);
        try self.emitConstant(Value{ .obj = &str.obj });
    }

    fn variable(self: *Parser, can_assign: bool) !void {
        try self.namedVariable(self.previous, can_assign);
    }

    fn namedVariable(self: *Parser, name: Token, can_assign: bool) !void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;
        var arg = self.resolveLocal(self.current_compiler.?, &name);

        if (arg != -1) {
            get_op = .GET_LOCAL;
            set_op = .SET_LOCAL;
        } else {
            arg = self.resolveUpvalue(self.current_compiler.?, &name);

            if (arg != -1) {
                get_op = .GET_UPVALUE;
                set_op = .SET_UPVALUE;
            } else {
                arg = try self.identifierConstant(&name);
                get_op = .GET_GLOBAL;
                set_op = .SET_GLOBAL;
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            try self.expression();
            try self.emitBytes(@intFromEnum(set_op), @intCast(arg));
        } else {
            try self.emitBytes(@intFromEnum(get_op), @intCast(arg));
        }
    }

    // TODO: use null instead of -1
    fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: *const Token) isize {
        if (compiler.enclosing == null) return -1;

        const local = self.resolveLocal(compiler.enclosing.?, name);
        if (local != -1) {
            compiler.enclosing.?.locals[@intCast(local)].is_captured = true;
            return self.addUpvalue(compiler, @intCast(local), true);
        }

        // Go up through enclosing compilers until an actual local variable is found
        // Then unwind by creating a chain upvalues that eventually point to that local variable
        const upvalue = self.resolveUpvalue(compiler.enclosing.?, name);
        if (upvalue != -1) {
            return self.addUpvalue(compiler, @intCast(upvalue), false);
        }

        return -1;
    }

    // TODO: use null instead of -1
    fn addUpvalue(self: *Parser, compiler: *Compiler, index: u8, is_local: bool) isize {
        const upvalue_count = compiler.function.upvalue_count;

        for (0..upvalue_count) |i| {
            const upvalue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }

        if (upvalue_count >= zlox_common.U8_COUNT) {
            self.err("Too many closure variables in funciton.");
            return 0;
        }

        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = index;

        compiler.function.upvalue_count += 1;
        return @intCast(upvalue_count);
    }

    // TODO: use null instead of -1
    fn resolveLocal(self: *Parser, compiler: *Compiler, name: *const Token) isize {
        var i = compiler.local_count;
        while (i > 0) {
            i -= 1;

            const local = &compiler.locals[i];
            if (Token.identifiersEqual(name, &local.name)) {
                if (local.depth == null) {
                    self.err("Can't read local variable in its own initialiser.");
                }
                return @intCast(i);
            }
        }

        return -1;
    }

    fn unary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;

        // Compile the operand
        try self.parsePrecedence(.UNARY);

        // Emit the op instruction
        switch (op_type) {
            .MINUS => try self.emitByte(@intFromEnum(OpCode.NEGATE)),
            .BANG => try self.emitByte(@intFromEnum(OpCode.NOT)),
            else => unreachable,
        }
    }

    fn binary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;
        const rule = getRule(op_type);

        // +1 to make it left associative
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (op_type) {
            .BANG_EQUAL => try self.emitBytes(@intFromEnum(OpCode.EQUAL), @intFromEnum(OpCode.NOT)),
            .EQUAL_EQUAL => try self.emitByte(@intFromEnum(OpCode.EQUAL)),
            .GREATER => try self.emitByte(@intFromEnum(OpCode.GREATER)),
            .GREATER_EQUAL => try self.emitBytes(@intFromEnum(OpCode.LESS), @intFromEnum(OpCode.NOT)),
            .LESS => try self.emitByte(@intFromEnum(OpCode.LESS)),
            .LESS_EQUAL => try self.emitBytes(@intFromEnum(OpCode.GREATER), @intFromEnum(OpCode.NOT)),
            .PLUS => try self.emitByte(@intFromEnum(OpCode.ADD)),
            .MINUS => try self.emitByte(@intFromEnum(OpCode.SUBTRACT)),
            .STAR => try self.emitByte(@intFromEnum(OpCode.MULTIPLY)),
            .SLASH => try self.emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => unreachable,
        }
    }

    fn call(self: *Parser, _: bool) !void {
        const arg_count = try self.argumentList();
        try self.emitBytes(@intFromEnum(OpCode.CALL), arg_count);
    }

    fn argumentList(self: *Parser) !u8 {
        var arg_count: u8 = 0;
        if (!self.check(.RIGHT_PAREN)) {
            var first_arg = true;
            while (first_arg or self.match(.COMMA)) {
                first_arg = false;

                try self.expression();
                arg_count += 1;
            }
        }

        self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");
        return arg_count;
    }

    fn literal(self: *Parser, _: bool) !void {
        switch (self.previous.type) {
            .FALSE => try self.emitByte(@intFromEnum(OpCode.FALSE)),
            .TRUE => try self.emitByte(@intFromEnum(OpCode.TRUE)),
            .NIL => try self.emitByte(@intFromEnum(OpCode.NIL)),
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

            if (getRule(self.previous.type).infix) |infix_rule| {
                try infix_rule(self, can_assign);
            } else {
                self.err("Unexpected token.");
                return;
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
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

    fn end(self: *Parser) !void {
        try self.emitReturn();
        self.current_compiler = self.current_compiler.?.enclosing;
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitBytes(@intFromEnum(OpCode.CONSTANT), try self.makeConstant(value));
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitByte(@intFromEnum(OpCode.NIL));
        try self.emitByte(@intFromEnum(OpCode.RETURN));
    }

    fn currentChunk(self: *Parser) *Chunk {
        return &self.current_compiler.?.function.chunk;
    }

    fn emitBytes(self: *Parser, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().write(byte, self.previous.line);
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        // NOTE: push and pop are for GC
        self.vm.push(value);
        const constant = try self.currentChunk().addConstant(value);
        _ = self.vm.pop();

        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constant);
    }
};
