const std = @import("std");

const WhitespaceError = error{UnterminatedMultilineComment};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

pub const Scanner = struct {
    start: [*]u8,
    current: [*]u8,
    end: [*]u8,
    line: usize,

    pub fn init(source: []u8) Scanner {
        return Scanner{
            .start = source.ptr,
            .current = source.ptr,
            .end = source.ptr + source.len,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace() catch |err| switch (err) {
            WhitespaceError.UnterminatedMultilineComment => return self.errorToken("Unterminated multi-line comment."),
        };

        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(TokenType.EOF);

        const c = self.advance();
        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(TokenType.LEFT_PAREN),
            ')' => self.makeToken(TokenType.RIGHT_PAREN),
            '{' => self.makeToken(TokenType.LEFT_BRACE),
            '}' => self.makeToken(TokenType.RIGHT_BRACE),
            ';' => self.makeToken(TokenType.SEMICOLON),
            ',' => self.makeToken(TokenType.COMMA),
            '.' => self.makeToken(TokenType.DOT),
            '-' => self.makeToken(TokenType.MINUS),
            '+' => self.makeToken(TokenType.PLUS),
            '/' => self.makeToken(TokenType.SLASH),
            '*' => self.makeToken(TokenType.STAR),
            '!' => self.makeToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => self.makeToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '<' => self.makeToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
            '>' => self.makeToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '"' => self.string(),
            else => self.errorToken("Unexpected character."),
        };
    }

    fn identifier(self: *Scanner) Token {
        while (isDigit(self.peek()) or isAlpha(self.peek())) _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        switch (self.start[0]) {
            'a' => return self.checkKeyword(1, "nd", TokenType.AND),
            'c' => return self.checkKeyword(1, "lass", TokenType.CLASS),
            'e' => return self.checkKeyword(1, "lse", TokenType.ELSE),
            'f' => {
                if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                    switch (self.start[1]) {
                        'a' => return self.checkKeyword(2, "lse", TokenType.FALSE),
                        'o' => return self.checkKeyword(2, "r", TokenType.FOR),
                        'u' => return self.checkKeyword(2, "n", TokenType.FUN),
                        else => {},
                    }
                }
            },
            'i' => return self.checkKeyword(1, "f", TokenType.IF),
            'n' => return self.checkKeyword(1, "il", TokenType.NIL),
            'o' => return self.checkKeyword(1, "r", TokenType.OR),
            'p' => return self.checkKeyword(1, "rint", TokenType.PRINT),
            'r' => return self.checkKeyword(1, "eturn", TokenType.RETURN),
            's' => return self.checkKeyword(1, "uper", TokenType.SUPER),
            't' => {
                if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                    switch (self.start[1]) {
                        'h' => return self.checkKeyword(2, "is", TokenType.THIS),
                        'r' => return self.checkKeyword(2, "ue", TokenType.TRUE),
                        else => {},
                    }
                }
            },
            'v' => return self.checkKeyword(1, "ar", TokenType.VAR),
            'w' => return self.checkKeyword(1, "hile", TokenType.WHILE),
            else => {},
        }

        return TokenType.IDENTIFIER;
    }

    fn checkKeyword(self: *Scanner, start: usize, rest: []const u8, typ: TokenType) TokenType {
        if (std.mem.eql(u8, self.start[start .. start + rest.len], rest)) {
            return typ;
        }

        return TokenType.IDENTIFIER;
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the '.'
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(TokenType.NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // Consume closing quote
        _ = self.advance();
        return self.makeToken(TokenType.STRING);
    }

    fn skipWhitespace(self: *Scanner) !void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    } else if (self.peekNext() == '*') {
                        while (!(self.peek() == '*' and self.peekNext() == '/') and !self.isAtEnd()) _ = self.advance();
                        if (self.isAtEnd()) return WhitespaceError.UnterminatedMultilineComment;

                        _ = self.advance();
                        _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.current[1];
    }

    fn peek(self: *Scanner) u8 {
        return self.current[0];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;
        self.current += 1;
        return true;
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return (self.current - 1)[0];
    }

    fn isAtEnd(self: *Scanner) bool {
        return @intFromPtr(self.current) >= @intFromPtr(self.end);
    }

    fn makeToken(self: *Scanner, typ: TokenType) Token {
        return Token{
            .type = typ,
            .start = self.start,
            .length = @intFromPtr(self.current) - @intFromPtr(self.start),
            .line = self.line,
        };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{
            .type = TokenType.ERROR,
            .start = message.ptr,
            .length = message.len,
            .line = self.line,
        };
    }
};

pub const Token = struct {
    type: TokenType,
    start: [*]const u8,
    length: usize,
    line: usize,
};

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};
