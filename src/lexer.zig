const std = @import("std");
const mem = std.mem;

pub const InvalidToken = struct {
    index: usize,
    msg: []const u8
};

pub fn Result(comptime T: type, comptime E: type) type {
    return union(enum) {
        const Self = @This();

        ok: T,
        err: E,

        pub fn unwrap(self: Self) T {
            switch (self) {
                .ok => |ok| { return ok; },
                .err => |e| {
                    std.debug.panic("called unwrap with error value: {s}", e);
                }
            }
        }
    };
}

pub const Token = union(enum) {
    const Tag = @typeInfo(@This()).Union.tag_type.?;

    eof,
    lparen,
    rparen,
    lbrace,
    rbrace,
    semicolon,
    lambda,
    assign,
    mutate,
    pipeline,
    underscore,
    num: i64,
    ident: []const u8,
    lit: []const u8,

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{ @tagName(self) });
        switch (self) {
            .num => |n| try writer.print(": {d}", .{n}),
            .ident => |s| try writer.print(": {s}", .{s}),
            .lit => |s| try writer.print(": \"{s}\"", .{s}),
            else => {}
        }
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn expect(text: []const u8, index: *usize, expected: u8) !?InvalidToken {
    index.* += 1;
    if (index.* >= text.len) {
        const msg = try std.fmt.allocPrint(allocator, "expected '{c}' after '{c}', got EOF\n", .{ expected, text[index.*-1] });
        return InvalidToken { .index = index.*, .msg = msg };
    } else if (text[index.*] != expected) {
        const msg = try std.fmt.allocPrint(allocator, "expected '{c}' after '{c}', got '{c}'", .{ expected, text[index.*-1], text[index.*] });
        return InvalidToken { .index = index.*, .msg = msg };
    }
    return null;
}

fn next(text: []const u8, index: *usize) ?u8 {
    index.* += 1;
    if (index.* >= text.len) return null;
    return text[index.*];
}

fn lex_ident(ident: []const u8) Token {
    const n = std.fmt.parseInt(i64, ident, 0) catch {
        return Token { .ident = ident };
    };

    return Token { .num = n };
}

pub fn tokenize(text: []const u8) !Result(std.ArrayList(Token), InvalidToken) {
    const Return = Result(std.ArrayList(Token), InvalidToken);

    var list = std.ArrayList(Token).init(allocator);

    var start: usize = 0;
    var index: usize = 0;
    while (index < text.len) {
        const c = text[index];

        if (std.ascii.isAlphanumeric(c) or c == '_') {}
        else {
            if (index > start) {
                try list.append(lex_ident(text[start..index]));
            } 
            const opt_token: ?Token = switch (c) {
                ' ' => null,
                '(' => Token.lparen,
                ')' => Token.rparen,
                '{' => Token.lbrace,
                '}' => Token.rbrace,
                ';' => Token.semicolon,
                '_' => Token.underscore,
                '=' => blk: {
                    if (try expect(text, &index, '>')) |err| return Return { .err = err };
                    break :blk Token.lambda;
                },
                '-' => blk: { 
                    const n = next(text, &index) orelse return Return { .err = .{ .index = index, .msg = "expected '-' or '>', got EOF" } };
                    if (n == '>') {
                        break :blk Token.assign;
                    } else if (n == '-') {
                        if (try expect(text, &index, '>')) |err| return Return { .err = err };
                        break :blk Token.mutate;
                    } else {
                        return Return { .err = .{ .index = index, .msg = try std.fmt.allocPrint(allocator, "expected '-' or '>', got {c}", .{ n }) } };
                    }
                },
                '|' => blk: { 
                    if (try expect(text, &index, '>')) |err| return Return { .err = err };
                    break :blk Token.pipeline;
                },
                '"' => blk: {
                    index += 1;
                    const literal_start = index;
                    while (index < text.len and text[index] != '"') {
                        index += 1;
                    }
                    if (index >= text.len) return Return { .err = .{ .index = index, .msg = "expected '\"', got EOF" } };

                    break :blk Token { .lit = text[literal_start..index] };
                },
                else => {
                    return Return { .err = .{ .index = index, .msg = "invalid character encountered"} };
                }
            };

            if (opt_token) |token| {
                try list.append(token);
            }

            start = index + 1;
        }
        index += 1;
    }

    if (index > start) {
        try list.append(lex_ident(text[start..index]));
    } 

    return Return { .ok = list };
}

pub const Lexer = struct {
    const Self = @This();

    tokens: std.ArrayList(Token),

    pub fn init(tokens: std.ArrayList(Token)) Self {
        std.mem.reverse(Token, tokens.items);

        return Self { .tokens = tokens };
    }

    /// Gets the next token from the lexer, removing it
    pub fn next(self: *Self) Token {
        return self.tokens.popOrNull() orelse Token.eof;
    }

    /// Gets the next token from the lexer, without removing it
    pub fn peek(self: *const Self) Token {
        return self.tokens.getLastOrNull() orelse Token.eof;
    }

    /// Removes the next token from the lexer, and checks its the right kind of token, otherwise panic
    pub fn expect(self: *Self, comptime expected: Token.Tag) Token {
        const n = self.next();
        if (n != expected) std.debug.panic("expected token {s}, got {s}", .{@tagName(expected), n});
        return n;
    }

    /// Checks that the next token is of the right kind without removing it, otherwise panic
    pub fn expect_peek(self: *Self, comptime expected: Token.Tag) Token {
        const n = self.peek();
        if (n != expected) std.debug.panic("expected token {s}, got {s}", .{@tagName(expected), n});
        return n;
    }
};
