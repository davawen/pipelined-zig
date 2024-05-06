const std = @import("std");

const mem = std.mem;
const Allocator = mem.Allocator;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const Lexer = lexer.Lexer;

pub const Ident = []const u8;
pub const Literal = []const u8;

pub fn printIdented(allocator: Allocator, ident: u8, writer: anytype, comptime fmt: []const u8, args: anytype) !void {
    const s = try std.fmt.allocPrint(allocator, fmt, args);
    defer allocator.free(s);

    var iter = std.mem.split(u8, s, "\n");
    var next = iter.next();
    while (next) |line| {
        try writer.print("\x1b[90m|\x1b[0m", .{});
        for (1..ident) |_| try writer.print(" ", .{});

        try writer.print("{s}", .{line});

        next = iter.next();
        if (next) |_| {
            try writer.print("\n", .{});
        }
    }
}

pub const Func = struct {
    args: std.ArrayList(Ident),
    body: *Expr,

    pub fn format(self: Func, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("(", .{});
        for (self.args.items, 0..) |arg, index| {
            try writer.print("{s}", .{arg});
            if (index < self.args.items.len-1) try writer.print(" ", .{});
        }
        try writer.print(") =>\n", .{});

        try printIdented(alloc, 4, writer, "{}", .{ self.body });
    }
};

pub const Tuple = std.ArrayList(Expr);

pub const Value = union(enum) {
    func: Func,
    tuple: Tuple,
    ident: Ident,
    lit: Literal,
    num: i64,
    underscore,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .func => |f| try writer.print("{}", .{ f }),
            .tuple => |t| {
                try writer.print("tuple: {{\n", .{});
                for (t.items) |item| {
                    try printIdented(alloc, 4, writer, "{},", .{ item });
                    try writer.print("\n", .{});
                }
                try writer.print("}}", .{});
            },
            .ident => |s| try writer.print("ident: {s}", .{ s }),
            .lit => |s| try writer.print("lit: \"{s}\"", .{ s }),
            .num => |n| try writer.print("num: {d}", .{ n }),
            .underscore => try writer.print("_", .{})
        }
    }
};

pub const Expr = union(enum) {
    do: struct { *Expr, *Expr },
    pipeline: struct { *Expr, Ident },
    assign: struct { *Expr, Ident },
    mutate: struct { *Expr, Ident },
    value: Value,

    pub fn format(self: Expr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .do => |v| {
                try writer.print("do:\n", .{});
                try printIdented(alloc, 4, writer, "{}", .{ v[0] });
                try writer.print("\n", .{});
                try printIdented(alloc, 4, writer, "{}", .{ v[1] });
            },
            .pipeline => |v| {
                try writer.print("pipeline into {s}:\n", .{ v[1] });
                try printIdented(alloc, 4, writer, "{}", .{ v[0] });
            },
            .assign => |v| {
                try writer.print("assign into {s}:\n", .{ v[1] });
                try printIdented(alloc, 4, writer, "{}", .{ v[0] });
            },
            .mutate => |v| {
                try writer.print("mutate into {s}:\n", .{ v[1] });
                try printIdented(alloc, 4, writer, "{}", .{ v[0] });
            },
            .value => |v| try writer.print("{}", .{ v }),
        }
    }
};

fn box(comptime T: type, obj: T) !*T {
    const p = try alloc.create(T);
    p.* = obj;
    return p;
} 

fn parse_tuple(tokens: *Lexer) Allocator.Error!Tuple {
    var out = std.ArrayList(Expr).init(alloc);
    while (tokens.peek() != .rbrace) {
        try out.append(try parse_expr(tokens));
    }
    _ = tokens.expect(Token.rbrace);

    return out;
}

fn parse_func(tokens: *Lexer) Allocator.Error!Func {
    var args = std.ArrayList(Ident).init(alloc);
    var next = tokens.next();
    while (next != .rparen) {
        switch (next) {
            .eof => std.debug.panic("expected a function argument, found EOF", .{}),
            .ident => |s| try args.append(s),
            else => std.debug.panic("expected a function argument, found {s}", .{ next })
        }

        next = tokens.next();
    }

    _ = tokens.expect(Token.lambda);
    const body = try box(Expr, try parse_expr(tokens));
    return Func { .args = args, .body = body };
}

fn parse_value(tokens: *Lexer) !Value {
    const value = switch (tokens.next()) {
        .num => |n| Value { .num = n },
        .lit => |s| Value { .lit = s },
        .ident => |s| Value { .ident = s },
        .underscore => Value.underscore,
        .lbrace => Value { .tuple = try parse_tuple(tokens) },
        .lparen => Value { .func = try parse_func(tokens) },
        else => |x| std.debug.panic("unexpected token {s}", .{ x })
    };

    return value;
}

pub fn parse_expr(tokens: *Lexer) !Expr {
    var tree = Expr { .value = try parse_value(tokens) };
    while (true) {
        switch (tokens.peek()) {
            .pipeline => {
                _ = tokens.next();
                const next = tokens.expect(Token.ident).ident;
                tree = Expr { .pipeline = .{ try box(Expr, tree), next } };
            },
            .assign => {
                _ = tokens.next();
                const next = tokens.expect(Token.ident).ident;
                tree = Expr { .assign = .{ try box(Expr, tree), next } };
            },
            .mutate => {
                _ = tokens.next();
                const next = tokens.expect(Token.ident).ident;
                tree = Expr { .mutate = .{ try box(Expr, tree), next } };
            },
            .semicolon => {
                _ = tokens.next();
                const next = try parse_expr(tokens);
                tree = Expr { .do = .{ try box(Expr, tree), try box(Expr, next) } };
            },
            else => break
        }
    }

    return tree;
}

