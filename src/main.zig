const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();

    const path: []const u8 = args.next() orelse return;
    const text: []const u8 = std.fs.cwd().readFileAlloc(alloc, path, 1 << 32) catch |err| {
        std.log.err("failed to open file '{s}': {}", .{ path, err });
        return;
    };

    const tokens = switch(try lexer.tokenize(text)) {
        .ok => |tokens| tokens,
        .err => |invalid| {
            std.debug.panic("invalid token error at index {}:\n  {s}", .{ invalid.index, invalid.msg });
        }
    };

    for (tokens.items) |token| {
        std.debug.print("{}\n", .{token});
    }

    var lex = lexer.Lexer.init(tokens);
    const expr = try parser.parse_expr(&lex);

    std.debug.print("{s}\n", .{text});
    std.debug.print("{}\n", .{expr});
}
