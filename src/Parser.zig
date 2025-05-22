tokenizer: Tokenizer,
values: std.ArrayListUnmanaged(edn.Value),
whitespaces: std.ArrayListUnmanaged([2]u32),
// TODO use dynamic bitsets + shorter array list whichs to avoid storing nulls. i.e. SparseArrayHashMap
first_child_ids: std.ArrayListUnmanaged(edn.ValueId),
next_sibling_ids: std.ArrayListUnmanaged(edn.ValueId),
depth: u8, // only used when logging
handlers: edn.TaggedElementHandler.Map,
/// the last token seen.  used to track required whitespace between tokens.
last_token: edn.Token = .{
    .tag = undefined,
    .loc = .{ .ws_start = 0, .start = 0, .end = std.math.maxInt(u32) },
},

const Parser = @This();

pub fn init(
    src: [:0]const u8,
    comptime handlers: []const edn.TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .values = .{},
        .whitespaces = .{},
        .first_child_ids = .{},
        .next_sibling_ids = .{},
        .handlers = .initComptime(handlers),
    };
}

pub fn initFixed(
    src: [:0]const u8,
    values: []edn.Value,
    whitespaces: [][2]u32,
    first_child_ids: []edn.ValueId,
    next_sibling_ids: []edn.ValueId,
    comptime handlers: []const edn.TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .values = .initBuffer(values),
        .whitespaces = .initBuffer(whitespaces),
        .first_child_ids = .initBuffer(first_child_ids),
        .next_sibling_ids = .initBuffer(next_sibling_ids),
        .handlers = .initComptime(handlers),
    };
}

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const edn = @import("root.zig");
const Tokenizer = @import("Tokenizer.zig");
