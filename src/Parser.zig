tokenizer: Tokenizer,
values_start: [*]edn.Value,
values: [*]edn.Value,
/// whitespace locations start
wss_start: [*][2]u32,
/// whitespace locations
wss: [*][2]u32,
options: edn.Options,
depth: u8, // only used when logging
measured: edn.Measured = .{},
handlers: edn.TaggedElementHandler.Map,
/// the last token seen.  used to track required whitespace between tokens.
last_token: edn.Token = .{
    .tag = undefined,
    .loc = .{ .ws_start = 0, .start = 0, .end = std.math.maxInt(u32) },
},

const Parser = @This();

pub fn init(
    src: [:0]const u8,
    options: edn.Options,
    values_start: [*]edn.Value,
    values: [*]edn.Value,
    wss_start: [*][2]u32,
    wss: [*][2]u32,
    comptime handlers: []const edn.TaggedElementHandler.Data,
) Parser {
    return .{
        .tokenizer = .init(src),
        .depth = 0,
        .options = options,
        .values_start = values_start,
        .values = values,
        .wss_start = wss_start,
        .wss = wss,
        .handlers = edn.TaggedElementHandler.Map.initComptime(handlers),
    };
}

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const edn = @import("root.zig");
const Tokenizer = @import("Tokenizer.zig");
