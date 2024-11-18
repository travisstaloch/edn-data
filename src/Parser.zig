const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const edn = @import("root.zig");

src: []const u8,
index: u32,
values_start: [*]edn.Value,
values: [*]edn.Value,
ws_start: [*][2]edn.Token,
ws: [*][2]edn.Token,
options: edn.Options,
depth: u8, // only used when logging
measured: edn.Measured = .{},
handlers: edn.TaggedElementHandler.Map,

const Parser = @This();

pub fn init(
    src: []const u8,
    options: edn.Options,
    values_start: [*]edn.Value,
    values: [*]edn.Value,
    ws_start: [*][2]edn.Token,
    ws: [*][2]edn.Token,
    comptime handlers: []const edn.TaggedElementHandler.Data,
) Parser {
    return .{
        .src = src,
        .index = 0,
        .depth = 0,
        .options = options,
        .values_start = values_start,
        .values = values,
        .ws_start = ws_start,
        .ws = ws,
        .handlers = edn.TaggedElementHandler.Map.initComptime(handlers),
    };
}

pub fn isIntOrFloatDigit(c: u8) bool {
    return std.ascii.isDigit(c) or switch (c) {
        '.', '-', 'e', 'E' => true,
        else => false,
    };
}

pub fn eos(p: *const Parser) bool {
    return p.index >= p.src.len;
}

pub fn peek(p: *const Parser, offset: usize) ?u8 {
    return if (p.index + offset < p.src.len)
        p.src[p.index + offset]
    else
        null;
}

pub fn next(p: *Parser) ?u8 {
    if (p.index >= p.src.len) return null;
    defer p.index += 1;
    return p.src[p.index];
}

fn isWhitespace(c: u8) bool {
    return std.ascii.isWhitespace(c) or c == ',';
}

pub fn skipWsAndComments(p: *Parser) void {
    while (true) {
        const this_start = p.index;
        p.skipWhileFn(isWhitespace);
        if (p.peek(0) == ';') p.skipUntilChar('\n');
        if (p.index == this_start) break;
    }
}

pub fn skipUntilWs(p: *Parser) void {
    return p.skipUntilFn(isWhitespace);
}

pub fn skipWhileFn(p: *Parser, f: *const fn (u8) bool) void {
    while (p.peek(0)) |c| {
        if (!f(c)) break;
        p.index += 1;
    }
}

pub fn skipUntilChar(p: *Parser, char: u8) void {
    while (p.peek(0)) |c| {
        if (c == char) break;
        p.index += 1;
    }
}

pub fn skipUntilFn(p: *Parser, f: *const fn (u8) bool) void {
    while (p.peek(0)) |c| {
        if (f(c)) break;
        p.index += 1;
    }
}
