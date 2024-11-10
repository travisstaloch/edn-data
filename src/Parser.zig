const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const edn = @import("root.zig");

src: []const u8,
index: u32,
alloc: Allocator,
values: std.ArrayListUnmanaged(edn.Value) = .{},
top_level_values: edn.ValueIndexList = .{},
depth: u8, // only used when logging

const Parser = @This();

pub fn init(alloc: Allocator, src: []const u8) Parser {
    return .{
        .src = src,
        .index = 0,
        .alloc = alloc,
        .depth = 0,
    };
}

pub fn deinit(p: *Parser) void {
    for (p.top_level_values.items) |vi| {
        p.values.items[vi.index].deinit(p.alloc, p.values.items);
    }
    p.values.deinit(p.alloc);
    p.top_level_values.deinit(p.alloc);
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

fn tagEnd(tag: edn.Value.Tag) u8 {
    return switch (tag) {
        .list => ')',
        .vector => ']',
        .map, .set => '}',
        else => unreachable,
    };
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
