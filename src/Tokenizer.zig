const Tokenizer = @This();

src: [:0]const u8,
index: u32 = 0,
lookbehind: RingBuffer = .init, // TODO remove lookbehind? i think this is only being used for peek.

const lookbehind_len = 8;

const RingBuffer = @import("ringbuffer.zig").RingBuffer(Token, 3);

comptime {
    // assert(2 ^ @bitSizeOf(std.meta.FieldType(Tokenizer, .lookbehind_idx)) == lookbehind_len);
    assert(std.meta.FieldType(RingBuffer, .buf) == [lookbehind_len]Token);
}

pub const Token = struct {
    tag: Tag,
    loc: Loc,
    pub const Loc = struct {
        ws_start: u32,
        start: u32,
        end: u32,

        pub const zero: Loc = .{ .ws_start = 0, .start = 0, .end = 0 };
        pub fn src(loc: Loc, s: [:0]const u8) []const u8 {
            return s[loc.start..loc.end];
        }
        pub fn ws(loc: Loc, s: [:0]const u8) []const u8 {
            return s[loc.ws_start..loc.start];
        }
    };

    pub const Tag = enum(u8) {
        // content tags. these require leading whitespace.
        symbol,
        int,
        float,
        keyword, // ':'
        nil, // 'n'
        true, // 't'
        false, // 'f'
        str, // '"'
        char, // '\\'
        tagged, // '#'
        // opening tags - don't require trailing whitespace
        lparen, // '('
        lcurly, // '{'
        lbracket, // '['
        set, // "#{"
        discard, // "#_"
        // closing tags - don't require leading whitespace
        rparen, // ')'
        rbracket, // ']'
        rcurly, // '}'
        eof, // 0
        invalid,

        pub fn lexeme(t: Tag) []const u8 {
            return switch (t) {
                .symbol => "symbol",
                .int => "int",
                .float => "float",
                .keyword => "keyword",
                .nil => "nil",
                .true => "true",
                .false => "false",
                .str => "\"",
                .char => "\\",
                .tagged => "#",
                .lparen => "(",
                .lcurly => "{",
                .lbracket => "[",
                .set => "#{",
                .discard => "#_",
                .rparen => ")",
                .rbracket => "]",
                .rcurly => "}",
                .eof => "0",
                .invalid => "invalid",
            };
        }

        pub fn isOpening(tag: Tag) bool {
            return @intFromEnum(tag) >= @intFromEnum(Tag.lparen) and @intFromEnum(tag) < @intFromEnum(Tag.rparen);
        }

        pub fn isClosing(tag: Tag) bool {
            return @intFromEnum(tag) >= @intFromEnum(Tag.rparen);
        }
    };

    pub fn src(t: Token, s: [:0]const u8) []const u8 {
        return t.loc.src(s);
    }
};

pub fn init(src: [:0]const u8) !Tokenizer {
    if (!std.unicode.utf8ValidateSlice(src)) return error.InvalidUTF8;
    var self: Tokenizer = .{ .src = src };
    for (0..lookbehind_len) |_| self.lookbehind.push(self.nextInner()) catch unreachable;
    return self;
}

pub fn next(self: *Tokenizer) Token {
    assert(self.lookbehind.len() == lookbehind_len);
    const tok = self.lookbehind.pop().?;
    self.lookbehind.push(self.nextInner()) catch unreachable;
    return tok;
}

fn combinedEnum(tag_type: type, E1: type, E2: type) type {
    const e1info = @typeInfo(E1).@"enum";
    const e2info = @typeInfo(E2).@"enum";
    var fields: [e1info.fields.len + e2info.fields.len]std.builtin.Type.EnumField = undefined;
    for (e1info.fields ++ e2info.fields, 0..) |f, i| {
        fields[i] = .{ .name = f.name, .value = i };
    }
    return @Type(.{ .@"enum" = .{
        .fields = &fields,
        .tag_type = tag_type,
        .decls = &.{},
        .is_exhaustive = true,
    } });
}

fn srcAt(self: *Tokenizer, index: u32) u8 {
    const i = self.index + index;
    // TODO maybe remove branching if it doesn't complicate other code too much
    return if (i <= self.src.len) self.src[i] else 0;
}

const State = combinedEnum(u8, Token.Tag, enum {
    start,
    symbol_slash,
    symbol_end,
    keyword_slash,
    str_escape, // '\\'
    char_u, // 'u'
    number,
    number_dot,
    float_e,
    float_e_digit,
    float_e_sign,
    ws, // ' '
    comment, // ';'
    comment_newline,
});

/// transition to state.
/// set token.tag (and token.loc.start) to `state` when its a valid tag and differs from token.tag.
/// returns comptime known state for continue.
fn tx(self: *Tokenizer, comptime state: State, inc: u8, token: *Token) State {
    if (@intFromEnum(state) < @intFromEnum(State.start)) {
        const t: Token.Tag = @enumFromInt(@intFromEnum(state));
        if (token.tag != t) {
            token.loc.start = self.index;
            token.tag = t;
        }
    }
    self.index += inc;
    return state;
}

/// transition to state with substitute tag.
/// set token.tag to `tag` (and token.loc.start) when it differs from token.tag.
/// returns comptime known state for continue.
fn tx1(self: *Tokenizer, comptime state: State, inc: u8, tag: Token.Tag, token: *Token) State {
    if (token.tag != tag) {
        token.tag = tag;
        token.loc.start = self.index;
    }
    self.index += inc;
    return state;
}

fn nextInner(self: *Tokenizer) Token {
    const start = @min(self.src.len, self.index);
    var r: Token = .{ .tag = .invalid, .loc = .{
        .ws_start = start,
        .start = start,
        .end = start,
    } };

    state: switch (State.start) { // zig fmt: on
        // inline else => |state| {
        // // std.debug.print("{s} '{s}'\n", .{@tagName(state), token.src(self.src)});
        // switch(state) {
        .start => switch (self.srcAt(0)) {
            0 => r.tag = .eof,
            ' ', '\t', '\r', '\n', ',' => continue :state self.tx(.ws, 1, &r),
            '(' => continue :state self.tx(.lparen, 1, &r),
            ')' => continue :state self.tx(.rparen, 1, &r),
            '[' => continue :state self.tx(.lbracket, 1, &r),
            ']' => continue :state self.tx(.rbracket, 1, &r),
            '{' => continue :state self.tx(.lcurly, 1, &r),
            '}' => continue :state self.tx(.rcurly, 1, &r),
            '#' => switch (self.srcAt(1)) {
                0, ' ', '\t', '\r', '\n', ',' => _ = self.tx(.invalid, 1, &r),
                '#', ':' => continue :state self.tx(.invalid, 1, &r),
                '{' => _ = self.tx(.set, 2, &r),
                '_' => _ = self.tx(.discard, 2, &r),
                else => continue :state self.tx1(.symbol, 1, .tagged, &r),
            },
            ';' => continue :state self.tx(.comment, 1, &r),
            '"' => continue :state self.tx(.str, 1, &r),
            '\\' => if (self.srcAt(1) == 'u')
                continue :state self.tx1(.char_u, 2, .char, &r)
            else
                continue :state self.tx(.char, 1, &r),
            ':' => switch (self.srcAt(1)) {
                ':', '/', '#' => continue :state self.tx(.invalid, 2, &r),
                else => continue :state self.tx(.keyword, 1, &r),
            },
            '0' => switch (self.srcAt(1)) {
                0, ' ', '\t', '\r', '\n', ',', ')', ']', '}' => _ = self.tx(.int, 1, &r),
                '0'...'9' => continue :state self.tx(.invalid, 2, &r),
                'e', 'E' => {
                    continue :state self.tx1(.float_e, 2, .float, &r);
                },
                '.' => continue :state self.tx(.float, 2, &r),
                else => continue :state self.tx(.invalid, 1, &r),
            },
            '1'...'9' => continue :state self.tx1(.number, 1, .int, &r),
            '-', '+' => switch (self.srcAt(1)) {
                // number if followed by a digit
                '0'...'9' => continue :state self.tx1(.number, 1, .int, &r),
                else => continue :state self.tx(.symbol, 1, &r),
            },
            '.' => if (std.ascii.isDigit(self.srcAt(1)))
                continue :state self.tx(.float, 1, &r)
            else
                continue :state self.tx(.symbol, 1, &r),
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>' => {
                continue :state self.tx(.symbol, 1, &r);
            },
            else => continue :state self.tx(.invalid, 1, &r),
        },
        // consume until next whitespace or container end
        .invalid => switch (self.srcAt(0)) {
            0, ' ', '\t', '\r', '\n', ',', ')', ']', '}' => {},
            else => continue :state self.tx(.invalid, 1, &r),
        },
        .ws => switch (self.srcAt(0)) {
            ' ', '\t', '\r', '\n', ',' => continue :state self.tx(.ws, 1, &r),
            ';' => continue :state self.tx(.comment, 1, &r),
            else => {
                r.loc.start = self.index;
                continue :state self.tx(.start, 0, &r);
            },
        },
        .symbol => switch (self.srcAt(0)) {
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>', '+', '-', '.', '0'...'9', '#', ':' => {
                self.index += 1;
                continue :state .symbol;
            },
            '/' => continue :state self.tx(.symbol_slash, 1, &r),
            else => continue :state .symbol_end,
        },
        .symbol_slash => switch (self.srcAt(0)) {
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>', '+', '-', '.', '0'...'9', '#', ':' => {
                continue :state self.tx(.symbol_slash, 1, &r);
            },
            '/' => continue :state self.tx(.invalid, 1, &r),
            else => continue :state .symbol_end,
        },
        .symbol_end => if (r.tag == .symbol) {
            // check for special literals
            const len = "false".len;
            var buf = [1]u8{0} ** len;
            const text = self.src[r.loc.start..self.index];
            const l = @min(buf.len, text.len);
            @memcpy(buf[0..l], text[0..l]);
            // std.debug.print("buf '{s}'\n", .{buf});
            switch (mem.readInt(u40, &buf, .big)) {
                mem.readInt(u40, "nil\x00\x00", .big) => r.tag = .nil,
                mem.readInt(u40, "true\x00", .big) => r.tag = .true,
                mem.readInt(u40, "false", .big) => r.tag = .false,
                else => if (buf[0] == 0) {
                    r.tag = .invalid;
                } else {
                    // check legal idents if namespace
                    var parts = mem.splitScalar(u8, text, '/');
                    while (parts.next()) |part| {
                        if (part.len == 0)
                            r.tag = .invalid
                        else switch (part[0]) {
                            '#', ':' => r.tag = .invalid,
                            else => {},
                        }
                    }
                },
            }
        },
        .str => switch (self.srcAt(0)) {
            0 => _ = self.tx(.invalid, 0, &r),
            '"' => self.index += 1,
            '\\' => continue :state self.tx(.str_escape, 1, &r),
            else => continue :state self.tx(.str, 1, &r),
        },
        .str_escape => switch (self.srcAt(0)) {
            '"', 'n', 't', 'r', '\\' => continue :state self.tx(.str, 1, &r),
            else => _ = self.tx(.invalid, 0, &r),
        },
        // only accept 1 char or "tab", "space", "return", or "newline"
        .char => if (self.index > self.src.len) continue :state .start else {
            // read char content into buffer to use integer comparison below
            const len = "newline".len;
            var buf = [1]u8{0} ** len;
            const rest = self.src[self.index..];
            // read until terminator
            for (0..@min(buf.len, rest.len)) |i| {
                switch (rest[i]) {
                    0, ' ', '\n', '\t', '\r' => break,
                    else => {
                        if (i > 0) switch (rest[i]) {
                            ')', ']', '}', ',' => break,
                            else => {},
                        };
                        self.index += 1;
                        buf[i] = rest[i];
                    },
                }
            }
            switch (mem.readInt(u56, &buf, .big)) {
                mem.readInt(u56, "tab\x00\x00\x00\x00", .big),
                mem.readInt(u56, "space\x00\x00", .big),
                mem.readInt(u56, "return\x00", .big),
                mem.readInt(u56, "newline", .big),
                => {}, // found tab, space, return or newline: ok
                else => if (buf[0] != 0 and buf[1] == 0) {} // len == 1: ok
                else {
                    r.tag = .invalid;
                    continue :state .invalid;
                },
            }
        },
        .char_u => switch (self.srcAt(0)) {
            0, ' ', '\n', '\t', '\r', ',', ')', ']', '}' => {
                const len = self.index - r.loc.start;
                if (len == 2 or len == 6) {} // \u or \uNNNN: ok
                else continue :state self.tx(.invalid, 0, &r);
            },
            '0'...'9', 'A'...'F', 'a'...'f' => continue :state self.tx(.char_u, 1, &r),
            else => continue :state self.tx(.invalid, 1, &r),
        },
        // TODO An integer can have the suffix N to indicate that arbitrary precision is desired.
        .number => switch (self.srcAt(0)) {
            '0'...'9' => continue :state self.tx(.number, 1, &r),
            '.' => {
                self.index += 1;
                r.tag = .float;
                continue :state .number_dot;
            },
            'e', 'E' => continue :state self.tx(.float_e, 1, &r),
            else => _ = self.tx(.int, 0, &r),
        },
        // TODO a floating-point number may have the suffix M to indicate that exact precision is desired.
        .number_dot => switch (self.srcAt(0)) {
            '.' => continue :state self.tx(.invalid, 1, &r),
            '0'...'9' => {
                self.index += 1;
                continue :state .float;
            },
            'e', 'E' => continue :state self.tx(.float_e, 1, &r),
            else => _ = self.tx(.float, 0, &r),
        },
        .float => switch (self.srcAt(0)) {
            '0'...'9' => continue :state self.tx(.float, 1, &r),
            'e', 'E' => continue :state self.tx(.float_e, 1, &r),
            else => {},
        },
        .float_e => switch (self.srcAt(0)) {
            '-', '+' => continue :state self.tx(.float_e_sign, 1, &r),
            '0'...'9' => continue :state self.tx(.float_e_digit, 1, &r),
            else => continue :state self.tx(.invalid, 0, &r),
        },
        .float_e_sign => switch (self.srcAt(0)) {
            '0'...'9' => continue :state self.tx(.float_e_digit, 1, &r),
            else => continue :state self.tx(.invalid, 0, &r),
        },
        .float_e_digit => switch (self.srcAt(0)) {
            '0'...'9' => continue :state self.tx(.float_e_digit, 1, &r),
            0, ' ', '\t', '\r', '\n', ',' => _ = self.tx(.float, 0, &r),
            else => continue :state self.tx(.invalid, 0, &r),
        },
        // comments may include ws and comments on the next line
        .comment => switch (self.srcAt(0)) {
            0 => continue :state self.tx(.start, 0, &r),
            '\n' => continue :state self.tx(.comment_newline, 1, &r),
            else => continue :state self.tx(.comment, 1, &r),
        },
        .comment_newline => switch (self.srcAt(0)) {
            else => continue :state self.tx(.start, 0, &r),
            ';' => continue :state self.tx(.comment, 1, &r),
            ' ', '\t', '\r', '\n', ',' => continue :state self.tx(.comment_newline, 1, &r),
        },
        .keyword => switch (self.srcAt(0)) {
            'a'...'z', 'A'...'Z', '0'...'9', '*', '+', '!', '-', '_', '?', '%', '&', '=', '<', '>', '.', ':', '#' => {
                continue :state self.tx(.keyword, 1, &r);
            },
            '/' => continue :state self.tx(.keyword_slash, 1, &r),
            else => {},
        },
        .keyword_slash => switch (self.srcAt(0)) {
            'a'...'z', 'A'...'Z', '0'...'9', '*', '+', '!', '-', '_', '?', '%', '&', '=', '<', '>', '.', ':', '#' => {
                continue :state self.tx(.keyword_slash, 1, &r);
            },
            '/' => continue :state self.tx(.invalid, 1, &r),
            else => {},
        },
        .tagged, .eof, .int, .nil, .true, .false => unreachable,
        .lparen, .lcurly, .lbracket, .set, .discard, .rparen, .rbracket, .rcurly => {},
    }
    // }}
    // zig fmt: on

    r.loc.end = @min(self.index, self.src.len);
    r.loc.start = @min(r.loc.start, self.src.len);
    // std.debug.print("token {s} '{s}'\n", .{ @tagName(r.tag), r.src(self.src) });
    return r;
}

pub fn peek(self: *const Tokenizer) Token {
    assert(self.index != 0);
    return self.lookbehind.peek().?;
}

pub fn isTag(self: *const Tokenizer, tag: Token.Tag) bool {
    return self.peek().tag == tag;
}

fn expectTokens(src: [:0]const u8, expecteds: []const Token.Tag) !void {
    var t = try Tokenizer.init(src);
    for (expecteds, 0..) |expected_tag, i| {
        const token = t.next();
        // std.debug.print("expected {s}. found {s} '{s}'. text offset {}. tag offset {}\n", .{ @tagName(expected_tag), @tagName(token.tag), token.src(src), t.index, i });
        testing.expectEqual(expected_tag, token.tag) catch {
            std.debug.print("expected {s}. found {s} '{s}'. text offset {}. tag offset {}\n", .{ @tagName(expected_tag), @tagName(token.tag), token.src(src), t.index, i });
            return error.UnexpectedToken;
        };
    }
    const token = t.next();
    if (token.tag != .eof) {
        std.debug.print("expected eof. found {s} '{s}'. text offset {}. tag offset {}\n", .{ @tagName(token.tag), token.src(src), t.index, expecteds.len });
        return error.UnexpectedToken;
    }
}

test "basic" {
    try expectTokens(
        \\(defrecord MenuItem [name rating])
        \\
        \\{:eggs 2
        \\ :lemon-juice 3.5
        \\ :butter 1}
        \\
        \\#{:a :b 88 "huat"}
    , // zig fmt: off
    &.{ .lparen, .symbol, .symbol, .lbracket, .symbol, .symbol, .rbracket, .rparen, 
        .lcurly, .keyword, .int, 
        .keyword, .float, 
        .keyword, .int, .rcurly, 
        .set, .keyword, .keyword, .int, .str, .rcurly,
        .eof }); // zig fmt: on
}

test "special literals" {
    try expectTokens(
        "nil true false",
        &.{ .nil, .true, .false, .eof },
    );
}

test "eof" {
    try expectTokens("", &.{.eof});
}

test "chars" {
    try expectTokens("\\space \\newline \\return \\tab \\c \\u03BB \\u \\z", &.{
        .char, .char, .char, .char, .char, .char, .char, .char, .eof,
    });
    try expectTokens(
        \\\u "hi"
    , &.{ .char, .str, .eof });
    try expectTokens("\\u0 \\u0000", &.{ .invalid, .char, .eof });
    try expectTokens("\\), \\], \\}, \\,,", &.{ .char, .char, .char, .char, .eof });
    try expectTokens("\\)) \\]] \\}}", &.{ .char, .rparen, .char, .rbracket, .char, .rcurly, .eof });
}

test "strs" {
    try expectTokens(
        \\"hello" "escaped \"quote\"" "\n\r\t\\"
    , &.{ .str, .str, .str, .eof });
}

test "numbers" {
    try expectTokens(
        \\42 3.14159 -1
        \\+2 .5
        \\0e10 0E10 9e-10 0.0e0 1.0E-9 100e1
        \\0 00
    , &.{ // zig fmt: off
            .int, .float, .int, 
            .int, .float, 
            .float, .float, .float, .float, .float, .float, 
            .int, .invalid,
            .eof,
    }); // zig fmt: on

}

test "keywords and syms" {
    try expectTokens(
        \\:eggs :lemon-juice kitchen/spoon my-namespace/foo
        \\+foo -foo .foo
        \\::invalid :too/many/slashes :/ :/anything
        \\too/many/slashes a/ /a
    , &.{ // zig fmt: off
            .keyword, .keyword, .symbol, .symbol, 
            .symbol, .symbol, .symbol, 
            .invalid, .invalid, .invalid, .invalid,
            .invalid, .invalid, .invalid,
            .eof,
    }); // zig fmt: on
}

test "comments/ws" {
    try expectTokens(" 99", &.{ .int, .eof });
    try expectTokens(" ;comment\n99", &.{ .int, .eof });
    try expectTokens("42 ; comment\n99", &.{ .int, .int, .eof });
    try expectTokens(
        \\; comment
        \\; comment
    , &.{.eof});
}

test "discard" {
    try expectTokens("#_foo [1 2 3]", &.{
        .discard, .symbol, .lbracket, .int,
        .int,     .int,    .rbracket, .eof,
    });
    try expectTokens("#_ foo 42", &.{ .discard, .symbol, .int });
}

test "containers" {
    try expectTokens(
        \\#{\space \u0000}
    , &.{ .set, .char, .char, .rcurly, .eof });
}

test "complex example" {
    try expectTokens(
        \\; Comments start with a semicolon.
        \\nil         ; also known in other languages as null
        \\
        \\; Booleans
        \\true
        \\false
        \\
        \\; strs are enclosed in double quotes
        \\"hungarian breakfast"
        \\
        \\; chars are preceded by backslashes
        \\\g \r \a \c \e
        \\
        \\; Keywords start with ':' and not '::'
        \\:eggs
        \\:cheese
        \\::bread
        \\
        \\; Maps are associative data structures
        \\{:eggs 2 :butter 1}
    , &.{ // zig fmt: off
        .nil, 
        .true, .false,
        .str,
        .char, .char, 
        .char, .char, .char,
        .keyword, .keyword, .invalid,
        .lcurly, .keyword, .int, 
        .keyword, .int, .rcurly, .eof,
    }); // zig fmt: on
}

const Expectation = struct { [:0]const u8, []const Token.Tag };
inline fn parseExpectations(args: anytype) []const Expectation {
    comptime {
        var expectations: []const Expectation = &.{};
        var str: [:0]const u8 = "";
        var tags: []const Token.Tag = &.{};
        for (args) |arg| {
            switch (@typeInfo(@TypeOf(arg))) {
                else => |ty| @compileError(@tagName(ty)),
                .enum_literal => {
                    tags = tags ++ &[1]Token.Tag{arg};
                },
                .pointer => {
                    if (str.len != 0) {
                        assert(tags.len != 0);
                        expectations = expectations ++ [1]Expectation{.{ str, tags }};
                    }
                    str = arg;
                    tags = &.{};
                },
            }
        }
        assert(str.len != 0);
        assert(tags.len != 0);
        expectations = expectations ++ [1]Expectation{.{ str, tags }};
        return expectations;
    }
}

test "edn.edn" {
    const f = try std.fs.cwd().openFile("examples/edn.edn", .{});
    defer f.close();
    const src = try f.readToEndAllocOptions(talloc, 100000, null, 8, 0);
    defer talloc.free(src);

    const expectations = parseExpectations(.{ // zig fmt: off
        \\; Comments start with a semicolon.
        \\; Anything after the semicolon is ignored.
        \\
        , .eof,
        \\;;;;;;;;;;;;;;;;;;;
        \\;;; Basic Types ;;;
        \\;;;;;;;;;;;;;;;;;;;
        \\
        , .eof,
        \\nil         ; also known in other languages as null
        \\
        , .nil, .eof,
        \\; Booleans
        \\true false
        , .true, .false, .eof,
        \\; strs are enclosed in double quotes
        \\"hungarian breakfast" "farmer's cheesy omelette"
        , .str, .str, .eof,
        \\; chars are preceded by backslashes
        \\\g \r \a \c \e
        , .char, .char, .char, .char, .char, .eof,
        \\; Keywords start with a colon. They behave like enums. Kind of
        \\; like syms in Ruby.
        \\:eggs
        \\:cheese
        \\:olives
        , .keyword, .keyword, .keyword, .eof,
        \\; syms are used to represent identifiers. 
        \\; You can namespace syms by using /. Whatever precedes / is
        \\; the namespace of the symbol.
        \\spoon
        \\kitchen/spoon ; not the same as spoon
        \\kitchen/fork
        \\github/fork    ; you can't eat with this
        , .symbol, .symbol, .symbol, .symbol, .eof,
        \\; Integers and floats
        \\42 3.14159
        , .int, .float, .eof,
        \\; Lists are sequences of values
        \\(:bun :beef-patty 9 "yum!")
        , .lparen, .keyword, .keyword, .int, .str, .rparen, .eof,
        \\; Vectors allow random access
        \\[:gelato 1 2 -2]
        , .lbracket, .keyword, .int, .int, .int, .rbracket, .eof,
        \\; Maps are associative data structures that associate the key with its value
        \\{:eggs        2 :lemon-juice 3.5 :butter      1}
        , .lcurly, .keyword, .int, .keyword, .float, .keyword, .int, .rcurly, .eof,
        \\; You're not restricted to using keywords as keys
        \\{[1 2 3 4] "tell the people what she wore", [5 6 7 8] "the more you see the more you hate"}
        , .lcurly, .lbracket, .int, .int, .int, .int, .rbracket, 
        .str, .lbracket, .int, .int, .int, .int, .rbracket, 
        .str, .rcurly, .eof,
        \\; You may use commas for readability. They are treated as whitespace.
        \\,,,,,,
        , .eof,
        \\; Sets are collections that contain unique elements.
        \\#{:a :b 88 "huat"}
        , .set, .keyword, .keyword, .int, .str, .rcurly, .eof,
        \\;;;;;;;;;;;;;;;;;;;;;;;
        \\;;; Tagged Elements ;;;
        \\;;;;;;;;;;;;;;;;;;;;;;;
        \\
        , .eof,
        \\; EDN can be extended by tagging elements with # syms.
        \\#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}
        , .tagged, .lcurly, .keyword, .str, .keyword, .int, .rcurly, .eof,
        \\; Let me explain this with a Clojure example. Suppose I want to transform that
        \\; piece of EDN into a MenuItem record.
        \\(defrecord MenuItem [name rating])
        , .lparen, .symbol, .symbol, .lbracket, .symbol, .symbol, .rbracket, .rparen, .eof,
        \\; defrecord defined, among other things, map->MenuItem which will take a map
        \\; of field names (as keywords) to values and generate a user.MenuItem record
        \\; To transform EDN to Clojure values, I will need to use the built-in EDN
        \\; reader, clojure.edn/read-string
        \\(clojure.edn/read-string "{:eggs 2 :butter 1 :flour 5}")
        \\; -> {:eggs 2 :butter 1 :flour 5}
        \\; To transform tagged elements, pass to clojure.edn/read-string an option map
        \\; with a :readers map that maps tag syms to data-reader functions, like so
        \\
        \\;(clojure.edn/read-string
        \\;    {:readers {'MyYelpClone/MenuItem map->MenuItem}}
        \\;    "#MyYelpClone/MenuItem {:name \"eggs-benedict\" :rating 10}")
        \\; -> #user.MenuItem{:name "eggs-benedict", :rating 10}
        , 
        .lparen, .symbol, .str, .rparen,
        .eof,
}); // zig fmt: on

    inline for (expectations) |expecteds| {
        expectTokens(expecteds[0], expecteds[1]) catch |e| {
            std.debug.print("{s}\n{any}\n", .{ expecteds[0], expecteds[1] });
            return e;
        };
    }
}

test "Tokenizer.edn" {
    const expectations = parseExpectations(.{ // zig fmt: off

        \\; * character sets
        \\:SYMBOL     #{(\a \z) (\A \Z) \* \! \_ \? \% \& \= \. \< \> \/ \+ \-}
        , .keyword, .set,
            .lparen, .char, .char, .rparen, 
            .lparen, .char, .char, .rparen, 
            .char, .char, .char, .char, .char, 
            .char, .char, .char, .char, .char, 
            .char, .char, .char,
        .rcurly, .eof,

        \\:SYMBOL     #{:edn/charset:a-zA-Z*!_?%&=.<>+ \- \/}
        \\:SYMBOL-1   #{:SYMBOL :edn/charset:0-9#:}
        , .keyword, .set, //4
           .keyword, .char, .char, .rcurly, //10
        .keyword, .set, //14
           .keyword, .keyword, .rcurly, //21
        .eof,

        \\; asdf
        \\:SYMBOL-1   #{
        \\  :SYMBOL (\0 \9) \# \:
        \\}
        , .keyword, .set, //6
            .keyword, .lparen, .char, .char, .rparen, .char, .char, //18
        .rcurly, .eof,

        \\:WS         #{\space \tab \return \newline \,}
        , .keyword, .set, .char, .char, .char, .char, .char, .rcurly, .eof,
        
        \\:DIGIT      #{:edn/charset:0-9}
        , .keyword, .set, .keyword, .rcurly, .eof,
        
        \\:DIGIT-1    #{:edn/charset:1-9}
        , .keyword, .set, .keyword, .rcurly, .eof,
        
        \\:WS-COMMENT (:WS \;) ; list is a union of character sets
        , .keyword, .lparen, .keyword, .char, .rparen, .eof,
        
        \\:KEYWORD    #{:edn/charset:a-zA-Z0-9*+!_?%&=<>. \- \/}
        , .keyword, .set, .keyword, .char, .char, .rcurly, .eof,
        
        \\:E          #{\e \E}
        , .keyword, .set, .char, .char, .rcurly, .eof,
        
        \\:STR-ESC    #{\" \newline \tab \\ \return}
        , .keyword, .set, .char, .char, .char, .char, .char, .rcurly, .eof,
        
        \\; TODO
        \\:NUM_SUFFIX #{\N \M}
        , .keyword, .set, .char, .char, .rcurly, .eof,
        
        \\; TODO
        \\; * string sets
        \\:SYMBOL-2   #{"nil" "true" "false"}
        , .keyword, .set, .str, .str, .str, .rcurly, .eof,
        
        \\:CHAR-NAMED #{"space" "newline" "tab" "return" "backspace" "formfeed"}
        , .keyword, .set, .str, .str, .str, .str, .str, .str, .rcurly, .eof,
        
        // rest of file omitted        
    }); // zig fmt: on

    inline for (expectations, 0..) |expectation, i| {
        expectTokens(expectation[0], expectation[1]) catch |e| {
            std.debug.print("expectation {}: {s}\n{any}\n", .{ i, expectation[0], expectation[1] });
            return e;
        };
    }
}

// ported from
test "jorinvo/edn-data tests" {
    // Basic values
    try expectTokens("nil", &.{ .nil, .eof });
    try expectTokens("true", &.{ .true, .eof });
    try expectTokens("false", &.{ .false, .eof });

    // Strings
    try expectTokens("\"\"", &.{ .str, .eof });
    try expectTokens("\"hello\"", &.{ .str, .eof });
    try expectTokens("\"hello\\nworld\"", &.{ .str, .eof });
    try expectTokens("\"\\\"quoted\\\"\"", &.{ .str, .eof });

    // Characters
    try expectTokens("\\a", &.{ .char, .eof });
    try expectTokens("\\newline", &.{ .char, .eof });
    try expectTokens("\\space", &.{ .char, .eof });
    try expectTokens("\\tab", &.{ .char, .eof });
    try expectTokens("\\u03BB", &.{ .char, .eof });

    // Symbols
    try expectTokens("hello", &.{ .symbol, .eof });
    try expectTokens("hello-world", &.{ .symbol, .eof });
    try expectTokens("hello/world", &.{ .symbol, .eof });
    try expectTokens("+", &.{ .symbol, .eof });
    try expectTokens("-", &.{ .symbol, .eof });
    try expectTokens("*", &.{ .symbol, .eof });
    try expectTokens("!", &.{ .symbol, .eof });
    try expectTokens("_", &.{ .symbol, .eof });
    try expectTokens("?", &.{ .symbol, .eof });
    try expectTokens("$", &.{ .invalid, .eof });
    try expectTokens("..", &.{ .symbol, .eof });
    try expectTokens(".-", &.{ .symbol, .eof });

    // Keywords
    try expectTokens(":hello", &.{ .keyword, .eof });
    try expectTokens(":hello/world", &.{ .keyword, .eof });
    try expectTokens("::invalid", &.{ .invalid, .eof });

    // Numbers
    try expectTokens("0", &.{ .int, .eof });
    try expectTokens("123", &.{ .int, .eof });
    try expectTokens("-123", &.{ .int, .eof });
    try expectTokens("+123", &.{ .int, .eof });
    try expectTokens("123.456", &.{ .float, .eof });
    try expectTokens("-123.456", &.{ .float, .eof });
    try expectTokens(".456", &.{ .float, .eof });
    try expectTokens("-.456", &.{ .symbol, .eof });
    try expectTokens("123e4", &.{ .float, .eof });
    try expectTokens("123e-4", &.{ .float, .eof });
    try expectTokens("123.456e7", &.{ .float, .eof });
    try expectTokens("123.456e-7", &.{ .float, .eof });
    try expectTokens("123.456e+7", &.{ .float, .eof });

    // Collections
    try expectTokens("()", &.{ .lparen, .rparen, .eof });
    try expectTokens("[]", &.{ .lbracket, .rbracket, .eof });
    try expectTokens("{}", &.{ .lcurly, .rcurly, .eof });
    try expectTokens("#{}", &.{ .set, .rcurly, .eof });

    // Nested collections
    try expectTokens("([])", &.{ .lparen, .lbracket, .rbracket, .rparen, .eof });
    try expectTokens("[()]", &.{ .lbracket, .lparen, .rparen, .rbracket, .eof });
    try expectTokens("{[]}", &.{ .lcurly, .lbracket, .rbracket, .rcurly, .eof });
    try expectTokens("#{()}", &.{ .set, .lparen, .rparen, .rcurly, .eof });

    // Collections with values
    try expectTokens("(1 2 3)", &.{ .lparen, .int, .int, .int, .rparen, .eof });
    try expectTokens("[1 2 3]", &.{ .lbracket, .int, .int, .int, .rbracket, .eof });
    try expectTokens("{:a 1 :b 2}", &.{ .lcurly, .keyword, .int, .keyword, .int, .rcurly, .eof });
    try expectTokens("#{1 2 3}", &.{ .set, .int, .int, .int, .rcurly, .eof });

    // Comments
    try expectTokens(";; comment", &.{.eof});
    try expectTokens("1 ;; comment\n2", &.{ .int, .int, .eof });
    try expectTokens(";; comment\n1", &.{ .int, .eof });

    // Whitespace
    try expectTokens(" \t\n\r,", &.{.eof});
    try expectTokens("1 2", &.{ .int, .int, .eof });
    try expectTokens("1,2", &.{ .int, .int, .eof });

    // Tagged elements
    try expectTokens("#inst \"2020-01-01\"", &.{ .tagged, .str, .eof });
    try expectTokens("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"", &.{ .tagged, .str, .eof });
    try expectTokens("#custom/tag [1 2 3]", &.{ .tagged, .lbracket, .int, .int, .int, .rbracket, .eof });

    // Discard
    try expectTokens("#_ 123", &.{ .discard, .int, .eof });
    try expectTokens("#_123 456", &.{ .discard, .int, .int, .eof });
    try expectTokens("#_ (1 2 3) 456", &.{ .discard, .lparen, .int, .int, .int, .rparen, .int, .eof });

    // Complex examples
    try expectTokens(
        \\{:a 1, :b "hello", :c [1 2 3], :d #{:x :y}, :e nil, :f true, :g false}
    , &.{ // zig fmt: off
        .lcurly, .keyword, .int, .keyword, .str, .keyword, .lbracket, .int, .int, .int, .rbracket, 
        .keyword, .set, .keyword, .keyword, .rcurly, .keyword, .nil, .keyword, .true, .keyword, .false, .rcurly, .eof
    }); // zig fmt: on

    try expectTokens(
        \\(defn greet [name] (str "Hello, " name "!"))
    , &.{ // zig fmt: off
        .lparen, .symbol, .symbol, .lbracket, .symbol, .rbracket, .lparen, .symbol, .str, .symbol, .str, .rparen, .rparen, .eof
    }); // zig fmt: on

    // Edge cases
    try expectTokens("", &.{.eof});
    try expectTokens("\\", &.{ .invalid, .eof });
    try expectTokens("\\\\", &.{ .char, .eof });
    try expectTokens("\"", &.{ .invalid, .eof });
    try expectTokens("\"\\", &.{ .invalid, .eof });
    try expectTokens(
        \\"\z\"
    , &.{ .invalid, .symbol, .char, .eof });
    try expectTokens("01", &.{ .invalid, .eof });
    try expectTokens("1..", &.{ .invalid, .eof });
    try expectTokens("1e", &.{ .invalid, .eof });
    try expectTokens("1e+", &.{ .invalid, .eof });
    try expectTokens("1e-", &.{ .invalid, .eof });
    try expectTokens("1e+a", &.{ .invalid, .eof });
    try expectTokens("(", &.{ .lparen, .eof });
    try expectTokens(")", &.{ .rparen, .eof });
    try expectTokens("[", &.{ .lbracket, .eof });
    try expectTokens("]", &.{ .rbracket, .eof });
    try expectTokens("{", &.{ .lcurly, .eof });
    try expectTokens("}", &.{ .rcurly, .eof });
    try expectTokens("#{", &.{ .set, .eof });
}

test "fuzz edge cases" {
    try expectTokens(
        \\STR_*ISC s  #{\" \n \t\\ \r}
    , &.{ .symbol, .symbol, .set, .char, .char, .invalid, .char, .rcurly, .eof });
}

test "tagged elements" {
    try expectTokens(
        "#inst \"2020-04-13T08:01:14.261Z\" #uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"",
        &.{ .tagged, .str, .tagged, .str, .eof },
    );
    try expectTokens(
        \\#inst "2020-04-13T08:01:14.261Z"
        \\#uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
        \\#my/custom-tag {:a 1, :b 2}
        \\#js/Date "2020-04-13"
    , &.{ // zig fmt: off
        .tagged, .str, 
        .tagged, .str, 
        .tagged, .lcurly, .keyword, .int, .keyword, .int, .rcurly,
        .tagged, .str, .eof
    }); // zig fmt: on
}

test "discard pattern" {
    try expectTokens(
        \\[1 #_2 3]
        \\[1 #_[2 3 4] 5]
        \\[1 #_ ;; comment
        \\ 2 3]
    , &.{ // zig fmt: off
        .lbracket, .int, .discard, .int, .int, .rbracket, 
        .lbracket, .int, .discard, .lbracket, .int, .int, .int, .rbracket, .int, .rbracket, 
        .lbracket, .int, .discard, .int, .int, .rbracket, .eof
    }); // zig fmt: on
}

test "nested collections" {
    try expectTokens(
        \\{:a {:b {:c [1 2 #{3 4}]}}}
    , &.{ // zig fmt: off
        .lcurly, .keyword, .lcurly, .keyword, .lcurly, .keyword, .lbracket, .int, .int, .set, .int, .int, .rcurly, .rbracket, .rcurly, .rcurly, .rcurly, .eof
    }); // zig fmt: on
}

test "symbols with special characters" {
    try expectTokens(
        \\hello-world
        \\hello/world
        \\hello.world
        \\hello*world
        \\hello+world
        \\hello!world
        \\hello_world
        \\hello?world
        \\hello%world
        \\hello&world
        \\hello=world
        \\hello<world
        \\hello>world
        \\hello/world
    , &.{ // zig fmt: off
        .symbol, .symbol, .symbol, .symbol, .symbol, .symbol, .symbol, .symbol,
        .symbol, .symbol, .symbol, .symbol, .symbol, .symbol, .eof
    }); // zig fmt: on
}

test "scientific notation" {
    try expectTokens(
        \\1e10
        \\1e+10
        \\1e-10
        \\1.5e10
        \\1.5e+10
        \\1.5e-10
        \\-1e10
        \\-1.5e10
    , &.{ // zig fmt: off
        .float, .float, .float, .float, .float, .float, .float, .float, .eof
    }); // zig fmt: on
}

test "multiline strings and comments" {
    try expectTokens(
        \\"This is a
        \\multiline string"
        \\
        \\;; This is a
        \\;; multiline comment
        \\
        \\42
    , &.{ .str, .int, .eof });
}

test "invalid tokens" {
    try expectTokens("@", &.{ .invalid, .eof });
    try expectTokens("$", &.{ .invalid, .eof });
    try expectTokens("`", &.{ .invalid, .eof });
    try expectTokens("~", &.{ .invalid, .eof });
    try expectTokens("^", &.{ .invalid, .eof });
    try expectTokens("'", &.{ .invalid, .eof });
    try expectTokens("\"unclosed string", &.{ .invalid, .eof });
    try expectTokens("\"invalid escape \\z\"", &.{ .invalid, .symbol, .invalid, .eof });
    try expectTokens("\\invalid-char-name", &.{ .invalid, .eof });
    try expectTokens("\\u123z", &.{ .invalid, .eof });
    try expectTokens("##foo", &.{ .invalid, .eof });
}

fn testLoc(
    expected_src: []const u8,
    comptime srcs: []const [:0]const u8,
    comptime value_srcs: []const [:0]const u8,
) !void {
    comptime var src: [:0]const u8 = &.{};
    inline for (srcs) |s| src = src ++ s;
    try testing.expectEqualStrings(expected_src, src);

    var t = try Tokenizer.init(src);
    var i: usize = 0;
    while (true) : (i += 1) {
        const token = t.next();
        // std.debug.print("{s}: ws '{s}' content '{s}'\n", .{ @tagName(token.tag), token.loc.ws(src), token.loc.src(src) });
        if (i < value_srcs.len) try testing.expectEqualStrings(value_srcs[i], token.loc.src(src));
        const ws = token.loc.ws(src);
        try testing.expectEqualStrings(srcs[i][0..ws.len], ws);
        try testing.expectEqualStrings(srcs[i][ws.len..], token.loc.src(src));
        if (token.tag == .eof) break;
    }
}

test "Token.Loc" {
    try testLoc(
        " ( [ ] { } ) ",
        &.{ " (", " [", " ]", " {", " }", " )", " " },
        &.{},
    );
    try testLoc(
        \\{:search_metadata {:completed_in 0.087 :max_id 505874924095815700 }}
    ,
        &.{ "{", ":search_metadata", " {", ":completed_in", " 0.087", " :max_id", " 505874924095815700", " }", "}", "" },
        &.{ "{", ":search_metadata", "{", ":completed_in", "0.087", ":max_id", "505874924095815700", "}", "}", "" },
    );
}

const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;
const mem = std.mem;
const assert = std.debug.assert;
