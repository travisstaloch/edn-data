const Tokenizer = @This();

src: [:0]const u8,
index: u32 = 0,
lookbehind: RingBuffer = .init,

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

        pub const zero: Loc = .{ .start = 0, .end = 0 };
        pub fn src(loc: Loc, s: []const u8) []const u8 {
            return s[loc.start..loc.end];
        }
        pub fn ws(loc: Loc, s: []const u8) []const u8 {
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
        lbrace, // '{'
        lbracket, // '['
        set, // "#{"
        discard, // "#_"
        // closing tags - don't require leading whitespace
        rparen, // ')'
        rbracket, // ']'
        rbrace, // '}'
        eof, // 0
        invalid,

        pub fn isOpening(tag: Tag) bool {
            return @intFromEnum(tag) >= @intFromEnum(Tag.lparen) and @intFromEnum(tag) < @intFromEnum(Tag.rparen);
        }

        pub fn isClosing(tag: Tag) bool {
            return @intFromEnum(tag) >= @intFromEnum(Tag.rparen);
        }
    };

    pub fn src(t: Token, s: []const u8) []const u8 {
        return t.loc.src(s);
    }
};

pub fn init(src: [:0]const u8) Tokenizer {
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

fn tagInc(self: *Tokenizer, tag: Token.Tag, inc: u32, result: *Token) void {
    result.loc.start = self.index;
    self.index += inc;
    result.tag = tag;
}

fn nextInner(self: *Tokenizer) Token {
    var result: Token = .{ .tag = undefined, .loc = .{
        .ws_start = self.index,
        .start = self.index,
        .end = undefined,
    } };
    const State = enum(u8) {
        start = 0,
        invalid = 1,
        symbol,
        symbol_slash,
        keyword, // ':'
        keyword_slash,
        str, // '"'
        str_escape, // '\\'
        char, // '\\'
        char_u, // 'u'
        number,
        number_dot,
        float,
        float_e,
        float_e_digit,
        float_e_sign,
        ws, // ' '
        comment, // ';'
        comment_newline,
    };

    state: switch (State.start) {
        // consume until next whitespace
        .invalid => switch (self.src[self.index]) {
            0, ' ', '\t', '\r', '\n', ',' => {},
            else => {
                self.index += 1;
                continue :state .invalid;
            },
        },
        .start => switch (self.src[self.index]) {
            0 => self.tagInc(.eof, 0, &result),
            ' ', '\t', '\r', '\n', ',' => {
                self.index += 1;
                continue :state .ws;
            },
            '(' => self.tagInc(.lparen, 1, &result),
            ')' => self.tagInc(.rparen, 1, &result),
            '[' => self.tagInc(.lbracket, 1, &result),
            ']' => self.tagInc(.rbracket, 1, &result),
            '{' => self.tagInc(.lbrace, 1, &result),
            '}' => self.tagInc(.rbrace, 1, &result),
            '#' => switch (self.src[self.index + 1]) {
                0, ' ', '\t', '\r', '\n', ',' => {
                    result.tag = .invalid;
                },
                '#', ':' => {
                    self.tagInc(.invalid, 2, &result);
                    continue :state .invalid;
                },
                '{' => {
                    self.tagInc(.set, 2, &result);
                },
                '_' => {
                    self.tagInc(.discard, 2, &result);
                },
                else => {
                    self.tagInc(.tagged, 1, &result);
                    continue :state .symbol;
                },
            },
            ';' => {
                self.index += 1;
                continue :state .comment;
            },
            '"' => {
                self.tagInc(.str, 1, &result);
                continue :state .str;
            },
            '\\' => {
                self.tagInc(.char, 1, &result);
                if (self.src[self.index] == 'u') {
                    self.index += 1;
                    continue :state .char_u;
                } else continue :state .char;
            },
            ':' => switch (self.src[self.index + 1]) {
                ':', '/', '#' => {
                    self.tagInc(.invalid, 2, &result);
                    continue :state .invalid;
                },
                else => {
                    self.tagInc(.keyword, 1, &result);
                    continue :state .keyword;
                },
            },
            '0' => switch (self.src[self.index + 1]) {
                '0'...'9' => {
                    self.tagInc(.invalid, 1, &result);
                    continue :state .invalid;
                },
                'e', 'E' => {
                    self.index += 2;
                    result.tag = .float;
                    continue :state .float_e;
                },
                '.' => {
                    self.index += 2;
                    result.tag = .float;
                    continue :state .float;
                },
                else => {
                    self.tagInc(.int, 1, &result);
                    continue :state .number;
                },
            },
            '1'...'9' => {
                self.tagInc(.int, 1, &result);
                continue :state .number;
            },
            '-', '+' => switch (self.src[self.index + 1]) {
                // number if followed by a digit
                '0'...'9' => {
                    self.tagInc(.int, 1, &result);
                    continue :state .number;
                },
                else => {
                    self.tagInc(.symbol, 1, &result);
                    continue :state .symbol;
                },
            },
            '.' => if (std.ascii.isDigit(self.src[self.index + 1])) {
                self.tagInc(.float, 1, &result);
                continue :state .float;
            } else {
                self.tagInc(.symbol, 1, &result);
                continue :state .symbol;
            },
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>' => {
                self.tagInc(.symbol, 1, &result);
                continue :state .symbol;
            },
            else => {
                self.tagInc(.invalid, 1, &result);
                continue :state .invalid;
            },
        },
        .ws => switch (self.src[self.index]) {
            ' ', '\t', '\r', '\n', ',' => {
                self.index += 1;
                continue :state .ws;
            },
            ';' => {
                self.index += 1;
                continue :state .comment;
            },
            else => {
                continue :state .start;
            },
        },
        .symbol => switch (self.src[self.index]) {
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>', '+', '-', '.', '0'...'9', '#', ':' => {
                self.index += 1;
                continue :state .symbol;
            },
            '/' => {
                self.index += 1;
                continue :state .symbol_slash;
            },
            else => {},
        },
        .symbol_slash => switch (self.src[self.index]) {
            'a'...'z', 'A'...'Z', '*', '!', '_', '?', '%', '&', '=', '<', '>', '+', '-', '.', '0'...'9', '#', ':' => {
                self.index += 1;
                continue :state .symbol_slash;
            },
            '/' => {
                self.index += 1;
                result.tag = .invalid;
                continue :state .invalid;
            },
            else => {},
        },
        .str => switch (self.src[self.index]) {
            0 => self.tagInc(.invalid, 0, &result),
            '"' => self.index += 1,
            '\\' => {
                self.index += 1;
                continue :state .str_escape;
            },
            else => {
                self.index += 1;
                continue :state .str;
            },
        },
        .str_escape => switch (self.src[self.index]) {
            '"', 'n', 't', 'r', '\\' => {
                self.index += 1;
                continue :state .str;
            },
            else => self.tagInc(.invalid, 0, &result),
        },
        // only accept 1 char or "tab", "space", "return", or "newline"
        .char => if (std.mem.startsWith(u8, self.src[self.index..], "space")) {
            self.index += 5;
        } else if (std.mem.startsWith(u8, self.src[self.index..], "newline")) {
            self.index += 7;
        } else if (std.mem.startsWith(u8, self.src[self.index..], "tab")) {
            self.index += 3;
        } else if (std.mem.startsWith(u8, self.src[self.index..], "return")) {
            self.index += 6;
        } else switch (self.src[self.index]) {
            0, ' ', '\n', '\t', '\r' => result.tag = .invalid,
            else => switch (self.src[self.index + 1]) {
                0, ' ', '\n', '\t', '\r', ')', ']', '}', ',' => self.index += 1,
                else => {
                    result.tag = .invalid;
                    continue :state .invalid;
                },
            },
        },
        .char_u => switch (self.src[self.index]) {
            0, ' ', '\n', '\t', '\r', ',', ')', ']', '}' => {},
            '0'...'9', 'A'...'F', 'a'...'f' => {
                self.index += 1;
                continue :state .char_u;
            },
            else => {
                result.tag = .invalid;
                continue :state .invalid;
            },
        },
        // TODO An integer can have the suffix N to indicate that arbitrary precision is desired.
        .number => switch (self.src[self.index]) {
            '0'...'9' => {
                self.index += 1;
                continue :state .number;
            },
            '.' => {
                result.tag = .float;
                self.index += 1;
                continue :state .number_dot;
            },
            'e', 'E' => {
                result.tag = .float;
                self.index += 1;
                continue :state .float_e;
            },
            else => {},
        },
        // TODO a floating-point number may have the suffix M to indicate that exact precision is desired.
        .number_dot => switch (self.src[self.index]) {
            '.' => {
                result.tag = .invalid;
                self.index += 1;
                continue :state .invalid;
            },
            '0'...'9' => {
                self.index += 1;
                continue :state .float;
            },
            'e', 'E' => {
                self.index += 1;
                continue :state .float_e;
            },
            else => {},
        },
        .float => switch (self.src[self.index]) {
            '0'...'9' => {
                self.index += 1;
                continue :state .float;
            },
            'e', 'E' => {
                self.index += 1;
                continue :state .float_e;
            },
            else => {},
        },
        .float_e => switch (self.src[self.index]) {
            '-', '+' => {
                self.index += 1;
                continue :state .float_e_sign;
            },
            '0'...'9' => {
                self.index += 1;
                continue :state .float_e_digit;
            },
            else => {
                result.tag = .invalid;
                continue :state .invalid;
            },
        },
        .float_e_sign => switch (self.src[self.index]) {
            '0'...'9' => {
                self.index += 1;
                continue :state .float_e_digit;
            },
            else => {
                result.tag = .invalid;
                continue :state .invalid;
            },
        },
        .float_e_digit => switch (self.src[self.index]) {
            '0'...'9' => {
                self.index += 1;
                continue :state .float_e_digit;
            },
            0, ' ', '\t', '\r', '\n', ',' => {},
            else => {
                self.index += 1;
                continue :state .invalid;
            },
        },
        // comments may include ws and comments on the next line
        .comment => switch (self.src[self.index]) {
            0 => continue :state .start,
            '\n' => {
                self.index += 1;
                continue :state .comment_newline;
            },
            else => {
                self.index += 1;
                continue :state .comment;
            },
        },
        .comment_newline => switch (self.src[self.index]) {
            else => continue :state .start,
            ';' => {
                self.index += 1;
                continue :state .comment;
            },
            ' ', '\t', '\r', '\n', ',' => {
                self.index += 1;
                continue :state .comment_newline;
            },
        },
        .keyword => switch (self.src[self.index]) {
            'a'...'z', 'A'...'Z', '0'...'9', '*', '+', '!', '-', '_', '?', '%', '&', '=', '<', '>', '.', ':', '#' => {
                self.index += 1;
                continue :state .keyword;
            },
            '/' => {
                self.index += 1;
                continue :state .keyword_slash;
            },
            else => {},
        },
        .keyword_slash => switch (self.src[self.index]) {
            'a'...'z', 'A'...'Z', '0'...'9', '*', '+', '!', '-', '_', '?', '%', '&', '=', '<', '>', '.', ':', '#' => {
                self.index += 1;
                continue :state .keyword_slash;
            },
            '/' => {
                result.tag = .invalid;
                self.index += 1;
                continue :state .invalid;
            },
            else => {},
        },
    }

    if (result.tag == .symbol) {
        // check for special literals
        const text = self.src[result.loc.start..self.index];
        if (std.mem.eql(u8, text, "nil")) {
            result.tag = .nil;
        } else if (std.mem.eql(u8, text, "true")) {
            result.tag = .true;
        } else if (std.mem.eql(u8, text, "false")) {
            result.tag = .false;
        } else {
            // check legal idents if namespace
            var parts = std.mem.splitScalar(u8, text, '/');
            while (parts.next()) |part| {
                if (part.len == 0)
                    result.tag = .invalid
                else switch (part[0]) {
                    '#', ':' => result.tag = .invalid,
                    else => {},
                }
            }
        }
    }

    result.loc.end = self.index;
    return result;
}

pub fn peek(self: *const Tokenizer) Token {
    assert(self.index != 0);
    return self.lookbehind.peek().?;
}

pub fn isTag(self: *const Tokenizer, tag: Token.Tag) bool {
    return self.peek().tag == tag;
}

fn expectTokens(src: [:0]const u8, expecteds: []const Token.Tag) !void {
    var t = Tokenizer.init(src);
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
        .lbrace, .keyword, .int, 
        .keyword, .float, 
        .keyword, .int, .rbrace, 
        .set, .keyword, .keyword, .int, .str, .rbrace,
        .eof }); // zig fmt: on
}

test "special literals" {
    try expectTokens(
        "nil true false",
        &.{ .nil, .true, .false, .eof },
    );
}

test "chars" {
    try expectTokens("\\space \\newline \\return \\tab \\c \\u03BB \\u \\z", &.{
        .char, .char, .char, .char, .char, .char, .char, .char, .eof,
    });
    try expectTokens(
        \\\u "hi"
    , &.{ .char, .str, .eof });
    try expectTokens("\\u0", &.{ .char, .eof });
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
        \\#{\space \u0}
    , &.{ .set, .char, .char, .rbrace, .eof });
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
        .lbrace, .keyword, .int, 
        .keyword, .int, .rbrace, .eof,
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
    const src = try f.readToEndAllocOptions(talloc, 100000, null, @enumFromInt(12), 0);
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
        , .lbrace, .keyword, .int, .keyword, .float, .keyword, .int, .rbrace, .eof,
        \\; You're not restricted to using keywords as keys
        \\{[1 2 3 4] "tell the people what she wore", [5 6 7 8] "the more you see the more you hate"}
        , .lbrace, .lbracket, .int, .int, .int, .int, .rbracket, 
        .str, .lbracket, .int, .int, .int, .int, .rbracket, 
        .str, .rbrace, .eof,
        \\; You may use commas for readability. They are treated as whitespace.
        \\,,,,,,
        , .eof,
        \\; Sets are collections that contain unique elements.
        \\#{:a :b 88 "huat"}
        , .set, .keyword, .keyword, .int, .str, .rbrace, .eof,
        \\;;;;;;;;;;;;;;;;;;;;;;;
        \\;;; Tagged Elements ;;;
        \\;;;;;;;;;;;;;;;;;;;;;;;
        \\
        , .eof,
        \\; EDN can be extended by tagging elements with # syms.
        \\#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}
        , .tagged, .lbrace, .keyword, .str, .keyword, .int, .rbrace, .eof,
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
    const f = try std.fs.cwd().openFile("examples/Tokenizer.edn", .{});
    defer f.close();
    const src = try f.readToEndAllocOptions(talloc, 100000, null, @enumFromInt(12), 0);
    defer talloc.free(src);

    const expectations = parseExpectations(.{ // zig fmt: off

        \\; * character sets
        \\:SYMBOL     #{(\a \z) (\A \Z) \* \! \_ \? \% \& \= \. \< \> \/ \+ \-}
        , .keyword, .set,
            .lparen, .char, .char, .rparen, 
            .lparen, .char, .char, .rparen, 
            .char, .char, .char, .char, .char, 
            .char, .char, .char, .char, .char, 
            .char, .char, .char,
        .rbrace, .eof,

        \\:SYMBOL     #{:edn/charset:a-zA-Z*!_?%&=.<>+ \- \/}
        \\:SYMBOL-1   #{:SYMBOL :edn/charset:0-9#:}
        , .keyword, .set, //4
           .keyword, .char, .char, .rbrace, //10
        .keyword, .set, //14
           .keyword, .keyword, .rbrace, //21
        .eof,

        \\; asdf
        \\:SYMBOL-1   #{
        \\  :SYMBOL (\0 \9) \# \:
        \\}
        , .keyword, .set, //6
            .keyword, .lparen, .char, .char, .rparen, .char, .char, //18
        .rbrace, .eof,

        \\:WS         #{\space \tab \return \newline \,}
        , .keyword, .set, .char, .char, .char, .char, .char, .rbrace, .eof,
        
        \\:DIGIT      #{:edn/charset:0-9}
        , .keyword, .set, .keyword, .rbrace, .eof,
        
        \\:DIGIT-1    #{:edn/charset:1-9}
        , .keyword, .set, .keyword, .rbrace, .eof,
        
        \\:WS-COMMENT (:WS \;) ; list is a union of character sets
        , .keyword, .lparen, .keyword, .char, .rparen, .eof,
        
        \\:KEYWORD    #{:edn/charset:a-zA-Z0-9*+!_?%&=<>. \- \/}
        , .keyword, .set, .keyword, .char, .char, .rbrace, .eof,
        
        \\:E          #{\e \E}
        , .keyword, .set, .char, .char, .rbrace, .eof,
        
        \\:STR-ESC    #{\" \newline \tab \\ \return}
        , .keyword, .set, .char, .char, .char, .char, .char, .rbrace, .eof,
        
        \\; TODO
        \\:NUM_SUFFIX #{\N \M}
        , .keyword, .set, .char, .char, .rbrace, .eof,
        
        \\; TODO
        \\; * string sets
        \\:SYMBOL-2   #{"nil" "true" "false"}
        , .keyword, .set, .str, .str, .str, .rbrace, .eof,
        
        \\:CHAR-NAMED #{"space" "newline" "tab" "return" "backspace" "formfeed"}
        , .keyword, .set, .str, .str, .str, .str, .str, .str, .rbrace, .eof,
        
        // rest of file omitted        
    }); // zig fmt: on

    inline for (expectations, 0..) |expecteds, i| {
        expectTokens(expecteds[0], expecteds[1]) catch |e| {
            std.debug.print("expectation {}: {s}\n{any}\n", .{ i, expecteds[0], expecteds[1] });
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
    try expectTokens("{}", &.{ .lbrace, .rbrace, .eof });
    try expectTokens("#{}", &.{ .set, .rbrace, .eof });

    // Nested collections
    try expectTokens("([])", &.{ .lparen, .lbracket, .rbracket, .rparen, .eof });
    try expectTokens("[()]", &.{ .lbracket, .lparen, .rparen, .rbracket, .eof });
    try expectTokens("{[]}", &.{ .lbrace, .lbracket, .rbracket, .rbrace, .eof });
    try expectTokens("#{()}", &.{ .set, .lparen, .rparen, .rbrace, .eof });

    // Collections with values
    try expectTokens("(1 2 3)", &.{ .lparen, .int, .int, .int, .rparen, .eof });
    try expectTokens("[1 2 3]", &.{ .lbracket, .int, .int, .int, .rbracket, .eof });
    try expectTokens("{:a 1 :b 2}", &.{ .lbrace, .keyword, .int, .keyword, .int, .rbrace, .eof });
    try expectTokens("#{1 2 3}", &.{ .set, .int, .int, .int, .rbrace, .eof });

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
        .lbrace, .keyword, .int, .keyword, .str, .keyword, .lbracket, .int, .int, .int, .rbracket, 
        .keyword, .set, .keyword, .keyword, .rbrace, .keyword, .nil, .keyword, .true, .keyword, .false, .rbrace, .eof
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
    try expectTokens("{", &.{ .lbrace, .eof });
    try expectTokens("}", &.{ .rbrace, .eof });
    try expectTokens("#{", &.{ .set, .eof });
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
        .tagged, .lbrace, .keyword, .int, .keyword, .int, .rbrace, 
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
        .lbrace, .keyword, .lbrace, .keyword, .lbrace, .keyword, .lbracket, .int, .int, .set, .int, .int, .rbrace, .rbracket, .rbrace, .rbrace, .rbrace, .eof
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
) !void {
    comptime var src: [:0]const u8 = &.{};
    inline for (srcs) |s| src = src ++ s;
    try testing.expectEqualStrings(expected_src, src);

    var t = Tokenizer.init(src);
    var i: usize = 0;
    while (true) : (i += 1) {
        const token = t.next();
        // std.debug.print("{s}: ws '{s}' content '{s}'\n", .{ @tagName(token.tag), token.loc.ws(src), token.loc.src(src) });
        if (token.tag != .eof) {
            try testing.expectEqual(2, srcs[i].len);
            try testing.expectEqualStrings(srcs[i][0..1], token.loc.ws(src));
            try testing.expectEqualStrings(srcs[i][1..2], token.loc.src(src));
        } else {
            try testing.expectEqual(1, srcs[i].len);
            try testing.expectEqualStrings(srcs[i][0..1], token.loc.ws(src));
            break;
        }
    }
}

test "Token.Loc" {
    try testLoc(
        " ( [ ] { } ) ",
        &.{ " (", " [", " ]", " {", " }", " )", " " },
    );
}

const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;
const assert = std.debug.assert;
