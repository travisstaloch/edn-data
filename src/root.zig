//! https://github.com/edn-format/edn

tokenizer: Tokenizer,
res: Result,
depth: u16,
handlers: TaggedElementHandler.Map,
/// the last token seen.  used to track required whitespace between tokens.
last_token: Token = .{
    .tag = undefined,
    .loc = .{ .ws_start = 0, .start = 0, .end = std.math.maxInt(u32) },
},

pub fn init(
    src: [:0]const u8,
    comptime handlers: []const TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .res = .init,
        .handlers = .initComptime(handlers),
    };
}

pub fn initFixed(
    src: [:0]const u8,
    buffers: Buffers,
    comptime handlers: []const TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .res = .{
            .values = .initBuffer(buffers.values),
            .whitespaces = .initBuffer(buffers.whitespaces),
            .children = buffers.children,
            .siblings = .initBuffer(buffers.siblings),
        },
        .handlers = .initComptime(handlers),
    };
}

pub const Error = error{
    Parse,
    Eof,
    HandlerParse,
    MissingHandler,
    EndOfContainer,
    MissingWhitespaceBetweenValues,
    InvalidStruct,
    InvalidStructField,
    MissingFields,
    InvalidToken,
    InvalidChar,
    InvalidString,
    InvalidUnion,
    InvalidBool,
    InvalidArray,
    InvalidEnum,
    InvalidInt,
    InvalidFloat,
    UnclosedContainer,
    ExtraInput,
    InvalidInput,
    Depth,
    AllocatorRequired,
} ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError ||
    error{InvalidUTF8};

pub const Token = Tokenizer.Token;

pub const Diagnostic = struct {
    /// zero based
    line: u32 = 0,
    column: u32 = 0,
    /// user is responsible for setting file_path
    file_path: ?[]const u8 = null,
    /// on error, this will be populated
    error_message: [256]u8 = [1]u8{0} ** 256,
    // TODO user defined printFn?
};

pub const Span = [2]u32;

pub const Value = union(Value.Tag) {
    nil,
    true,
    false,
    string: Token,
    character: u21,
    keyword: Token,
    symbol: Token,
    tagged: struct { tag: Token, value: *Value },
    integer: i128,
    float: Token,
    list: List,
    vector: List,
    set: List,
    map: List,
    userdata: *anyopaque,

    pub const Tag = enum(u8) {
        nil,
        true,
        false,
        string,
        character,
        keyword,
        symbol,
        tagged,
        integer,
        float,
        // containers
        list,
        vector,
        set,
        map,
        // end containers
        userdata,
    };

    pub const List = struct {
        len: u32 = 0,
        trailing_ws: Span = .{ 0, 0 },
    };

    pub const Id = enum(u32) {
        null = std.math.maxInt(u32),
        _,
        pub fn int(id: Id) u32 {
            return @intFromEnum(id);
        }
    };

    pub const Ids = std.ArrayListUnmanaged(Id);

    pub fn eql(
        a: *const Value,
        a_src: [:0]const u8,
        a_result: Result,
        b: *const Value,
        b_src: [:0]const u8,
        b_result: Result,
    ) bool {
        return a.* == std.meta.activeTag(b.*) and
            switch (a.*) {
                .nil, .true, .false => true,
                .integer => a.integer == b.integer,
                .character => |c| c == b.character,
                .tagged => |tagged| mem.eql(u8, tagged.tag.src(a_src), b.tagged.tag.src(b_src)) and
                    tagged.value.eql(a_src, a_result, b.tagged.value, b_src, b_result),
                inline .float, .keyword, .symbol, .string => |payload, tag| mem.eql(
                    u8,
                    payload.src(a_src),
                    @field(b, @tagName(tag)).src(b_src),
                ),
                inline .vector, .list => |as, tag| blk: {
                    const bs = @field(b, @tagName(tag));
                    var aiter = a_result.iterator(a);
                    var biter = b_result.iterator(b);
                    break :blk as.len == bs.len and for (0..as.len) |_| {
                        const aa = a_result.items(.values, aiter.next() orelse break false);
                        const bb = b_result.items(.values, biter.next() orelse break false);
                        if (!eql(aa, a_src, a_result, bb, b_src, b_result)) break false;
                    } else true;
                },
                .set, .map => blk: {
                    const actx = MapContext{ .src = a_src, .result = a_result };
                    const ahash = actx.hash(a);
                    const bctx = MapContext{ .src = b_src, .result = b_result };
                    const bhash = bctx.hash(b);
                    break :blk ahash == bhash;
                },
                .userdata => unreachable,
                // else => std.debug.panic("TODO {s}", .{@tagName(a.*)}),
            };
    }

    pub fn formatter(value: *const Value, src: [:0]const u8, result: Result) std.fmt.Formatter(Data, formatValue) {
        return .{ .data = .{ .value = value, .src = src, .result = result } };
    }

    pub fn isContainer(v: Value) bool {
        const i = @intFromEnum(@as(Tag, v));
        return i >= @intFromEnum(Tag.list) and i < @intFromEnum(Tag.userdata);
    }

    pub fn listAt(v: *const Value, index: usize, result: Result) *const Value {
        return &result.values.items[v - result.values.items.ptr + index];
    }
};

pub const MapContext = struct {
    src: [:0]const u8,
    result: Result,

    fn hashValue(self: MapContext, v: *const Value, hptr: anytype) void {
        hptr.update(mem.asBytes(&@intFromEnum(v.*)));
        switch (v.*) {
            .nil, .true, .false => {},
            .character, .integer => |p| hptr.update(mem.asBytes(&p)),
            .float, .keyword, .symbol, .string => |token| hptr.update(token.src(self.src)),
            // these are equal if they have the same count of elements and, for
            // every element in one set, an equal element is in the other.
            .list, .vector, .set, .map => |*l| {
                hptr.update(mem.asBytes(&l.len));
                var iter = self.result.iterator(v);
                while (iter.next()) |cidx| self.hashValue(self.result.items(.values, cidx), hptr);
            },
            // maps are equal if they have the same number of entries, and for
            // every key/value entry in one map an equal key is present and
            // mapped to an equal value in the other.
            .tagged => |tagged| {
                hptr.update(tagged.tag.src(self.src));
                self.hashValue(tagged.value, hptr);
            },
            .userdata => unreachable,
            // else => std.debug.panic("TODO {s}", .{@tagName(v.*)}),
        }
    }

    pub fn hash(self: MapContext, v: *const Value) u32 {
        var h = std.hash.Wyhash.init(0);
        self.hashValue(v, &h);
        return @truncate(h.final());
    }

    pub fn eql(_: MapContext, _: Value, _: Value, _: usize) bool {
        unreachable;
    }
};

pub const TaggedElementHandler = struct {
    pub const Fn = *const fn (
        value: Value,
        value_src: []const u8,
        src: [:0]const u8,
        userdata: ?*anyopaque,
    ) Error!Value;

    /// initialization format: {tag_name, handler_fn}.
    /// tag_name must include leading '#'.
    pub const Data = struct { []const u8, Fn };

    pub const Map = std.StaticStringMap(Fn);
};

pub const Result = struct {
    values: Values,
    whitespaces: Whitespaces,
    children: std.DynamicBitSetUnmanaged,
    siblings: Value.Ids,

    pub const init: Result = .{
        .values = .{},
        .whitespaces = .{},
        .children = .{},
        .siblings = .{},
    };

    pub fn deinit(r: Result, alloc: Allocator) void {
        const rr: *Result = @constCast(&r);
        rr.values.deinit(alloc);
        rr.whitespaces.deinit(alloc);
        rr.children.deinit(alloc);
        rr.siblings.deinit(alloc);
    }

    fn findOne(component: []const u8, r: Result, src: [:0]const u8, value: *const Value) !*const Value {
        switch (value.*) {
            inline .list, .vector, .set => {
                const i = try std.fmt.parseInt(usize, component, 0);
                var iter = r.iterator(value);
                const id = iter.nth(i) orelse return error.PathNotFound;
                return r.items(.values, id);
            },
            .map => |map| {
                var iter = r.iterator(value);
                for (0..map.len) |_| {
                    const kid = iter.next() orelse return error.PathNotFound;
                    const vid = iter.next() orelse return error.PathNotFound;
                    const k = r.items(.values, kid);
                    const v = r.items(.values, vid);
                    switch (k.*) {
                        .string,
                        .keyword,
                        .symbol,
                        => |t| if (mem.eql(u8, component, t.src(src))) return v,
                        .integer => {
                            var buf: [std.math.log10(std.math.maxInt(i128)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "{f}", .{k.formatter(src, r)});
                            if (mem.eql(u8, component, s)) {
                                return v;
                            }
                        },
                        .character => {
                            var buf: [std.math.log(usize, 2, std.math.maxInt(u21)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "{f}", .{k.formatter(src, r)});
                            if (mem.eql(u8, component, s)) {
                                return v;
                            }
                        },
                        else => return error.PathNotFound,
                    }
                }
            },
            else => unreachable,
        }
        return error.PathNotFound;
    }

    /// path: search components joined by double slashes ('//'). i.e. '0//1//foo'
    /// search starts from top level which a list so first path component should
    /// be an integer index.
    pub fn find(r: Result, path: []const u8, src: [:0]const u8) !*const Value {
        var it = mem.splitSequence(u8, path, "//");
        const component0 = it.next() orelse return error.InvalidPath;
        var cur = try findOne(component0, r, src, &r.values.items[0]);
        while (it.next()) |component| {
            cur = try findOne(component, r, src, cur);
        }
        return if (it.next() != null) error.PathNotFound else cur;
    }

    pub const Data = struct { result: Result, src: [:0]const u8 };
    pub fn formatter(result: Result, src: [:0]const u8) std.fmt.Formatter(Result.Data, formatParseResult) {
        return .{ .data = .{ .src = src, .result = result } };
    }

    pub const Iterator = struct {
        result: Result,
        id: Value.Id,

        pub fn next(iter: *Iterator) ?Value.Id {
            if (iter.id == .null) return null;
            defer iter.id = iter.result.items(.siblings, iter.id).*;
            return iter.id;
        }

        /// n is zero based meaning 0 returns first child
        pub fn nth(iter: *Iterator, n: usize) ?Value.Id {
            for (0..n) |_| _ = iter.next();
            return iter.next();
        }
    };

    /// visits each child of parent.  does not descend to grand children.
    pub fn iterator(result: Result, parent: *const Value) Iterator {
        const id = parent - result.values.items.ptr;
        return .{ .result = result, .id = if (result.children.isSet(id)) @enumFromInt(id + 1) else .null };
    }

    pub fn firstValue(r: Result) *const Value {
        return &r.values.items[1];
    }

    fn ItemsField(T: type, field: std.meta.FieldEnum(Result)) type {
        return @typeInfo(@FieldType(@FieldType(T, @tagName(field)), "items")).pointer.child;
    }

    pub fn items(r: anytype, comptime field: std.meta.FieldEnum(Result), id: Value.Id) *ItemsField(Result, field) {
        return &@field(r, @tagName(field)).items[id.int()];
    }
};

pub const Options = struct { // TODO use packed struct and workaround compiler issues here
    userdata: ?*anyopaque = null,
    /// when set line and column will be populated on error and an error message
    /// with format <file_path>:<line>:<column> will be printed to diagnostic.error_message.
    /// user is responsible for setting Diagnostic.file_path.
    diagnostic: ?*Diagnostic = null,
    allocator: ?Allocator = null,
    /// whether to save whitespace.  when false ParseResult.whitespaces will be
    /// empty and each whitespace will be formatted as a single space.
    preserve_whitespace: bool = true,
    // TODO possibly merge store and allocate?
    /// false to measure by updating result lengths without allocating.
    allocate: bool = true,
    /// users generally won't need to set this option.  its used internally to
    /// skip storing/measuring depending on 'allocate' option.
    /// when false discard values by parsing without storing them.
    store: bool = true,
    /// stride = 2 is used to parse maps
    stride: u6 = 1,
    /// when true make sure the last token is eof.  only used when parsing
    /// structured input instead of non-arbitrary input as with edn.Result.
    error_on_extra_input: bool = true,

    /// returns a copy of 'self' with fields from 'other' which don't have their default value.
    pub fn with(self: Options, other: Options) Options {
        var result = self;
        inline for (@typeInfo(Options).@"struct".fields) |f| {
            switch (@typeInfo(f.type)) {
                .optional => if (@field(other, f.name) != null) {
                    @field(result, f.name) = @field(other, f.name);
                },
                else => if (@field(other, f.name) != f.defaultValue().?) {
                    @field(result, f.name) = @field(other, f.name);
                },
            }
        }
        return result;
    }
};

pub const Buffers = struct {
    values: []Value,
    whitespaces: []Span,
    children: std.DynamicBitSetUnmanaged,
    siblings: []Value.Id,
};

pub const Shape = struct {
    /// total number of values in Result
    capacity: u32 = 0,

    pub inline fn Arrays(comptime s: Shape) type {
        const MaskInt = std.DynamicBitSetUnmanaged.MaskInt;
        return struct {
            values: [s.capacity]Value,
            whitespace: [s.capacity]Span,
            children: [(s.capacity + @sizeOf(MaskInt) - 1) / @sizeOf(MaskInt)]MaskInt,
            siblings: [s.capacity]Value.Id,

            pub fn buffers(arrays: *@This()) Buffers {
                @memset(&arrays.children, 0);
                return .{
                    .values = &arrays.values,
                    .whitespaces = &arrays.whitespace,
                    .children = .{ .bit_length = arrays.values.len, .masks = &arrays.children },
                    .siblings = &arrays.siblings,
                };
            }
        };
    }
};

pub fn measure(
    src: [:0]const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Shape {
    var p = try Parser.init(src, comptime_options.handlers);
    const opts = options.with(.{ .allocate = false });
    const id = p.addValue(.{ .tag = .lparen, .loc = .zero }, .{ .list = .{} }, .null, opts);
    _ = try p.parseList(.eof, &.{ .rparen, .rbracket, .rcurly }, id, opts);
    if (TRACE) trace("measure {}", .{p.res.values.items.len});
    return .{
        .capacity = @intCast(p.res.values.items.len),
    };
}

pub const Values = std.ArrayListUnmanaged(Value);
pub const Whitespaces = std.ArrayListUnmanaged(Span);

/// assumes that slices are large enough to hold the parse results.
/// users may call measure() to get the necessary sizes.
pub fn parseFromSliceBuf(
    src: [:0]const u8,
    shape: Shape,
    buffers: Buffers,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Result {
    var p = try Parser.initFixed(src, buffers, comptime_options.handlers);
    const id = addValue(&p, .{ .tag = .lparen, .loc = .zero }, .{ .list = .{} }, .null, options);
    if (src.len == 0) return p.res;
    const list = try parseList(&p, .eof, &.{ .rparen, .rbracket, .rcurly }, id, options);
    if (options.allocate) p.res.items(.values, id).* = .{ .list = list };

    if (TRACE) trace("shape.values {} parsed values count {} top level count {}", .{ shape.capacity, p.res.values.items.len, list.len });

    // TODO should this be assert(last_token.tag == .eof) instead of an error?
    if (p.tokenizer.index != 0 and p.tokenizer.peek().tag != .eof)
        return error.ExtraInput;

    return p.res;
}

fn parseFromSliceAlloc(
    src: [:0]const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Result {
    const allocator = options.allocator orelse return error.AllocatorRequired;
    const shape = try measure(src, options, comptime_options);

    const values = try allocator.alloc(Value, shape.capacity);
    errdefer allocator.free(values);
    const whitespaces: []Span = if (options.preserve_whitespace)
        try allocator.alloc(Span, shape.capacity)
    else
        &.{};
    errdefer allocator.free(whitespaces);
    var children = try std.DynamicBitSetUnmanaged.initEmpty(allocator, shape.capacity);
    errdefer children.deinit(allocator);
    const siblings = try allocator.alloc(Value.Id, shape.capacity);
    errdefer allocator.free(siblings);

    return try parseFromSliceBuf(src, shape, .{
        .values = values,
        .whitespaces = whitespaces,
        .children = children,
        .siblings = siblings,
    }, options, comptime_options);
}

pub const ComptimeOptions = struct {
    eval_branch_quota: u32 = 1000,
    handlers: []const TaggedElementHandler.Data = &.{},
};

pub inline fn parseFromSliceComptime(
    comptime src: [:0]const u8,
    comptime options: Options,
    comptime comptime_options: ComptimeOptions,
) !Result {
    comptime {
        @setEvalBranchQuota(comptime_options.eval_branch_quota);
        const shape = try measure(src, options, comptime_options);
        if (TRACE) trace("parseFromSliceComptime cap {}", .{shape.capacity});
        var arrays: shape.Arrays() = undefined;
        return try parseFromSliceBuf(src, shape, arrays.buffers(), options, comptime_options);
    }
}

/// T = Result uses options.allocator to parse arbitrary shaped data.
/// performs two parsing passes first measure and then store.
/// and the second allocates and write to those entries using parseFromSliceBuf()
pub fn parseFromSlice(comptime T: type, src: [:0]const u8, options: Options, comptime comptime_options: ComptimeOptions) Error!T {
    var p = try Parser.init(src, comptime_options.handlers);
    const t = if (T == Result)
        try parseFromSliceAlloc(src, options, comptime_options)
    else t: {
        const t = try parseType(T, null, &p, options.with(.{ .allocate = false }));
        if (options.error_on_extra_input and
            (p.tokenizer.index == 0 or !p.tokenizer.isTag(.eof)))
            return error.ExtraInput;
        break :t t;
    };
    return t;
}

inline fn unsupportedType(comptime T: type) noreturn {
    @compileError("unsupported type '" ++ @typeName(T) ++ "'");
}

inline fn isContainer(T: type) bool {
    comptime {
        const info = @typeInfo(T);
        return info == .@"struct" or info == .@"union" or info == .@"enum" or info == .@"opaque";
    }
}

fn parseType(
    comptime T: type,
    comptime P: ?type, // Parent type
    p: *Parser,
    options: Options,
) Error!T {
    if (isContainer(T) and @hasDecl(T, "ednParse")) {
        return T.ednParse(p, options.userdata, options);
    }

    if (p.tokenizer.index != 0 and p.tokenizer.isTag(.tagged)) {
        // support tagged element handlers
        // TODO skip discards
        const tag = p.tokenizer.next();

        const tagged = p.tokenizer.next();
        _, const next = try p.parseValue(tagged, .null, options);
        if (P) |Parent| {
            comptime assert(isContainer(Parent));
            if (@hasDecl(Parent, "ednTagHandler")) {
                return Parent.ednTagHandler(
                    T,
                    tag.src(p.tokenizer.src),
                    tagged.src(p.tokenizer.src),
                    p.tokenizer.src,
                    next,
                    options.userdata,
                );
            }
        }
        return error.MissingHandler;
    }

    return switch (@typeInfo(T)) {
        .@"struct" => parseStruct(T, p, options),
        .@"union" => u: {
            const Fe = std.meta.FieldEnum(T);
            _ = try expectNext(.lcurly, error.InvalidUnion, p);
            const c = p.tokenizer.next();
            const field = switch (c.tag) {
                .keyword => blk: {
                    const key = Value{ .keyword = c };
                    break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                        return error.InvalidStructField;
                },
                else => {
                    return error.InvalidUnion;
                },
            };
            if (TRACE) trace("{s: <[1]}parseType union key {2s}", .{ "", p.depth * 2, @tagName(field) });
            const u = switch (field) {
                inline else => |tag| @unionInit(
                    T,
                    @tagName(tag),
                    try parseType(@FieldType(T, @tagName(tag)), T, p, options),
                ),
            };
            if (consume(.rcurly, p) == null) return error.InvalidUnion;
            break :u u;
        },
        .bool => blk: {
            const t = p.tokenizer.next();
            if (t.tag == .true) {
                break :blk true;
            } else if (t.tag == .false) {
                break :blk false;
            } else break :blk error.InvalidBool;
        },
        .int => blk: {
            const t = try expectNext(.int, error.InvalidInt, p);
            break :blk try std.fmt.parseInt(T, t.src(p.tokenizer.src), 0);
        },
        .float => blk: {
            const t = p.tokenizer.next();
            if (t.tag != .float and t.tag != .int) return error.InvalidFloat;
            break :blk std.fmt.parseFloat(T, t.src(p.tokenizer.src));
        },
        .pointer => |i| blk: switch (i.size) {
            .slice => switch (i.child) {
                u8 => {
                    const t = try expectNext(.str, error.InvalidString, p);
                    const s = Value{ .string = t };
                    break :blk s.string.src(p.tokenizer.src);
                },
                else => unsupportedType(T),
            },
            else => unsupportedType(T),
        },
        .optional => |i| blk: {
            const t = p.tokenizer.peek();
            break :blk switch (t.tag) {
                .nil => {
                    _ = p.tokenizer.next();
                    break :blk null;
                },
                else => try parseType(i.child, P, p, options),
            };
        },
        .@"enum" => blk: {
            const t = try expectNext(.keyword, error.InvalidEnum, p);
            const key = Value{ .keyword = t };
            if (TRACE) trace("enum key {s}", .{key.keyword.src(p.tokenizer.src)});
            break :blk std.meta.stringToEnum(T, key.keyword.src(p.tokenizer.src)[1..]) orelse
                error.InvalidEnum;
        },
        inline .array, .vector => |i| blk: {
            var a: [i.len]i.child = undefined;
            _ = try expectNext(.lbracket, error.InvalidArray, p);
            for (&a) |*ele| {
                ele.* = try parseType(i.child, P, p, options);
            }
            _ = try expectNext(.rbracket, error.InvalidArray, p);
            break :blk a;
        },
        else => unsupportedType(T),
    };
}

fn parseStruct(comptime T: type, p: *Parser, options: Options) !T {
    const Fe = std.meta.FieldEnum(T);
    var seen_fields = std.enums.EnumSet(Fe).initEmpty();

    if (TRACE) trace("{s: <[1]}parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    defer if (TRACE) trace("{s: <[1]}<parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    p.depth += 1;
    defer p.depth -= 1;
    _ = try expectNext(.lcurly, error.InvalidStruct, p);

    var t: T = undefined;

    while (true) {
        const c = p.tokenizer.next();
        if (TRACE) trace("{s: <[1]}parseStruct '{2s}'", .{ "", p.depth * 2, @tagName(c.tag) });
        if (c.tag == .rcurly) break;

        const field = switch (c.tag) {
            .keyword => blk: {
                const key = Value{ .keyword = c };
                break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                    return error.InvalidStructField;
            },
            else => return error.InvalidStruct,
        };
        if (TRACE) trace("{s: <[1]}parseStruct key {2s}", .{ "", p.depth * 2, @tagName(field) });

        switch (field) {
            inline else => |tag| {
                @field(t, @tagName(tag)) = try parseType(@FieldType(T, @tagName(tag)), T, p, options);
            },
        }
        seen_fields.insert(field);
    }

    var unseen_iter = seen_fields.complement().iterator();
    while (unseen_iter.next()) |field| switch (field) {
        inline else => |tag| {
            const info = std.meta.fieldInfo(T, tag);
            if (info.defaultValue()) |default| {
                @field(t, @tagName(tag)) = default;
                seen_fields.insert(tag);
            } else {
                return error.MissingFields;
            }
        },
    };
    if (seen_fields.complement().count() != 0) return error.MissingFields;
    return t;
}

fn formatParseResult(data: Result.Data, writer: *std.io.Writer) std.io.Writer.Error!void {
    if (data.result.values.items.len <= 1 or
        data.result.values.items[0] != .list or
        data.result.values.items[0].list.len == 0) return;
    try formatValue(.{
        .value = &data.result.values.items[0],
        .src = data.src,
        .result = data.result,
    }, writer);
}

const Data = struct { value: *const Value, src: [:0]const u8, result: Result };
fn formatValue(
    data: Data,
    writer: *std.io.Writer,
) std.io.Writer.Error!void {
    const have_ws = data.result.whitespaces.items.len > 0;
    const index = data.value - data.result.values.items.ptr;
    // std.debug.print("{s} index {} data.value {*}\n", .{ @tagName(data.value.*), index, data.value });
    if (index >= data.result.values.items.len) return; // corrupt data
    if (have_ws and index < data.result.whitespaces.items.len) {
        // print leading whitespace
        const ws = data.result.whitespaces.items[index];
        try writer.writeAll(data.src[ws[0]..ws[1]]);
    }
    const v = data.value.*;
    switch (v) {
        .true,
        .false,
        .nil,
        => try writer.writeAll(@tagName(v)),
        .string,
        .keyword,
        .symbol,
        .float,
        => |t| try writer.writeAll(t.src(data.src)),
        .character => |c| switch (c) {
            ' ' => try writer.writeAll("\\space"),
            '\n' => try writer.writeAll("\\newline"),
            '\t' => try writer.writeAll("\\tab"),
            '\r' => try writer.writeAll("\\return"),
            else => if (c < 256 and !std.ascii.isPrint(@truncate(c)))
                try writer.print("\\u{x:0>4}", .{c})
            else if (c >= 256)
                try writer.print("\\u{u}", .{c})
            else
                try writer.print("\\{u}", .{c}),
        },
        .integer => |x| try writer.printInt(x, 10, .lower, .{}),
        .tagged => |*tagged| {
            const src = tagged.tag.src(data.src);
            try writer.writeAll(src);
            // FIXME - this hack allows '#foo 1' to print correctly instead of '#foo  1' when .whitespace = false.
            if (!have_ws and (src.len == 0 or !std.ascii.isWhitespace(src[src.len - 1]))) {
                try writer.writeByte(' ');
            }
            try writer.print("{f}", .{tagged.value.formatter(data.src, data.result)});
        },
        inline .list, .vector, .set, .map => |l, tag| {
            // std.debug.print("{} {*} {*}\n", .{ index, data.value, data.result.values.items.ptr });
            if (!have_ws and index != 0) {
                switch (tag) {
                    .list => try writer.writeByte('('),
                    .vector => try writer.writeByte('['),
                    .set => try writer.writeAll("#{"),
                    .map => try writer.writeByte('{'),
                    else => {},
                }
            }

            // visit children
            var iter = data.result.iterator(data.value);
            var i: usize = 0;
            while (iter.next()) |child_id| : (i += 1) {
                const child = data.result.items(.values, child_id);
                if (!have_ws and i > 0) try writer.writeByte(' ');
                try writer.print("{f}", .{child.formatter(data.src, data.result)});
            }
            try writer.writeAll(data.src[l.trailing_ws[0]..l.trailing_ws[1]]);
        },
        .userdata => try writer.writeAll("__userdata__"),
    }
}

fn consume(tag: Token.Tag, p: *Parser) ?Token {
    if (p.tokenizer.isTag(tag)) {
        return p.tokenizer.next();
    }
    return null;
}

fn expectNext(tag: Token.Tag, e: Error, p: *Parser) Error!Token {
    const t = p.tokenizer.next();
    if (t.tag != tag) {
        if (TRACE) trace("expectNext wanted '{s}' got '{s}'", .{ @tagName(tag), t.tag.lexeme() });
        return e;
    }
    return t;
}

fn parseInt(p: *Parser, t: Token) !Value {
    if (TRACE) trace("{s: <[1]}parseInt('{2s}')", .{ "", p.depth * 2, t.src(p.tokenizer.src) });
    assert(t.tag == .int);
    return;
}

// Characters are preceded by a backslash: \c, \newline, \return, \space
// and \tab yield the corresponding characters. Unicode characters are
// represented with \uNNNN as in Java. Backslash cannot be followed by
// whitespace.
fn parseChar(p: *Parser, t: Token) !Value {
    if (TRACE) trace("{s: <[1]}parseChar()", .{ "", p.depth * 2 });
    assert(t.tag == .char);
    const src0 = t.loc.src(p.tokenizer.src);
    assert(src0[0] == '\\');
    const src = src0[1..];

    // tokenizer checks these so we don't need to validate here
    const char: u21 = switch (src.len) {
        1 => src[0],
        3 => blk: {
            assert(mem.eql(u8, src, "tab"));
            break :blk '\t';
        },
        5 => switch (src[0]) {
            'u' => try std.fmt.parseInt(u21, src[1..], 16),
            's' => blk: {
                assert(mem.eql(u8, src, "space"));
                break :blk ' ';
            },
            else => unreachable,
        },
        6 => blk: {
            assert(mem.eql(u8, src, "return"));
            break :blk '\r';
        },
        7 => blk: {
            assert(mem.eql(u8, src, "newline"));
            break :blk '\n';
        },
        else => return error.InvalidChar,
    };
    if (TRACE) trace("{s: <[1]}parseChar() result {2}:'{2u}'", .{ "", p.depth * 2, char });

    return .{ .character = char };
}

fn addValue(p: *Parser, t: Token, v: Value, parent: Value.Id, options: Options) Value.Id {
    const id: Value.Id = @enumFromInt(p.res.values.items.len);
    if (options.allocate) {
        if (TRACE) trace("{s: <[1]}storeValue('{2s}') id {3}", .{ "", p.depth * 2, t.src(p.tokenizer.src), id });
        p.res.values.appendAssumeCapacity(v);
        if (options.preserve_whitespace) p.res.whitespaces.appendAssumeCapacity(.{ t.loc.ws_start, t.loc.start });
        p.res.siblings.appendAssumeCapacity(.null);
        if (parent != .null) {
            const pv = p.res.items(.values, parent);
            switch (pv.*) {
                inline .list, .vector, .set, .map => |_, tag| {
                    @field(pv, @tagName(tag)).len += 1;
                    if (!p.res.children.isSet(parent.int())) {
                        // If parent has no children yet, this is the first child
                        p.res.children.set(parent.int());
                    } else {
                        // Otherwise, find the last sibling and update its next_sibling
                        var child_id: Value.Id = @enumFromInt(parent.int() + 1);
                        while (p.res.items(.siblings, child_id).* != .null) {
                            child_id = p.res.items(.siblings, child_id).*;
                        }
                        p.res.items(.siblings, child_id).* = id;
                    }
                },
                .tagged => {
                    // don't need to `p.res.children.set(parent.int())` here.
                    // tagged elements don't use this info and this can cause
                    // crashes on adverse inputs.
                },
                else => unreachable,
            }
        }
    } else {
        const inc = @intFromBool(t.tag != .discard);
        p.res.values.items.len += inc;
        p.res.whitespaces.items.len += inc;
        p.res.siblings.items.len += inc;
    }

    if (options.preserve_whitespace) assert(p.res.values.items.len == p.res.whitespaces.items.len);
    assert(p.res.values.items.len == p.res.siblings.items.len);
    return id;
}

/// handles lists, vectors, sets and maps.
/// prefix.tag == .eof is used at top level which is treated as an implicit
/// list with no parens. maps are parsed with options.stride = 2.  discards with
/// options.store = false.
fn parseList(
    p: *Parser,
    end_tag: Token.Tag,
    invalid_tags: []const Token.Tag,
    parent: Value.Id,
    options: Options,
) Error!Value.List {
    p.depth += 1;
    defer p.depth -= 1;

    var list: Value.List = .{};
    outer: while (true) : (list.len += 1) {
        for (0..options.stride) |_| {
            const t = p.tokenizer.next();
            // if (TRACE) trace("{s: <[1]}parseList('{2s}') {3}", .{ "", p.depth * 2, t.src(p.tokenizer.src), t.tag });
            if (t.tag == end_tag) {
                list.trailing_ws = .{ t.loc.ws_start, t.loc.end };
                p.last_token = t;
                break :outer;
            } else if (mem.indexOfScalar(Token.Tag, invalid_tags, t.tag)) |_| {
                list.trailing_ws = .{ t.loc.ws_start, t.loc.end };
                p.last_token = t;
                return error.UnclosedContainer;
            }
            _ = p.parseValue(t, parent, options) catch |e| switch (e) {
                error.EndOfContainer, error.Eof => {
                    list.trailing_ws = .{ p.last_token.loc.ws_start, p.last_token.loc.end };
                    break :outer;
                },
                else => return e,
            };
        }
    }

    return list;
}

fn parseTagged(p: *Parser, t: Token, parent: Value.Id, options: Options) Error!Value {
    // Upon encountering a tag, the reader will first read the
    // next element (which may itself be or comprise other
    // tagged elements), then pass the result to the
    // corresponding handler for further interpretation, and
    // the result of the handler will be the data value yielded
    // by the tag + tagged element, i.e. reading a tag and
    // tagged element yields one value.
    // If a reader encounters a tag for which no handler is
    // registered, the implementation can either report an error,
    // call a designated 'unknown element' handler, or create a
    // well-known generic representation that contains both the
    // tag and the tagged element, as it sees fit. Note that the
    // non-error strategies allow for readers which are capable of
    // reading any and all edn, in spite of being unaware of the
    // details of any extensions present.
    // Current strategy is to ignore the tagged symbol, adding it
    // to leading ws and fall back on default value parsing when
    // no handler is present.

    p.depth += 1;
    defer p.depth -= 1;
    const t2 = p.tokenizer.next();
    const next_id, const next = p.parseValue(t2, parent, options) catch |e| switch (e) {
        error.Eof, error.EndOfContainer => return error.InvalidInput,
        else => return e,
    };
    if (TRACE) trace("tagged {s} '{s}'\n", .{ @tagName(t2.tag), t2.src(p.tokenizer.src) });
    const v = if (p.handlers.get(t.src(p.tokenizer.src))) |handler| v: {
        const cur = p.tokenizer.peek();
        const v = try handler(
            next,
            p.tokenizer.src[t2.loc.start..cur.loc.ws_start],
            p.tokenizer.src,
            options.userdata,
        );
        break :v v;
    } else next;

    if (options.allocate and options.store) {
        p.res.items(.values, parent).tagged = .{ .tag = t, .value = p.res.items(.values, next_id) };
        p.res.items(.values, next_id).* = v;
    }
    return v;
}

// TODO allow users to skip expensive parsing such as parsing integers
pub fn userParseValue(p: *Parser, options: Options) !Value {
    _, const val = try p.parseValue(p.tokenizer.next(), .null, options);
    return val;
}

fn parseValue(p: *Parser, t: Token, parent: Value.Id, options: Options) Error!struct { Value.Id, Value } {
    return p.parseValueInner(t, parent, options) catch |e| {
        @branchHint(.cold);
        p.writeDiagnostic(e, if (p.tokenizer.index == 0) .{ .tag = .eof, .loc = std.mem.zeroes(Token.Loc) } else p.tokenizer.peek(), options);
        return e;
    };
}

fn parseValueInner(p: *Parser, t: Token, parent: Value.Id, options: Options) Error!struct { Value.Id, Value } {
    // std.debug.print("{s: <[1]}{2s}:\n  ws '{3s}'\n  content '{4s}'\n", .{ "", p.depth * 2, t.tag.lexeme(), p.tokenizer.src[t.loc.ws_start..t.loc.start], t.src(p.tokenizer.src) });
    if (p.depth == 255) return error.Depth;
    if (TRACE) trace("{s: <[1]}parseValueInner('{2s}')", .{ "", p.depth * 2, t.src(p.tokenizer.src) });
    if (p.last_token.loc.end == t.loc.start and
        !t.tag.isClosing() and
        !p.last_token.tag.isOpening())
    {
        return error.MissingWhitespaceBetweenValues;
    }
    p.last_token = t;

    const ws = Token.Loc{ .ws_start = t.loc.ws_start, .start = t.loc.end, .end = t.loc.end };
    const value: Value = switch (t.tag) {
        .nil => .nil,
        .true => .true,
        .false => .false,
        .int => .{ .integer = try std.fmt.parseInt(i128, t.src(p.tokenizer.src), 0) },
        .float => .{ .float = t },
        .str => .{ .string = t },
        .char => try p.parseChar(t),
        .discard => {
            const t2 = p.tokenizer.next();
            const tmerged = merged: {
                p.depth += 1;
                defer p.depth -= 1;
                _, const discard = p.parseValueInner(t2, parent, options.with(.{ .store = false })) catch |e| switch (e) {
                    error.Eof, error.EndOfContainer => return error.InvalidInput,
                    else => return e,
                };
                // merge tokens. include discard in whitespace.
                const t3 = p.tokenizer.next();
                const tmerged: Token = .{ .tag = t3.tag, .loc = .{
                    .ws_start = t.loc.ws_start,
                    .start = t3.loc.start,
                    .end = t3.loc.end,
                } };
                p.last_token = tmerged;
                if (TRACE) trace("{s: <[1]}discarded {2s} '{3s}' merged ws '{4s}' '{5s}'", .{ "", p.depth * 2, @tagName(discard), t2.src(p.tokenizer.src), @tagName(tmerged.tag), p.tokenizer.src[tmerged.loc.ws_start..tmerged.loc.start] });
                break :merged tmerged;
            };
            return try p.parseValueInner(tmerged, parent, options);
        },
        .tagged => {
            const id = p.addValue(t, .{ .tagged = undefined }, parent, options);
            const value = p.parseTagged(t, id, options) catch |e| switch (e) {
                error.Eof, error.EndOfContainer => return error.InvalidInput,
                else => return e,
            };
            return .{ id, value };
        },
        .keyword => .{ .keyword = t },
        .symbol => .{ .symbol = t },
        .lparen => {
            const id = p.addValue(.{ .tag = t.tag, .loc = ws }, .{ .list = .{} }, parent, options);
            const v = Value{ .list = try p.parseList(.rparen, &.{ .rbracket, .rcurly, .eof }, id, options) };
            if (options.allocate) p.res.items(.values, id).* = v;
            return .{ id, v };
        },
        .lbracket => {
            const id = p.addValue(.{ .tag = t.tag, .loc = ws }, .{ .vector = .{} }, parent, options);
            const v = Value{ .vector = try p.parseList(.rbracket, &.{ .rparen, .rcurly, .eof }, id, options) };
            if (options.allocate) p.res.items(.values, id).* = v;
            return .{ id, v };
        },
        .set => {
            const id = p.addValue(.{ .tag = t.tag, .loc = ws }, .{ .set = .{} }, parent, options);
            const v = Value{ .set = try p.parseList(.rcurly, &.{ .rparen, .rbracket, .eof }, id, options) };
            if (options.allocate) p.res.items(.values, id).* = v;
            return .{ id, v };
        },
        .lcurly => {
            const id = p.addValue(.{ .tag = t.tag, .loc = ws }, .{ .map = .{} }, parent, options);
            const v = Value{ .map = try p.parseList(.rcurly, &.{ .rparen, .rbracket, .eof }, id, options.with(.{ .stride = 2 })) };
            if (options.allocate) p.res.items(.values, id).* = v;
            return .{ id, v };
        },
        .rcurly, .rparen, .rbracket => return error.EndOfContainer,
        .eof => return error.Eof,
        .invalid => return error.InvalidToken,
    };

    return .{
        if (options.store) blk: {
            break :blk p.addValue(t, value, parent, options);
        } else @enumFromInt(p.res.values.items.len),
        value,
    };
}

fn writeDiagnostic(p: *const Parser, e: Error, t: Token, options: Options) void {
    if (options.diagnostic) |diag| { // user wants a diagnostic
        diag.* = .{ .line = 0, .column = 0 };
        for (p.tokenizer.src[0..t.loc.start]) |c| {
            if (c == '\n') {
                diag.line += 1;
                diag.column = 0;
            } else {
                diag.column += 1;
            }
        }
        var fbs = std.io.fixedBufferStream(&diag.error_message);
        if (diag.file_path) |file_path| {
            fbs.writer().print("error: {s}: {}:{} {s}. source '{s}'.\n", .{
                file_path,
                diag.line,
                diag.column,
                @errorName(e),
                t.src(p.tokenizer.src),
            }) catch {};
        } else {
            fbs.writer().print("error: {}:{} {s}. source '{s}'.\n", .{
                diag.line,
                diag.column,
                @errorName(e),
                t.src(p.tokenizer.src),
            }) catch {};
        }
        fbs.writer().writeByte(0) catch {};
    }
}

pub const StringifyOptions = struct {
    whitespace: enum {
        minified,
        indent_1,
        indent_2,
        indent_3,
        indent_4,
        indent_8,
        indent_tab,
    } = .minified,
};

fn indent(writer: *std.Io.Writer, options: StringifyOptions, depth: u16) !void {
    var char: u8 = ' ';
    const n_chars = switch (options.whitespace) {
        .minified => return,
        .indent_1 => 1 * depth,
        .indent_2 => 2 * depth,
        .indent_3 => 3 * depth,
        .indent_4 => 4 * depth,
        .indent_8 => 8 * depth,
        .indent_tab => blk: {
            char = '\t';
            break :blk depth;
        },
    };
    try writer.writeByte('\n');
    try writer.splatByteAll(char, n_chars);
}

pub fn printFromJson(v: std.json.Value, writer: anytype, options: StringifyOptions) !void {
    try printFromJsonInner(v, writer, options, 0);
}

fn printFromJsonInner(v: std.json.Value, writer: *std.Io.Writer, options: StringifyOptions, depth: u16) !void {
    switch (v) {
        .string => |bytes| {
            try writer.writeAll("\"");
            var iter = std.unicode.Utf8Iterator{ .bytes = bytes, .i = 0 };
            while (iter.nextCodepoint()) |cp| switch (cp) {
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                '\\' => try writer.writeAll("\\\\"),
                '"' => try writer.writeAll("\\\""),
                else => try writer.print("{u}", .{cp}),
            };
            try writer.writeAll("\"");
        },
        .integer => |i| try writer.printInt(i, 10, .lower, .{}),
        .float => |f| try writer.printFloat(f, .{ .mode = .decimal }),
        .bool => |b| try writer.writeAll(if (b) "true" else "false"),
        .null => try writer.writeAll("nil"),
        .object => |o| {
            try writer.writeAll("{");
            for (o.unmanaged.keys(), o.unmanaged.values(), 0..) |k, vv, i| {
                if (i != 0) try writer.writeByte(' ');
                try indent(writer, options, depth + 1);
                try writer.writeAll(":");
                try writer.writeAll(k);
                try writer.writeByte(' ');
                try printFromJsonInner(vv, writer, options, depth + 1);
            }
            try indent(writer, options, depth);
            try writer.writeAll("}");
        },
        .array => |a| {
            try writer.writeAll("[");
            for (a.items, 0..) |vv, i| {
                if (i != 0) try writer.writeByte(' ');
                try indent(writer, options, depth + 1);
                try printFromJsonInner(vv, writer, options, depth + 1);
            }
            try indent(writer, options, depth);
            try writer.writeAll("]");
        },
        .number_string => |s| try writer.writeAll(s),
        // else => std.debug.panic("writeJson TODO '{s}'\n", .{@tagName(v)}),
    }
}

const TRACE = false;
const log = std.log.scoped(.edn);
fn trace(comptime fmt: []const u8, args: anytype) void {
    _ = fmt; // autofix
    _ = args; // autofix
    if (@inComptime()) {
        // @compileLog(std.fmt.comptimePrint(fmt, args));
    } else {
        // log.debug(fmt, args);
        // std.debug.print(fmt ++ "\n", args);
    }
}

pub const Tokenizer = @import("Tokenizer.zig");
pub const Parser = @This();

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
