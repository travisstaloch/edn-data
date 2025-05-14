//! a simple ringbuffer
//! based on https://www.snellman.net/blog/archive/2016-12-13-ring-buffers/

/// capacity is always a power of 2
pub fn RingBuffer(T: type, comptime capacity_log2: u8) type {
    return struct {
        read_idx: WideIndex,
        write_idx: WideIndex,
        buf: [capacity]T,

        pub const capacity: comptime_int = std.math.pow(usize, 2, capacity_log2);
        /// integer type with exclusive range [0, capacity)
        pub const Index = std.meta.Int(.unsigned, capacity_log2);
        /// integer type with exclusive range [0, capacity*2)
        pub const WideIndex = std.meta.Int(.unsigned, capacity_log2 + 1);
        const Self = @This();

        comptime {
            assert(std.math.maxInt(Index) == capacity - 1);
            assert(std.math.maxInt(WideIndex) == capacity * 2 - 1);
        }

        pub const init: Self = .{ .read_idx = 0, .write_idx = 0, .buf = undefined };

        pub fn push(self: *Self, val: T) !void {
            if (self.full()) return error.Full;
            defer self.write_idx +%= 1;
            self.buf[@as(Index, @truncate(self.write_idx))] = val;
        }

        pub fn pop(self: *Self) ?T {
            if (self.empty()) return null;
            defer {
                self.buf[@as(Index, @truncate(self.read_idx))] = undefined;
                self.read_idx +%= 1;
            }
            return self.buf[@as(Index, @truncate(self.read_idx))];
        }

        pub fn peek(self: *const Self) ?T {
            if (self.empty()) return null;
            return self.buf[@as(Index, @truncate(self.read_idx))];
        }

        pub fn empty(self: *const Self) bool {
            return self.write_idx == self.read_idx;
        }

        pub fn full(self: *const Self) bool {
            return self.len() == capacity;
        }

        pub fn len(self: *const Self) WideIndex {
            return self.write_idx -% self.read_idx;
        }
    };
}

fn basicTestRb(T: type, comptime cap_log2: usize) !void {
    const Rb = RingBuffer(T, cap_log2);
    var rb: Rb = .init;

    // initial, empty state
    try testing.expect(rb.empty());
    try testing.expect(!rb.full());
    try testing.expectEqual(0, rb.len());

    // push 3
    try rb.push(10);
    try testing.expectEqual(1, rb.len());
    try testing.expect(!rb.empty());
    try rb.push(20);
    try rb.push(30);
    try testing.expectEqual(3, rb.len());
    try testing.expectEqual(10, rb.peek());
    try testing.expectEqual(3, rb.len());

    // pop 3 -> empty
    try testing.expectEqual(10, rb.pop());
    try testing.expectEqual(20, rb.pop());
    try testing.expectEqual(30, rb.pop());
    try testing.expect(rb.empty());
    try testing.expectEqual(0, rb.len());
    try testing.expectEqual(null, rb.pop());
    try testing.expectEqual(null, rb.peek());

    // fill
    for (0..Rb.capacity) |i| try rb.push(@truncate(i));
    try testing.expectEqual(Rb.capacity, rb.len());
    try testing.expect(rb.full());
    try testing.expectError(error.Full, rb.push(100));

    // wrap-around
    for (0..Rb.capacity) |i| try testing.expectEqual(@as(T, @truncate(i)), rb.pop());
    try testing.expect(rb.empty());
}

fn fuzzTestRb(T: type, comptime cap_log2: usize) !void {
    // randomly push, pop or peek and check against reference ArrayList
    var rb: RingBuffer(T, cap_log2) = .init;
    const capacity = @TypeOf(rb).capacity;

    var reference = std.ArrayList(T).init(testing.allocator);
    defer reference.deinit();

    var prng = std.Random.DefaultPrng.init(0);
    const random = prng.random();

    for (0..1000) |_| {
        try testing.expectEqual(reference.items.len, rb.len());
        switch (random.uintLessThan(u8, 3)) {
            0 => { // push
                if (reference.items.len < capacity) {
                    const value = random.int(T);
                    try rb.push(value);
                    try reference.append(value);

                    try testing.expectEqual(reference.items.len, rb.len());
                    try testing.expect(!rb.empty());
                    try testing.expectEqual(reference.items.len == capacity, rb.full());
                } else {
                    try testing.expect(rb.full());
                    try testing.expectError(error.Full, rb.push(random.int(T)));
                }
            },
            1 => { // pop
                if (reference.items.len > 0) {
                    const expected = reference.orderedRemove(0);
                    const actual = rb.pop();

                    try testing.expectEqual(expected, actual);
                    try testing.expectEqual(reference.items.len, rb.len());
                    try testing.expectEqual(reference.items.len == 0, rb.empty());
                    try testing.expect(!rb.full());
                } else {
                    try testing.expect(rb.empty());
                    try testing.expectEqual(null, rb.pop());
                }
            },
            2 => { // peek
                if (reference.items.len > 0) {
                    const expected = reference.items[0];
                    const actual = rb.peek();

                    try testing.expectEqual(expected, actual);
                    try testing.expectEqual(reference.items.len, rb.len());
                } else {
                    try testing.expect(rb.empty());
                    try testing.expectEqual(null, rb.peek());
                }
            },
            else => unreachable,
        }
    }

    // check wrap-around behavior by filling, then emptying halfway, then filling again
    rb = .init;
    reference.clearRetainingCapacity();
    // fill
    for (0..capacity) |j| {
        try rb.push(@truncate(j));
        try reference.append(@truncate(j));
    }
    // empty half
    for (0..capacity / 2) |_| {
        try testing.expect(rb.pop() != null);
        _ = reference.orderedRemove(0);
    }
    // fill again
    for (0..capacity / 2) |j| {
        const value = @as(T, @truncate(j + 1000));
        try rb.push(value);
        try reference.append(value);
    }
    // check all
    while (reference.items.len > 0) {
        const expected = reference.orderedRemove(0);
        const actual = rb.pop();
        try testing.expectEqual(expected, actual);
    }
    try testing.expect(rb.empty());
    try testing.expect(reference.items.len == 0);
}

test "index wrapping" {
    // check that indices properly wrap around after many operations
    const Rb = RingBuffer(u8, 3);
    var rb = Rb.init;

    for (0..1000) |_| {
        for (0..Rb.capacity) |j| try rb.push(@truncate(j));
        for (0..Rb.capacity) |j| try testing.expectEqual(@as(u8, @truncate(j)), rb.pop());
        try testing.expect(rb.empty());
        try testing.expectEqual(0, rb.len());
    }
}

test "fuzz different capacities and types" {
    inline for (.{ u8, u16, u32, usize }) |T| {
        inline for (0..12) |i| {
            if (i > 1) try basicTestRb(T, i);
            try fuzzTestRb(T, i);
        }
    }
}

const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
