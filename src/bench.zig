const std = @import("std");

const edn = @import("extensible-data-notation");

pub const std_options = std.Options{ .log_level = .warn };

fn usage() void {
    std.debug.print(
        \\usage <options?> <file>
        \\
        \\benchmark edn file parsing
        \\
        \\options:
        \\  --json: bench json file parsing
        \\
    , .{});
}

pub fn main() !void {
    mainInner() catch |e| {
        std.debug.print("{s}\n", .{@errorName(e)});
        usage();
    };
}

fn mainInner() !void {
    var buf: [1024 * 1024 * 10]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const alloc = fba.allocator();
    const args = try std.process.argsAlloc(alloc);
    if (args.len < 2) return error.Args;

    if (std.mem.eql(u8, args[1], "--json")) {
        if (args.len < 3) return error.Args;
        const src = try std.fs.cwd().readFileAlloc(args[2], alloc, .unlimited);
        defer alloc.free(src);
        var timer = try std.time.Timer.start();
        const json = try std.json.parseFromSlice(std.json.Value, alloc, src, .{});
        std.mem.doNotOptimizeAway(json);
        std.debug.print("\nstd.json.parseFromSlice took {D}\n", .{timer.lap()});
        return;
    }
    if (std.mem.eql(u8, args[1], "-")) @panic("TODO support reading stdin");
    const src = try std.fs.cwd().readFileAllocOptions(args[1], alloc, .unlimited, .fromByteUnits(1), 0);
    var timer = try std.time.Timer.start();
    var diag: edn.Diagnostic = .{ .file_path = args[1] };
    const result = edn.parseFromSlice(edn.Result, src, .{ .diagnostic = &diag, .allocator = alloc, .preserve_whitespace = false }, .{}) catch {
        std.debug.print("{s}\n", .{diag.error_message});
        return;
    };
    std.mem.doNotOptimizeAway(result);
    std.debug.print("\nedn.parseFromSlice took {D}\n", .{timer.lap()});
}
