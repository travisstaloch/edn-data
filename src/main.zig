const std = @import("std");

const edn = @import("extensible-data-notation");

pub const std_options = std.Options{
    .log_level = .warn,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    const file = try std.fs.cwd().openFile(args[1], .{});
    const src = try file.readToEndAllocOptions(alloc, 1024 * 1024, null, .@"8", 0);
    defer alloc.free(src);
    var result = try edn.parseFromSliceAlloc(alloc, src, .{}, .{});
    defer result.deinit(alloc);
    try std.io.getStdOut().writer().print("{}", .{result.formatter(src)});
}
