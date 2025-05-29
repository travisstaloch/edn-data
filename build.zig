const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("extensible-data-notation", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{},
    });

    const exe = b.addExecutable(.{
        .name = "edn-parse",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("extensible-data-notation", mod);
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // tests
    const test_step = b.step("test", "Run unit tests");
    const filters = b.option([]const []const u8, "test-filters", "") orelse &.{};
    const main_tests = try addTest("src/tests.zig", "tests", b, target, optimize, filters, test_step);
    main_tests.root_module.addImport("extensible-data-notation", mod);
    _ = try addTest("src/ringbuffer.zig", "test-ringbuffer", b, target, optimize, filters, test_step);
    _ = try addTest("src/Tokenizer.zig", "test-Tokenizer", b, target, optimize, filters, test_step);
    _ = try addTest("src/root.zig", "test-root", b, target, optimize, filters, test_step);
}
fn addTest(
    file_path: []const u8,
    name: []const u8,
    b: *std.Build,
    target: anytype,
    optimize: anytype,
    filters: anytype,
    test_step: anytype,
) !*std.Build.Step.Compile {
    const tests = b.addTest(.{
        .root_source_file = b.path(file_path),
        .target = target,
        .optimize = optimize,
        .name = name,
    });
    tests.filters = filters;
    const run_tests = b.addRunArtifact(tests);
    run_tests.has_side_effects = true;
    test_step.dependOn(&run_tests.step);
    b.installArtifact(tests);
    return tests;
}
