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
    {
        const tests = b.addTest(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
        });
        tests.root_module.addImport("extensible-data-notation", mod);
        tests.filters = filters;
        const run_tests = b.addRunArtifact(tests);
        run_tests.has_side_effects = true;
        test_step.dependOn(&run_tests.step);
        b.installArtifact(tests);
    }
    {
        // TODO include these in src/tests.zig somehow.  these are included here instead
        // of in src/tests.zig to avoid a 'file exists in multiple modules' error.
        const tests = b.addTest(.{
            .root_source_file = b.path("src/ringbuffer.zig"),
            .target = target,
            .optimize = optimize,
        });
        tests.filters = filters;
        const run_tests = b.addRunArtifact(tests);
        test_step.dependOn(&run_tests.step);
        b.installArtifact(tests);
    }
    {
        const tests = b.addTest(.{
            .root_source_file = b.path("src/Tokenizer.zig"),
            .target = target,
            .optimize = optimize,
        });
        tests.filters = filters;
        const run_tests = b.addRunArtifact(tests);
        test_step.dependOn(&run_tests.step);
        b.installArtifact(tests);
    }
}
