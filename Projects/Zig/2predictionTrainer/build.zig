const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
        .platform = rlz.PlatformBackend.glfw,
    });

    const app_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    app_module.addImport("raylib", raylib_dep.module("raylib"));
    addMediaFoundation(app_module, b);

    const exe = b.addExecutable(.{
        .name = "badminton-stroke-test",
        .root_module = app_module,
    });
    b.installArtifact(exe);

    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    if (b.args) |args| run.addArgs(args);
    const run_step = b.step("run", "Run the pre-stroke labeler");
    run_step.dependOn(&run.step);

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    test_module.addImport("raylib", raylib_dep.module("raylib"));
    addMediaFoundation(test_module, b);
    const tests = b.addTest(.{ .root_module = test_module });
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}

fn addMediaFoundation(module: *std.Build.Module, b: *std.Build) void {
    module.addCSourceFile(.{
        .file = b.path("src/mf_decoder.c"),
        .flags = &.{ "-std=c11", "-DUNICODE", "-D_UNICODE" },
    });
    module.addIncludePath(b.path("src"));
    module.linkSystemLibrary("mfplat", .{});
    module.linkSystemLibrary("mfreadwrite", .{});
    module.linkSystemLibrary("mfuuid", .{});
    module.linkSystemLibrary("ole32", .{});
    module.linkSystemLibrary("propsys", .{});
}
