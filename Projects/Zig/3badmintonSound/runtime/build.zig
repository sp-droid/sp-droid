const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const module = b.createModule(.{
        .root_source_file = b.path("src/c_api.zig"),
        .target = target,
        .optimize = optimize,
        .pic = true,
    });
    const library = b.addLibrary(.{
        .name = "badminton_audio",
        .linkage = .static,
        .root_module = module,
    });
    library.installHeader(b.path("include/badminton_audio.h"), "badminton_audio.h");
    b.installArtifact(library);
}
