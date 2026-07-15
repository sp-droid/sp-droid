const std = @import("std");

pub const Options = struct {
    allocator: std.mem.Allocator,
    dataset_root: []const u8,
    reindex: bool = false,
    index_only: bool = false,
    qa_screenshot: bool = false,

    pub fn parse(init: std.process.Init) !Options {
        var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
        defer args.deinit();
        const executable_argument = args.next() orelse ".";
        var result = Options{
            .allocator = init.gpa,
            .dataset_root = try init.gpa.dupe(u8, "."),
        };
        errdefer result.deinit();
        var dataset_was_explicit = false;
        while (args.next()) |argument| {
            if (std.mem.eql(u8, argument, "--dataset")) {
                const value = args.next() orelse return error.MissingDatasetPath;
                init.gpa.free(result.dataset_root);
                result.dataset_root = try init.gpa.dupe(u8, value);
                dataset_was_explicit = true;
            } else if (std.mem.eql(u8, argument, "--reindex")) {
                result.reindex = true;
            } else if (std.mem.eql(u8, argument, "--index-only")) {
                result.index_only = true;
            } else if (std.mem.eql(u8, argument, "--qa-screenshot")) {
                result.qa_screenshot = true;
            } else {
                std.log.err("unknown argument: {s}", .{argument});
                return error.UnknownArgument;
            }
        }
        if (!dataset_was_explicit) {
            const discovered = try discoverDatasetRoot(init.gpa, init.io, executable_argument);
            init.gpa.free(result.dataset_root);
            result.dataset_root = discovered;
        }
        return result;
    }

    pub fn deinit(self: *Options) void {
        self.allocator.free(self.dataset_root);
        self.* = undefined;
    }
};

fn discoverDatasetRoot(
    allocator: std.mem.Allocator,
    io: std.Io,
    executable_argument: []const u8,
) ![]u8 {
    const cwd = try std.process.currentPathAlloc(io, allocator);
    defer allocator.free(cwd);
    if (try containsDataset(allocator, io, cwd)) return allocator.dupe(u8, cwd);

    const executable = try std.fs.path.resolve(allocator, &.{ cwd, executable_argument });
    defer allocator.free(executable);
    var candidate = std.fs.path.dirname(executable) orelse cwd;
    var level: u4 = 0;
    while (level < 8) : (level += 1) {
        if (!std.mem.eql(u8, candidate, cwd) and
            try containsDataset(allocator, io, candidate))
        {
            return allocator.dupe(u8, candidate);
        }
        const parent = std.fs.path.dirname(candidate) orelse break;
        if (std.mem.eql(u8, parent, candidate)) break;
        candidate = parent;
    }
    return allocator.dupe(u8, cwd);
}

fn containsDataset(allocator: std.mem.Allocator, io: std.Io, root: []const u8) !bool {
    const marker = try std.fs.path.join(allocator, &.{ root, "Professional", "match1", "csv" });
    defer allocator.free(marker);
    var directory = std.Io.Dir.openDirAbsolute(io, marker, .{}) catch return false;
    directory.close(io);
    return true;
}
