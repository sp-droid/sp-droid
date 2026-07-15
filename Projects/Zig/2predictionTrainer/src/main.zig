const std = @import("std");

pub fn main(init: std.process.Init) !void {
    @import("game.zig").run(init) catch |err| {
        std.log.err("Pre-stroke labeler stopped: {s}", .{@errorName(err)});
        return err;
    };
}
