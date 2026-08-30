const std = @import("std");
const app = @import("app.zig");

pub fn main(init: std.process.Init) !void {
    var arguments = try std.process.Args.Iterator.initAllocator(
        init.minimal.args,
        init.gpa,
    );
    defer arguments.deinit();
    _ = arguments.next();
    const smoke_test = if (arguments.next()) |argument|
        std.mem.eql(u8, argument, "--smoke")
    else
        false;
    try app.run(init.io, .{ .smoke_test = smoke_test });
}
