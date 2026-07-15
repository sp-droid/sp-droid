const std = @import("std");
const types = @import("types.zig");

pub const Catalog = struct {
    allocator: std.mem.Allocator,
    assets: std.ArrayList(types.RallyAsset) = .empty,

    pub fn scan(allocator: std.mem.Allocator, io: std.Io, root: []const u8) !Catalog {
        var result = Catalog{ .allocator = allocator };
        errdefer result.deinit();

        var match_id: u8 = 1;
        while (match_id <= 23) : (match_id += 1) {
            const match_name = try std.fmt.allocPrint(allocator, "match{d}", .{match_id});
            defer allocator.free(match_name);
            const csv_dir_path = try std.fs.path.join(allocator, &.{ root, "Professional", match_name, "csv" });
            defer allocator.free(csv_dir_path);
            const video_dir_path = try std.fs.path.join(allocator, &.{ root, "Professional", match_name, "video" });
            defer allocator.free(video_dir_path);

            var csv_dir = std.Io.Dir.cwd().openDir(io, csv_dir_path, .{ .iterate = true }) catch |err| {
                std.log.err("missing dataset directory: {s}", .{csv_dir_path});
                return err;
            };
            defer csv_dir.close(io);
            var video_dir = try std.Io.Dir.cwd().openDir(io, video_dir_path, .{});
            defer video_dir.close(io);

            var iterator = csv_dir.iterate();
            while (try iterator.next(io)) |entry| {
                if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, "_ball.csv")) continue;
                const stem = entry.name[0 .. entry.name.len - "_ball.csv".len];
                const video_name = try std.fmt.allocPrint(allocator, "{s}.mp4", .{stem});
                defer allocator.free(video_name);
                _ = video_dir.statFile(io, video_name, .{}) catch |err| {
                    std.log.err("CSV has no same-stem MP4: {s}\\{s}", .{ csv_dir_path, entry.name });
                    return err;
                };

                const csv_path = try std.fs.path.join(allocator, &.{ csv_dir_path, entry.name });
                errdefer allocator.free(csv_path);
                const video_path = try std.fs.path.join(allocator, &.{ video_dir_path, video_name });
                errdefer allocator.free(video_path);
                try result.assets.append(allocator, .{
                    .match_id = match_id,
                    .stem = try allocator.dupe(u8, stem),
                    .csv_path = csv_path,
                    .video_path = video_path,
                });
            }
        }

        std.mem.sort(types.RallyAsset, result.assets.items, {}, lessThanAsset);
        if (result.assets.items.len == 0) return error.EmptyDataset;
        return result;
    }

    pub fn deinit(self: *Catalog) void {
        for (self.assets.items) |asset| {
            self.allocator.free(asset.stem);
            self.allocator.free(asset.csv_path);
            self.allocator.free(asset.video_path);
        }
        self.assets.deinit(self.allocator);
        self.* = undefined;
    }
};

fn lessThanAsset(_: void, a: types.RallyAsset, b: types.RallyAsset) bool {
    if (a.match_id != b.match_id) return a.match_id < b.match_id;
    return std.mem.lessThan(u8, a.stem, b.stem);
}

pub const Trajectory = struct {
    allocator: std.mem.Allocator,
    points: []types.TrajectoryPoint,

    pub fn deinit(self: *Trajectory) void {
        self.allocator.free(self.points);
        self.* = undefined;
    }
};

pub fn loadTrajectory(
    allocator: std.mem.Allocator,
    io: std.Io,
    asset: *types.RallyAsset,
) !Trajectory {
    const contents = try std.Io.Dir.cwd().readFileAlloc(io, asset.csv_path, allocator, .limited(32 * 1024 * 1024));
    defer allocator.free(contents);
    const trajectory = try parseTrajectory(allocator, contents, asset.frame_rate_num, asset.frame_rate_den);
    asset.annotated_frame_count = @intCast(trajectory.points.len);
    return trajectory;
}

pub fn parseTrajectory(
    allocator: std.mem.Allocator,
    contents: []const u8,
    frame_rate_num: u32,
    frame_rate_den: u32,
) !Trajectory {
    if (frame_rate_num == 0 or frame_rate_den == 0) return error.InvalidFrameRate;
    var points: std.ArrayList(types.TrajectoryPoint) = .empty;
    errdefer points.deinit(allocator);

    var lines = std.mem.splitScalar(u8, contents, '\n');
    const raw_header = lines.next() orelse return error.MissingCsvHeader;
    const header = std.mem.trimEnd(u8, raw_header, "\r");
    if (!std.mem.eql(u8, header, "Frame,Visibility,X,Y")) return error.InvalidCsvHeader;

    var expected_frame: u32 = 0;
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0) continue;
        var fields = std.mem.splitScalar(u8, line, ',');
        const frame = try std.fmt.parseInt(u32, fields.next() orelse return error.InvalidCsvRow, 10);
        const visibility_raw = try std.fmt.parseInt(u8, fields.next() orelse return error.InvalidCsvRow, 10);
        const x = try std.fmt.parseFloat(f64, fields.next() orelse return error.InvalidCsvRow);
        const y = try std.fmt.parseFloat(f64, fields.next() orelse return error.InvalidCsvRow);
        if (fields.next() != null) return error.InvalidCsvRow;
        if (frame != expected_frame) return error.NonContiguousFrames;
        if (visibility_raw > 1) return error.InvalidVisibility;
        if (visibility_raw == 0 and (x != 0 or y != 0)) return error.InvisibleCoordinatesNotZero;

        const timestamp_numerator = @as(u128, frame) * 10_000_000 * frame_rate_den;
        const timestamp_hns: i64 = @intCast((timestamp_numerator + frame_rate_num / 2) / frame_rate_num);
        try points.append(allocator, .{
            .frame = frame,
            .timestamp_hns = timestamp_hns,
            .visibility = visibility_raw == 1,
            .image = .{ .x = x, .y = y },
        });
        expected_frame += 1;
    }
    if (points.items.len == 0) return error.EmptyTrajectory;
    interpolateShortGaps(points.items, 3);
    return .{ .allocator = allocator, .points = try points.toOwnedSlice(allocator) };
}

pub fn interpolateShortGaps(points: []types.TrajectoryPoint, max_gap: usize) void {
    if (points.len < 3) return;
    var i: usize = 1;
    while (i + 1 < points.len) {
        if (points[i].visibility) {
            i += 1;
            continue;
        }
        const gap_start = i;
        while (i < points.len and !points[i].visibility) : (i += 1) {}
        if (i >= points.len) break;
        const gap_len = i - gap_start;
        if (gap_len > max_gap or !points[gap_start - 1].visibility or !points[i].visibility) continue;
        const before = points[gap_start - 1].image;
        const after = points[i].image;
        for (0..gap_len) |offset| {
            const t = @as(f64, @floatFromInt(offset + 1)) / @as(f64, @floatFromInt(gap_len + 1));
            points[gap_start + offset].visibility = true;
            points[gap_start + offset].interpolated = true;
            points[gap_start + offset].image = .{
                .x = before.x + (after.x - before.x) * t,
                .y = before.y + (after.y - before.y) * t,
            };
        }
    }
}
