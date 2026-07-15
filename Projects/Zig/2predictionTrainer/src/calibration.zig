const std = @import("std");
const geometry = @import("geometry.zig");
const defaults = @import("default_calibrations.zig");
const types = @import("types.zig");

const version: u32 = 1;

const CacheFile = struct {
    version: u32,
    entries: []const types.CourtCalibration,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayList(types.CourtCalibration) = .empty,

    pub fn load(allocator: std.mem.Allocator, io: std.Io, root: []const u8) !Store {
        var result = Store{ .allocator = allocator };
        errdefer result.deinit();
        for (defaults.corners, 0..) |corners, index| {
            try result.upsert(try fromCorners(@intCast(index + 1), null, corners));
        }
        const path = try filePath(allocator, root);
        defer allocator.free(path);
        const contents = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(4 * 1024 * 1024)) catch return result;
        defer allocator.free(contents);
        var parsed = std.json.parseFromSlice(CacheFile, allocator, contents, .{ .ignore_unknown_fields = true }) catch return result;
        defer parsed.deinit();
        if (parsed.value.version != version) return result;
        for (parsed.value.entries) |entry| try result.upsert(entry);
        return result;
    }

    pub fn deinit(self: *Store) void {
        for (self.entries.items) |entry| if (entry.rally_stem) |stem| self.allocator.free(stem);
        self.entries.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn find(self: *const Store, match_id: u8, rally_stem: []const u8) ?types.CourtCalibration {
        for (self.entries.items) |entry| {
            if (entry.match_id == match_id and entry.rally_stem != null and
                std.mem.eql(u8, entry.rally_stem.?, rally_stem)) return entry;
        }
        for (self.entries.items) |entry| {
            if (entry.match_id == match_id and entry.rally_stem == null) return entry;
        }
        return null;
    }

    /// Detector windows are defined in calibrated court space, so the event
    /// cache must be invalidated whenever any effective calibration changes.
    pub fn hash(self: *const Store) u64 {
        var value: u64 = 1469598103934665603;
        hashU64(&value, version);
        hashU64(&value, self.entries.items.len);
        for (self.entries.items) |entry| {
            hashU64(&value, entry.match_id);
            if (entry.rally_stem) |stem| {
                hashByte(&value, 1);
                hashU64(&value, stem.len);
                for (stem) |byte| hashByte(&value, byte);
            } else hashByte(&value, 0);
            hashU64(&value, entry.image_width);
            hashU64(&value, entry.image_height);
            for (entry.homography.m) |coefficient| hashU64(&value, @as(u64, @bitCast(coefficient)));
            hashByte(&value, @intFromBool(entry.broadcast_near_is_bottom));
        }
        return value;
    }

    pub fn upsert(self: *Store, entry: types.CourtCalibration) !void {
        for (self.entries.items) |*existing| {
            const same_scope = existing.match_id == entry.match_id and
                ((existing.rally_stem == null and entry.rally_stem == null) or
                    (existing.rally_stem != null and entry.rally_stem != null and
                        std.mem.eql(u8, existing.rally_stem.?, entry.rally_stem.?)));
            if (!same_scope) continue;
            if (existing.rally_stem) |stem| self.allocator.free(stem);
            existing.* = entry;
            if (entry.rally_stem) |stem| existing.rally_stem = try self.allocator.dupe(u8, stem);
            return;
        }
        var owned = entry;
        if (entry.rally_stem) |stem| owned.rally_stem = try self.allocator.dupe(u8, stem);
        try self.entries.append(self.allocator, owned);
    }

    pub fn save(self: *const Store, allocator: std.mem.Allocator, io: std.Io, root: []const u8) !void {
        const work_path = try std.fs.path.join(allocator, &.{ root, "work" });
        defer allocator.free(work_path);
        try std.Io.Dir.cwd().createDirPath(io, work_path);
        const json = try std.json.Stringify.valueAlloc(
            allocator,
            CacheFile{ .version = version, .entries = self.entries.items },
            .{ .whitespace = .indent_2 },
        );
        defer allocator.free(json);
        const path = try filePath(allocator, root);
        defer allocator.free(path);
        const file = try std.Io.Dir.cwd().createFile(io, path, .{ .truncate = true });
        defer file.close(io);
        try file.writeStreamingAll(io, json);
    }
};

pub fn fromCorners(match_id: u8, rally_stem: ?[]const u8, image: [4]types.Vec2) !types.CourtCalibration {
    const court = [4]types.Vec2{
        .{ .x = 0, .y = 0 },
        .{ .x = types.Court.doubles_width_m, .y = 0 },
        .{ .x = types.Court.doubles_width_m, .y = types.Court.length_m },
        .{ .x = 0, .y = types.Court.length_m },
    };
    const homography = try geometry.solveHomography(image, court);
    var max_error: f64 = 0;
    for (image, court) |source, expected| {
        const actual = homography.project(source) orelse return error.DegenerateCalibration;
        const dx = actual.x - expected.x;
        const dy = actual.y - expected.y;
        max_error = @max(max_error, @sqrt(dx * dx + dy * dy));
    }
    return .{
        .match_id = match_id,
        .rally_stem = rally_stem,
        .image_width = 1280,
        .image_height = 720,
        .homography = homography,
        .broadcast_near_is_bottom = true,
        .reprojection_error_px = max_error,
    };
}

fn filePath(allocator: std.mem.Allocator, root: []const u8) ![]u8 {
    return std.fs.path.join(allocator, &.{ root, "work", "court_calibrations.json" });
}

fn hashByte(hash: *u64, byte: u8) void {
    hash.* = (hash.* ^ byte) *% 1099511628211;
}

fn hashU64(hash: *u64, input: anytype) void {
    var value: u64 = @intCast(input);
    var index: u4 = 0;
    while (index < 8) : (index += 1) {
        hashByte(hash, @truncate(value));
        value >>= 8;
    }
}
