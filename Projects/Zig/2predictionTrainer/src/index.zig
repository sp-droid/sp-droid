const std = @import("std");
const calibration = @import("calibration.zig");
const dataset = @import("dataset.zig");
const detector = @import("detector.zig");
const mf = @import("media_foundation.zig");
const types = @import("types.zig");

pub const RallyEvents = struct {
    asset_index: u32,
    contacts: []types.ContactEvent,
};

pub const StrokeIndex = struct {
    allocator: std.mem.Allocator,
    rallies: std.ArrayList(RallyEvents) = .empty,

    pub fn deinit(self: *StrokeIndex) void {
        for (self.rallies.items) |rally| self.allocator.free(rally.contacts);
        self.rallies.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn candidateCount(self: *const StrokeIndex) usize {
        var count: usize = 0;
        for (self.rallies.items) |rally| {
            for (rally.contacts) |contact| {
                if (contact.classification != .ground and contact.frame > 0) count += 1;
            }
        }
        return count;
    }
};

const CacheRally = struct {
    match_id: u8,
    stem: []const u8,
    codec: types.Codec,
    width: u32,
    height: u32,
    frame_rate_num: u32,
    frame_rate_den: u32,
    annotated_frame_count: u32,
    decoded_frame_count: u32,
    contacts: []const types.ContactEvent,
};

const CacheFile = struct {
    detector_version: u32,
    calibration_hash: u64,
    asset_count: u32,
    rallies: []const CacheRally,
};

pub fn loadOrBuild(
    allocator: std.mem.Allocator,
    io: std.Io,
    root: []const u8,
    catalog: *dataset.Catalog,
    calibrations: *const calibration.Store,
    force_reindex: bool,
) !StrokeIndex {
    if (!force_reindex) {
        if (try loadCache(allocator, io, root, catalog, calibrations.hash())) |loaded| {
            std.log.info("loaded {d} pre-stroke candidates from work/stroke_index.json", .{loaded.candidateCount()});
            return loaded;
        }
    }

    var built = try buildIndex(allocator, io, catalog, calibrations);
    errdefer built.deinit();
    try saveCache(allocator, io, root, catalog, calibrations.hash(), &built);
    return built;
}

fn buildIndex(
    allocator: std.mem.Allocator,
    io: std.Io,
    catalog: *dataset.Catalog,
    calibrations: *const calibration.Store,
) !StrokeIndex {
    var result = StrokeIndex{ .allocator = allocator };
    errdefer result.deinit();
    var last_match: u8 = 0;

    for (catalog.assets.items, 0..) |*asset, asset_index| {
        if (asset.match_id != last_match) {
            last_match = asset.match_id;
            std.log.info("indexing match {d}/23", .{last_match});
        }
        try probeAsset(allocator, asset);
        var trajectory = try dataset.loadTrajectory(allocator, io, asset);
        defer trajectory.deinit();
        const court_calibration = calibrations.find(asset.match_id, asset.stem) orelse
            return error.MissingCourtCalibration;
        var detected = try detector.detect(
            allocator,
            trajectory.points,
            asset.frame_rate_num,
            asset.frame_rate_den,
            court_calibration.homography,
        );
        defer detected.deinit();
        try result.rallies.append(allocator, .{
            .asset_index = @intCast(asset_index),
            .contacts = try allocator.dupe(types.ContactEvent, detected.contacts),
        });
    }
    std.log.info("generated {d} pre-stroke candidates from {d} rallies", .{
        result.candidateCount(), catalog.assets.items.len,
    });
    if (result.candidateCount() == 0) return error.NoStrokeCandidatesDetected;
    return result;
}

fn probeAsset(allocator: std.mem.Allocator, asset: *types.RallyAsset) !void {
    const path_z = try allocator.dupeZ(u8, asset.video_path);
    defer allocator.free(path_z);
    var decoder = try mf.Decoder.open(path_z);
    defer decoder.close();
    asset.width = decoder.info.width;
    asset.height = decoder.info.height;
    asset.frame_rate_num = decoder.info.frame_rate_num;
    asset.frame_rate_den = decoder.info.frame_rate_den;
    asset.codec = codecFromFourcc(decoder.info.native_subtype_fourcc);
    if (asset.width != 1280 or asset.height != 720) return error.UnsupportedDatasetResolution;
}

fn codecFromFourcc(value: u32) types.Codec {
    // Media Foundation subtype GUID Data1 contains the little-endian FourCC.
    const lower = value | 0x20202020;
    const mp4v = fourcc('m', 'p', '4', 'v');
    const avc1 = fourcc('a', 'v', 'c', '1');
    const h264 = fourcc('h', '2', '6', '4');
    if (lower == mp4v) return .mp4v;
    if (lower == avc1 or lower == h264) return .h264;
    return .unknown;
}

fn fourcc(a: u8, b: u8, c: u8, d: u8) u32 {
    return @as(u32, a) | (@as(u32, b) << 8) | (@as(u32, c) << 16) | (@as(u32, d) << 24);
}

fn cachePath(allocator: std.mem.Allocator, root: []const u8) ![]u8 {
    return std.fs.path.join(allocator, &.{ root, "work", "stroke_index.json" });
}

fn loadCache(
    allocator: std.mem.Allocator,
    io: std.Io,
    root: []const u8,
    catalog: *dataset.Catalog,
    calibration_hash: u64,
) !?StrokeIndex {
    const path = try cachePath(allocator, root);
    defer allocator.free(path);
    const contents = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(128 * 1024 * 1024)) catch return null;
    defer allocator.free(contents);
    var parsed = std.json.parseFromSlice(CacheFile, allocator, contents, .{ .ignore_unknown_fields = true }) catch return null;
    defer parsed.deinit();
    if (parsed.value.detector_version != detector.detector_version or
        parsed.value.calibration_hash != calibration_hash or
        parsed.value.asset_count != catalog.assets.items.len or
        parsed.value.rallies.len != catalog.assets.items.len) return null;

    var result = StrokeIndex{ .allocator = allocator };
    errdefer result.deinit();
    for (parsed.value.rallies) |cached| {
        const asset_index = findAsset(catalog, cached.match_id, cached.stem) orelse return null;
        const asset = &catalog.assets.items[asset_index];
        asset.codec = cached.codec;
        asset.width = cached.width;
        asset.height = cached.height;
        asset.frame_rate_num = cached.frame_rate_num;
        asset.frame_rate_den = cached.frame_rate_den;
        asset.annotated_frame_count = cached.annotated_frame_count;
        asset.decoded_frame_count = cached.decoded_frame_count;
        try result.rallies.append(allocator, .{
            .asset_index = @intCast(asset_index),
            .contacts = try allocator.dupe(types.ContactEvent, cached.contacts),
        });
    }
    if (result.candidateCount() == 0) return null;
    return result;
}

fn findAsset(catalog: *const dataset.Catalog, match_id: u8, stem: []const u8) ?usize {
    for (catalog.assets.items, 0..) |asset, i| {
        if (asset.match_id == match_id and std.mem.eql(u8, asset.stem, stem)) return i;
    }
    return null;
}

fn saveCache(
    allocator: std.mem.Allocator,
    io: std.Io,
    root: []const u8,
    catalog: *const dataset.Catalog,
    calibration_hash: u64,
    index: *const StrokeIndex,
) !void {
    const work_path = try std.fs.path.join(allocator, &.{ root, "work" });
    defer allocator.free(work_path);
    try std.Io.Dir.cwd().createDirPath(io, work_path);

    var cache_rallies = try allocator.alloc(CacheRally, index.rallies.items.len);
    defer allocator.free(cache_rallies);
    for (index.rallies.items, 0..) |rally, i| {
        const asset = catalog.assets.items[rally.asset_index];
        cache_rallies[i] = .{
            .match_id = asset.match_id,
            .stem = asset.stem,
            .codec = asset.codec,
            .width = asset.width,
            .height = asset.height,
            .frame_rate_num = asset.frame_rate_num,
            .frame_rate_den = asset.frame_rate_den,
            .annotated_frame_count = asset.annotated_frame_count,
            .decoded_frame_count = asset.decoded_frame_count,
            .contacts = rally.contacts,
        };
    }
    const value = CacheFile{
        .detector_version = detector.detector_version,
        .calibration_hash = calibration_hash,
        .asset_count = @intCast(catalog.assets.items.len),
        .rallies = cache_rallies,
    };
    const json = try std.json.Stringify.valueAlloc(allocator, value, .{ .whitespace = .indent_2 });
    defer allocator.free(json);
    const path = try cachePath(allocator, root);
    defer allocator.free(path);
    const file = try std.Io.Dir.cwd().createFile(io, path, .{ .truncate = true });
    defer file.close(io);
    try file.writeStreamingAll(io, json);
    std.log.info("saved detector v{d} index to {s}", .{ detector.detector_version, path });
}
