const std = @import("std");

const file_version: u32 = 1;
const current_presentation_version: u32 = 2;

pub const Verdict = enum {
    wrong,
    correct,
};

pub const Key = struct {
    match_id: u8,
    stem: []const u8,
    contact_frame: u32,
};

pub const Entry = struct {
    presentation_version: u32 = 1,
    detector_version: u32,
    match_id: u8,
    stem: []const u8,
    contact_frame: u32,
    displayed_frame: u32,
    verdict: Verdict,
};

const File = struct {
    version: u32,
    labels: []const Entry,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayList(Entry) = .empty,

    pub fn load(allocator: std.mem.Allocator, io: std.Io, root: []const u8) !Store {
        var result = Store{ .allocator = allocator };
        errdefer result.deinit();
        const path = try filePath(allocator, root);
        defer allocator.free(path);
        const contents = std.Io.Dir.cwd().readFileAlloc(
            io,
            path,
            allocator,
            .limited(16 * 1024 * 1024),
        ) catch |err| switch (err) {
            error.FileNotFound => return result,
            else => return err,
        };
        defer allocator.free(contents);
        var parsed = try std.json.parseFromSlice(File, allocator, contents, .{
            .ignore_unknown_fields = true,
        });
        defer parsed.deinit();
        if (parsed.value.version != file_version) return error.UnsupportedStrokeLabelsVersion;
        for (parsed.value.labels) |label| {
            var owned = label;
            owned.stem = try allocator.dupe(u8, label.stem);
            try result.entries.append(allocator, owned);
        }
        return result;
    }

    pub fn deinit(self: *Store) void {
        for (self.entries.items) |entry| self.allocator.free(entry.stem);
        self.entries.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn contains(self: *const Store, key: Key) bool {
        const index = self.find(key) orelse return false;
        return self.entries.items[index].presentation_version == current_presentation_version;
    }

    pub fn put(
        self: *Store,
        key: Key,
        displayed_frame: u32,
        detector_version: u32,
        verdict: Verdict,
    ) !void {
        if (self.find(key)) |index| {
            const entry = &self.entries.items[index];
            entry.presentation_version = current_presentation_version;
            entry.displayed_frame = displayed_frame;
            entry.detector_version = detector_version;
            entry.verdict = verdict;
            return;
        }
        const entry = Entry{
            .presentation_version = current_presentation_version,
            .detector_version = detector_version,
            .match_id = key.match_id,
            .stem = try self.allocator.dupe(u8, key.stem),
            .contact_frame = key.contact_frame,
            .displayed_frame = displayed_frame,
            .verdict = verdict,
        };
        errdefer self.allocator.free(entry.stem);
        try self.entries.append(self.allocator, entry);
    }

    pub fn save(self: *const Store, allocator: std.mem.Allocator, io: std.Io, root: []const u8) !void {
        const work = try std.fs.path.join(allocator, &.{ root, "work" });
        defer allocator.free(work);
        try std.Io.Dir.cwd().createDirPath(io, work);
        const json = try std.json.Stringify.valueAlloc(
            allocator,
            File{ .version = file_version, .labels = self.entries.items },
            .{ .whitespace = .indent_2 },
        );
        defer allocator.free(json);
        const path = try filePath(allocator, root);
        defer allocator.free(path);
        const file = try std.Io.Dir.cwd().createFile(io, path, .{ .truncate = true });
        defer file.close(io);
        try file.writeStreamingAll(io, json);
    }

    fn find(self: *const Store, key: Key) ?usize {
        for (self.entries.items, 0..) |entry, index| {
            if (entry.match_id == key.match_id and
                entry.contact_frame == key.contact_frame and
                std.mem.eql(u8, entry.stem, key.stem)) return index;
        }
        return null;
    }
};

fn filePath(allocator: std.mem.Allocator, root: []const u8) ![]u8 {
    return std.fs.path.join(allocator, &.{ root, "work", "stroke_labels.json" });
}
