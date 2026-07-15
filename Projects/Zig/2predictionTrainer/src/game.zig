const std = @import("std");
const rl = @import("raylib");
const calibration = @import("calibration.zig");
const dataset = @import("dataset.zig");
const detector = @import("detector.zig");
const stroke_index = @import("index.zig");
const options_module = @import("options.zig");
const stroke_labels = @import("stroke_labels.zig");
const types = @import("types.zig");
const video = @import("video.zig");

const window_width: f32 = 1600;
const window_height: f32 = 920;
const video_rect = rl.Rectangle{ .x = 0, .y = 10, .width = 1600, .height = 900 };
const wrong_rect = rl.Rectangle{ .x = 350, .y = 830, .width = 420, .height = 70 };
const correct_rect = rl.Rectangle{ .x = 830, .y = 830, .width = 420, .height = 70 };
const minimum_before_seconds = 1.0;
const before_seconds = 2.0;
const after_seconds = 1.0;
const pause_seconds = 1.0;

const State = enum {
    loading,
    before,
    paused,
    after,
    ready,
    complete,
    failed,
};

const Scenario = struct {
    asset_index: u32,
    contact: types.ContactEvent,
};

const Clip = struct {
    start_frame: u32,
    pause_frame: u32,
    end_frame: u32,
};

pub fn run(init: std.process.Init) !void {
    var options = try options_module.Options.parse(init);
    defer options.deinit();
    var catalog = try dataset.Catalog.scan(init.gpa, init.io, options.dataset_root);
    defer catalog.deinit();
    var calibrations = try calibration.Store.load(init.gpa, init.io, options.dataset_root);
    defer calibrations.deinit();
    var index = try stroke_index.loadOrBuild(
        init.gpa,
        init.io,
        options.dataset_root,
        &catalog,
        &calibrations,
        options.reindex,
    );
    defer index.deinit();

    const scenarios = try buildScenarios(init.gpa, &index, &catalog);
    defer init.gpa.free(scenarios);
    if (options.index_only) {
        std.log.info("{d} ordered pre-stroke candidates", .{scenarios.len});
        return;
    }
    if (scenarios.len == 0) return error.NoStrokeCandidates;

    var labels = try stroke_labels.Store.load(init.gpa, init.io, options.dataset_root);
    defer labels.deinit();

    rl.setTraceLogLevel(.warning);
    rl.setConfigFlags(.{
        .window_highdpi = true,
        .msaa_4x_hint = true,
        .window_resizable = false,
        .fullscreen_mode = true,
    });
    rl.initWindow(@intFromFloat(window_width), @intFromFloat(window_height), "Pre-stroke Labeler");
    defer rl.closeWindow();
    if (!rl.isWindowFullscreen()) rl.toggleFullscreen();
    if (!rl.isWindowFullscreen()) return error.FullscreenUnavailable;
    rl.setTargetFPS(120);

    const decoder = try video.DecoderThread.create(init.gpa);
    defer decoder.destroy();
    var surface = try video.Surface.init(init.gpa);
    defer surface.deinit();

    var session = Session{
        .allocator = init.gpa,
        .io = init.io,
        .dataset_root = options.dataset_root,
        .catalog = &catalog,
        .scenarios = scenarios,
        .labels = &labels,
        .decoder = decoder,
        .surface = &surface,
    };
    session.labelled = session.countAlreadyLabelled();
    try session.startNext();
    const qa_started = rl.getTime();
    var qa_before = false;
    var qa_paused = false;
    var qa_after = false;

    while (!rl.windowShouldClose()) {
        try session.update(rl.getTime());
        session.draw();
        if (options.qa_screenshot) {
            switch (session.state) {
                .before => if (!qa_before) {
                    rl.takeScreenshot("work/labeler_qa_before.png");
                    qa_before = true;
                },
                .paused => if (!qa_paused) {
                    rl.takeScreenshot("work/labeler_qa_paused.png");
                    qa_paused = true;
                },
                .after => if (!qa_after) {
                    rl.takeScreenshot("work/labeler_qa_after.png");
                    qa_after = true;
                },
                .ready => {
                    rl.takeScreenshot("work/labeler_qa.png");
                    break;
                },
                else => {},
            }
        }
        if (options.qa_screenshot and rl.getTime() - qa_started > 20) break;
    }
}

fn buildScenarios(
    allocator: std.mem.Allocator,
    index: *const stroke_index.StrokeIndex,
    catalog: *const dataset.Catalog,
) ![]Scenario {
    var result: std.ArrayList(Scenario) = .empty;
    errdefer result.deinit(allocator);
    for (index.rallies.items) |rally| {
        const asset = catalog.assets.items[rally.asset_index];
        const minimum_before_frames: u32 = @intFromFloat(@ceil(
            asset.fps() * minimum_before_seconds,
        ));
        for (rally.contacts) |contact| {
            // The terminal visit is a floor event. Every other visit is shown,
            // including low-confidence ones, so the human labels are unbiased.
            if (contact.classification == .ground or
                contact.frame < minimum_before_frames) continue;
            try result.append(allocator, .{
                .asset_index = rally.asset_index,
                .contact = contact,
            });
        }
    }
    return result.toOwnedSlice(allocator);
}

const Session = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    dataset_root: []const u8,
    catalog: *const dataset.Catalog,
    scenarios: []const Scenario,
    labels: *stroke_labels.Store,
    decoder: *video.DecoderThread,
    surface: *video.Surface,
    cursor: usize = 0,
    labelled: usize = 0,
    state: State = .loading,
    clip: Clip = undefined,
    playback_wall_start: f64 = 0,
    playback_media_start_hns: i64 = 0,
    pause_started: f64 = 0,
    after_wall_start: f64 = 0,
    after_media_start_hns: ?i64 = null,

    fn countAlreadyLabelled(self: *const Session) usize {
        var count: usize = 0;
        for (self.scenarios) |scenario| {
            if (self.labels.contains(self.keyFor(scenario))) count += 1;
        }
        return count;
    }

    fn keyFor(self: *const Session, scenario: Scenario) stroke_labels.Key {
        const asset = self.catalog.assets.items[scenario.asset_index];
        return .{
            .match_id = asset.match_id,
            .stem = asset.stem,
            .contact_frame = scenario.contact.frame,
        };
    }

    fn current(self: *const Session) Scenario {
        return self.scenarios[self.cursor];
    }

    fn clipFor(self: *const Session, scenario: Scenario) Clip {
        const asset = self.catalog.assets.items[scenario.asset_index];
        const before_frames: u32 = @max(1, @as(u32, @intFromFloat(@round(
            asset.fps() * before_seconds,
        ))));
        const after_frames: u32 = @max(1, @as(u32, @intFromFloat(@round(
            asset.fps() * after_seconds,
        ))));
        const pause_frame = scenario.contact.frame - 1;
        const start_frame = pause_frame -| before_frames;

        const last_annotated = asset.annotated_frame_count - 1;
        const end_frame = @max(
            @min(scenario.contact.frame + after_frames, last_annotated),
            scenario.contact.frame,
        );
        return .{
            .start_frame = @min(start_frame, pause_frame),
            .pause_frame = pause_frame,
            .end_frame = end_frame,
        };
    }

    fn startNext(self: *Session) !void {
        while (self.cursor < self.scenarios.len and
            self.labels.contains(self.keyFor(self.scenarios[self.cursor])))
        {
            self.cursor += 1;
        }
        if (self.cursor == self.scenarios.len) {
            self.surface.current_frame = null;
            self.state = .complete;
            return;
        }

        const scenario = self.current();
        const asset = self.catalog.assets.items[scenario.asset_index];
        self.clip = self.clipFor(scenario);
        _ = try self.decoder.request(
            asset.video_path,
            self.clip.start_frame,
            self.clip.end_frame,
            asset.annotated_frame_count,
        );
        self.surface.current_frame = null;
        self.after_media_start_hns = null;
        self.state = .loading;
    }

    fn update(self: *Session, now: f64) !void {
        switch (self.state) {
            .loading => self.updateLoading(now),
            .before => self.updateBefore(now),
            .paused => self.updatePaused(now),
            .after => self.updateAfter(now),
            .ready => try self.updateReady(),
            .complete, .failed => {},
        }
    }

    fn updateLoading(self: *Session, now: f64) void {
        if (self.decoder.peek()) |frame| {
            if (frame.frame != self.clip.start_frame) {
                self.decoder.release(frame);
                self.state = .failed;
                return;
            }
            self.surface.upload(frame);
            self.playback_wall_start = now;
            self.playback_media_start_hns = frame.timestamp_hns;
            self.decoder.release(frame);
            if (frame.frame == self.clip.pause_frame) {
                self.pause_started = now;
                self.state = .paused;
            } else {
                self.state = .before;
            }
            return;
        }
        if (self.decoder.status() == .failed or self.decoder.status() == .completed) {
            self.state = .failed;
        }
    }

    fn updateBefore(self: *Session, now: f64) void {
        while (self.decoder.peek()) |frame| {
            if (frame.frame > self.clip.pause_frame) {
                self.state = .failed;
                return;
            }
            const due = self.playback_wall_start +
                @as(f64, @floatFromInt(frame.timestamp_hns - self.playback_media_start_hns)) /
                    10_000_000.0;
            if (now + 0.0005 < due) return;
            self.surface.upload(frame);
            self.decoder.release(frame);
            if (frame.frame == self.clip.pause_frame) {
                self.pause_started = now;
                self.state = .paused;
                return;
            }
        }
        if (self.decoder.status() == .failed or self.decoder.status() == .completed) {
            self.state = .failed;
        }
    }

    fn updatePaused(self: *Session, now: f64) void {
        if (now - self.pause_started < pause_seconds) return;
        self.after_wall_start = now;
        self.after_media_start_hns = null;
        self.state = .after;
    }

    fn updateAfter(self: *Session, now: f64) void {
        while (self.decoder.peek()) |frame| {
            if (self.after_media_start_hns == null) self.after_media_start_hns = frame.timestamp_hns;
            const due = self.after_wall_start +
                @as(f64, @floatFromInt(frame.timestamp_hns - self.after_media_start_hns.?)) /
                    10_000_000.0;
            if (now + 0.0005 < due) return;
            self.surface.upload(frame);
            self.decoder.release(frame);
            if (frame.frame >= self.clip.end_frame) {
                self.state = .ready;
                return;
            }
        }
        if (self.decoder.status() == .failed) {
            self.state = .failed;
        } else if (self.decoder.status() == .completed) {
            self.state = if (self.surface.current_frame != null and
                self.surface.current_frame.? >= self.clip.end_frame)
                .ready
            else
                .failed;
        }
    }

    fn updateReady(self: *Session) !void {
        if (!rl.isMouseButtonPressed(.left)) return;
        const mouse = logicalMousePosition();
        if (rl.checkCollisionPointRec(mouse, wrong_rect)) {
            try self.record(.wrong);
        } else if (rl.checkCollisionPointRec(mouse, correct_rect)) {
            try self.record(.correct);
        }
    }

    fn record(self: *Session, verdict: stroke_labels.Verdict) !void {
        const scenario = self.current();
        try self.labels.put(
            self.keyFor(scenario),
            scenario.contact.frame - 1,
            detector.detector_version,
            verdict,
        );
        try self.labels.save(self.allocator, self.io, self.dataset_root);
        self.labelled += 1;
        self.cursor += 1;
        try self.startNext();
    }

    fn draw(self: *const Session) void {
        rl.beginDrawing();
        defer rl.endDrawing();
        rl.clearBackground(.black);
        rl.beginMode2D(canvasCamera());
        defer rl.endMode2D();

        if (self.surface.current_frame != null) self.surface.draw(video_rect);
        switch (self.state) {
            .loading => drawCentered("LOADING CLIP", 430, 28, .light_gray),
            .before => {
                self.drawHeader();
                drawPhase("BEFORE", .white);
            },
            .paused => {
                self.drawHeader();
                drawPhase("PAUSE", .{ .r = 255, .g = 205, .b = 84, .a = 255 });
            },
            .after => {
                self.drawHeader();
                drawPhase("AFTER", .white);
            },
            .ready => {
                self.drawHeader();
                drawChoice(wrong_rect, "WRONG", .{ .r = 185, .g = 42, .b = 52, .a = 245 });
                drawChoice(correct_rect, "CORRECT", .{ .r = 26, .g = 145, .b = 82, .a = 245 });
            },
            .complete => {
                drawCentered("DATASET LABELLED", 390, 42, .white);
                drawCentered("100%", 455, 64, .{ .r = 69, .g = 221, .b = 139, .a = 255 });
            },
            .failed => drawCentered("COULD NOT DECODE THIS FRAME", 430, 30, .red),
        }
    }

    fn drawHeader(self: *const Session) void {
        rl.drawRectangle(0, 10, 1600, 75, .{ .r = 0, .g = 0, .b = 0, .a = 205 });
        const scenario = self.current();
        const asset = self.catalog.assets.items[scenario.asset_index];
        var item_buffer: [160]u8 = undefined;
        const item = std.fmt.bufPrintZ(
            &item_buffer,
            "MATCH {d}  /  {s}  /  FRAME {d}",
            .{ asset.match_id, asset.stem, scenario.contact.frame - 1 },
        ) catch "";
        rl.drawText(item, 35, 35, 23, .white);

        const percent = 100.0 * @as(f64, @floatFromInt(self.labelled)) /
            @as(f64, @floatFromInt(self.scenarios.len));
        var progress_buffer: [128]u8 = undefined;
        const progress = std.fmt.bufPrintZ(
            &progress_buffer,
            "{d}/{d}  {d:.1}% LABELLED",
            .{ self.labelled, self.scenarios.len, percent },
        ) catch "";
        rl.drawText(progress, 1190, 35, 23, .white);
    }
};

fn drawChoice(rect: rl.Rectangle, label: [:0]const u8, base: rl.Color) void {
    var color = base;
    if (rl.checkCollisionPointRec(logicalMousePosition(), rect)) {
        color.r +|= 25;
        color.g +|= 25;
        color.b +|= 25;
    }
    rl.drawRectangleRec(rect, color);
    rl.drawRectangleLinesEx(rect, 2, .white);
    const text_width: f32 = @floatFromInt(rl.measureText(label, 30));
    rl.drawText(
        label,
        @intFromFloat(rect.x + (rect.width - text_width) / 2),
        @intFromFloat(rect.y + 20),
        30,
        .white,
    );
}

fn drawPhase(label: [:0]const u8, color: rl.Color) void {
    rl.drawRectangle(650, 840, 300, 50, .{ .r = 0, .g = 0, .b = 0, .a = 205 });
    const width = rl.measureText(label, 24);
    rl.drawText(label, 800 - @divTrunc(width, 2), 853, 24, color);
}

fn drawCentered(text: [:0]const u8, y: i32, size: i32, color: rl.Color) void {
    const width = rl.measureText(text, size);
    rl.drawText(text, @divTrunc(@as(i32, @intFromFloat(window_width)) - width, 2), y, size, color);
}

fn canvasCamera() rl.Camera2D {
    const screen_width: f32 = @floatFromInt(rl.getScreenWidth());
    const screen_height: f32 = @floatFromInt(rl.getScreenHeight());
    const scale = @min(screen_width / window_width, screen_height / window_height);
    return .{
        .offset = .{
            .x = (screen_width - window_width * scale) / 2,
            .y = (screen_height - window_height * scale) / 2,
        },
        .target = .{ .x = 0, .y = 0 },
        .rotation = 0,
        .zoom = scale,
    };
}

fn logicalMousePosition() rl.Vector2 {
    return rl.getScreenToWorld2D(rl.getMousePosition(), canvasCamera());
}
