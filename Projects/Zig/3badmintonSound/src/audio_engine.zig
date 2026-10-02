const std = @import("std");
const rl = @import("raylib");
const stream_audio = @import("stream_audio.zig");
const model = @import("model.zig");
const live_swoosh = @import("live_swoosh.zig");

pub const output_sample_rate_hz: u32 = 48_000;
const stream_block_frames: usize = 512;

pub const Status = struct {
    ready: bool,
    failed: bool,
    playing_clip: bool,
    clip_queued: bool,
    submitted_clips: u64,
    completed_clips: u64,
    replaced_clips: u64,
    streamed_frames: u64,
};

const QueuedClip = struct {
    samples: []f32,
    sample_rate_hz: u32,
    impact_frame: usize,
};

/// Owns every raylib audio call on one dedicated thread. The raylib/miniaudio
/// backend consumes the persistent stream on its device callback thread while
/// this owner thread keeps both halves filled with one-shot hit data plus the
/// independent continuous velocity-driven aerodynamic source.
pub const AudioEngine = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    mutex: std.Io.Mutex = .init,
    pending_clip: ?QueuedClip = null,
    swoosh_control: live_swoosh.Control = .{},
    stopping: bool = false,
    ready: bool = false,
    failed: bool = false,
    playing_clip: bool = false,
    last_error: ?[]const u8 = null,
    submitted_clips: u64 = 0,
    completed_clips: u64 = 0,
    replaced_clips: u64 = 0,
    streamed_frames: u64 = 0,
    thread: ?std.Thread = null,

    pub fn init(io: std.Io, allocator: std.mem.Allocator) AudioEngine {
        return .{ .io = io, .allocator = allocator };
    }

    pub fn start(self: *AudioEngine) !void {
        self.thread = try std.Thread.spawn(.{}, threadMain, .{self});
    }

    pub fn deinit(self: *AudioEngine) void {
        if (self.thread) |thread| {
            self.lock();
            self.stopping = true;
            self.unlock();
            thread.join();
            self.thread = null;
        }
        if (self.pending_clip) |clip| {
            self.allocator.free(clip.samples);
            self.pending_clip = null;
        }
    }

    /// Hands one immutable strike to the audio thread. The queue has a
    /// capacity of one: an unplayed stale strike is replaced instead of
    /// creating latency. Rate conversion is deliberately deferred to the
    /// audio owner thread.
    pub fn submit(
        self: *AudioEngine,
        samples: []const f32,
        sample_rate_hz: u32,
        impact_frame: usize,
    ) !bool {
        const copied_samples = try self.allocator.dupe(f32, samples);
        self.lock();
        defer self.unlock();
        if (self.stopping or self.failed) {
            self.allocator.free(copied_samples);
            return false;
        }
        if (self.pending_clip) |stale| {
            self.allocator.free(stale.samples);
            self.replaced_clips += 1;
        }
        self.pending_clip = .{
            .samples = copied_samples,
            .sample_rate_hz = sample_rate_hz,
            .impact_frame = impact_frame,
        };
        self.submitted_clips += 1;
        return true;
    }

    /// Updates the persistent aerodynamic source. This is deliberately
    /// independent of submit(): changing velocity never launches a clip.
    pub fn setLiveControls(self: *AudioEngine, profile: model.Profile) void {
        const control = live_swoosh.Control.fromProfile(profile);
        self.lock();
        self.swoosh_control = control;
        self.unlock();
    }

    pub fn status(self: *AudioEngine) Status {
        self.lock();
        defer self.unlock();
        return .{
            .ready = self.ready,
            .failed = self.failed,
            .playing_clip = self.playing_clip,
            .clip_queued = self.pending_clip != null,
            .submitted_clips = self.submitted_clips,
            .completed_clips = self.completed_clips,
            .replaced_clips = self.replaced_clips,
            .streamed_frames = self.streamed_frames,
        };
    }

    pub fn takeError(self: *AudioEngine) ?[]const u8 {
        self.lock();
        defer self.unlock();
        const message = self.last_error;
        self.last_error = null;
        return message;
    }

    fn threadMain(self: *AudioEngine) void {
        rl.initAudioDevice();
        if (!rl.isAudioDeviceReady()) {
            self.publishError("AudioDeviceUnavailable");
            return;
        }
        defer rl.closeAudioDevice();

        rl.setAudioStreamBufferSizeDefault(stream_block_frames);
        const stream = rl.loadAudioStream(
            output_sample_rate_hz,
            32,
            1,
        ) catch {
            self.publishError("LoadAudioStream");
            return;
        };
        defer rl.unloadAudioStream(stream);

        var active_clip: ?[]f32 = null;
        defer if (active_clip) |clip| self.allocator.free(clip);
        var cursor = stream_audio.ClipCursor{ .samples = &.{} };
        // Stream frames relative to the latest contact; null when no swing
        // is in progress.
        var swing_frame: ?i64 = null;
        var block: [stream_block_frames]f32 = @splat(0.0);
        var swoosh = live_swoosh.Generator.init();

        // Both stream halves begin processed. Filling both before Play avoids
        // an initialization underrun and establishes a continuous timeline.
        for (0..2) |_| {
            self.fillBlock(&block, &active_clip, &cursor, &swing_frame, &swoosh);
            rl.updateAudioStream(stream, @ptrCast(block[0..].ptr), block.len);
            self.noteStreamedBlock();
        }
        rl.playAudioStream(stream);
        self.setReady();

        while (!self.shouldStop()) {
            var wrote_block = false;
            while (rl.isAudioStreamProcessed(stream)) {
                self.fillBlock(&block, &active_clip, &cursor, &swing_frame, &swoosh);
                rl.updateAudioStream(
                    stream,
                    @ptrCast(block[0..].ptr),
                    block.len,
                );
                self.noteStreamedBlock();
                wrote_block = true;
            }
            if (!wrote_block) {
                std.Io.sleep(self.io, .fromMilliseconds(2), .awake) catch {};
            }
        }
        rl.stopAudioStream(stream);
    }

    fn fillBlock(
        self: *AudioEngine,
        block: []f32,
        active_clip: *?[]f32,
        cursor: *stream_audio.ClipCursor,
        swing_frame: *?i64,
        swoosh: *live_swoosh.Generator,
    ) void {
        std.debug.assert(block.len == stream_block_frames);
        var swoosh_block: [stream_block_frames]f32 = @splat(0.0);
        @memset(block, 0.0);
        const swoosh_control = self.currentSwooshControl();
        const swing_params = swingParams(swoosh_control);
        var written: usize = 0;
        while (written < block.len) {
            if (active_clip.* == null) {
                const queued = self.takePendingClip() orelse break;
                const impact_frame = queued.impact_frame *
                    output_sample_rate_hz / @max(1, queued.sample_rate_hz);
                const lead_frames: usize = @intFromFloat(@round(
                    model.swingLeadSeconds(
                        swing_params,
                        @as(f64, @floatFromInt(impact_frame)) /
                            @as(f64, @floatFromInt(output_sample_rate_hz)),
                    ) * @as(f64, @floatFromInt(output_sample_rate_hz)),
                ));
                const next = self.prepareClip(queued, lead_frames) orelse continue;
                active_clip.* = next;
                // Block start relative to contact, in stream frames.
                swing_frame.* = -@as(i64, @intCast(written + lead_frames + impact_frame));
                cursor.* = .{ .samples = next };
                self.setPlaying(true);
            }
            written += cursor.copyInto(block[written..]);
            if (cursor.finished()) {
                self.allocator.free(active_clip.*.?);
                active_clip.* = null;
                cursor.* = .{ .samples = &.{} };
                self.finishClip();
            }
        }
        var live_control = swoosh_control;
        if (swoosh_control.swing_build_up_ms > 0.0) {
            // Evaluate the swing at the block centre; the generator's own
            // velocity response smooths the 10.7 ms steps.
            var envelope: f64 = 0.0;
            if (swing_frame.*) |frame| {
                const centre = frame + @as(i64, @intCast(block.len / 2));
                envelope = model.swingEnvelope(
                    swing_params,
                    @as(f64, @floatFromInt(centre)) /
                        @as(f64, @floatFromInt(output_sample_rate_hz)),
                );
                const next_frame = frame + @as(i64, @intCast(block.len));
                const follow_frames: i64 = @intFromFloat(
                    swoosh_control.swing_follow_through_ms * 0.001 *
                        @as(f64, @floatFromInt(output_sample_rate_hz)),
                );
                swing_frame.* = if (next_frame > follow_frames) null else next_frame;
            }
            live_control.racket_speed_mps *= envelope;
        }
        swoosh.setControl(live_control);
        swoosh.addTo(&swoosh_block);
        for (block, swoosh_block) |*sample, wind| sample.* += wind;
        const ceiling: f32 = @floatCast(std.math.pow(
            f64,
            10.0,
            std.math.clamp(swoosh_control.limiter_ceiling_dbfs, -24.0, 0.0) / 20.0,
        ));
        for (block) |*sample| sample.* = std.math.clamp(sample.*, -ceiling, ceiling);
    }

    /// Converts to the stream rate and prepends `lead_frames` of silence for
    /// the audible swing build-up.
    fn prepareClip(self: *AudioEngine, queued: QueuedClip, lead_frames: usize) ?[]f32 {
        const converted = if (queued.sample_rate_hz == output_sample_rate_hz)
            queued.samples
        else blk: {
            const resampled = stream_audio.resampleWindowedSinc(
                self.allocator,
                queued.samples,
                queued.sample_rate_hz,
                output_sample_rate_hz,
            ) catch |err| {
                self.allocator.free(queued.samples);
                self.publishWarning(@errorName(err));
                return null;
            };
            self.allocator.free(queued.samples);
            break :blk resampled;
        };
        if (lead_frames == 0) return converted;
        defer self.allocator.free(converted);
        const delayed = self.allocator.alloc(f32, lead_frames + converted.len) catch |err| {
            self.publishWarning(@errorName(err));
            return null;
        };
        @memset(delayed[0..lead_frames], 0.0);
        @memcpy(delayed[lead_frames..], converted);
        return delayed;
    }

    fn takePendingClip(self: *AudioEngine) ?QueuedClip {
        self.lock();
        defer self.unlock();
        const next = self.pending_clip;
        self.pending_clip = null;
        return next;
    }

    fn currentSwooshControl(self: *AudioEngine) live_swoosh.Control {
        self.lock();
        defer self.unlock();
        return self.swoosh_control;
    }

    fn swingParams(control: live_swoosh.Control) model.ModelParams {
        var params = model.ModelParams{};
        params.swing_build_up_ms = control.swing_build_up_ms;
        params.swing_follow_through_ms = control.swing_follow_through_ms;
        return params;
    }

    fn setReady(self: *AudioEngine) void {
        self.lock();
        self.ready = true;
        self.unlock();
    }

    fn setPlaying(self: *AudioEngine, playing: bool) void {
        self.lock();
        self.playing_clip = playing;
        self.unlock();
    }

    fn finishClip(self: *AudioEngine) void {
        self.lock();
        self.playing_clip = false;
        self.completed_clips += 1;
        self.unlock();
    }

    fn noteStreamedBlock(self: *AudioEngine) void {
        self.lock();
        self.streamed_frames += stream_block_frames;
        self.unlock();
    }

    fn publishError(self: *AudioEngine, message: []const u8) void {
        self.lock();
        self.failed = true;
        self.ready = false;
        self.last_error = message;
        self.unlock();
    }

    fn publishWarning(self: *AudioEngine, message: []const u8) void {
        self.lock();
        self.last_error = message;
        self.unlock();
    }

    fn shouldStop(self: *AudioEngine) bool {
        self.lock();
        defer self.unlock();
        return self.stopping;
    }

    fn lock(self: *AudioEngine) void {
        self.mutex.lockUncancelable(self.io);
    }

    fn unlock(self: *AudioEngine) void {
        self.mutex.unlock(self.io);
    }
};
