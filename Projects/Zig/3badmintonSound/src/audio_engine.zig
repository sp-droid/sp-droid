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
        };
        self.submitted_clips += 1;
        return true;
    }

    /// Updates one persistent aerodynamic source. This is deliberately
    /// independent of submit(): changing velocity never launches a clip.
    pub fn setSwoosh(self: *AudioEngine, profile: model.Profile) void {
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
        var block: [stream_block_frames]f32 = @splat(0.0);
        var swoosh = live_swoosh.Generator.init();

        // Both stream halves begin processed. Filling both before Play avoids
        // an initialization underrun and establishes a continuous timeline.
        for (0..2) |_| {
            self.fillBlock(&block, &active_clip, &cursor, &swoosh);
            rl.updateAudioStream(stream, @ptrCast(block[0..].ptr), block.len);
            self.noteStreamedBlock();
        }
        rl.playAudioStream(stream);
        self.setReady();

        while (!self.shouldStop()) {
            var wrote_block = false;
            while (rl.isAudioStreamProcessed(stream)) {
                self.fillBlock(&block, &active_clip, &cursor, &swoosh);
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
        swoosh: *live_swoosh.Generator,
    ) void {
        @memset(block, 0.0);
        var written: usize = 0;
        while (written < block.len) {
            if (active_clip.* == null) {
                const queued = self.takePendingClip() orelse break;
                const next = self.prepareClip(queued) orelse continue;
                active_clip.* = next;
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
        swoosh.setControl(self.currentSwooshControl());
        swoosh.addTo(block);
    }

    fn prepareClip(self: *AudioEngine, queued: QueuedClip) ?[]f32 {
        if (queued.sample_rate_hz == output_sample_rate_hz) {
            return queued.samples;
        }
        const converted = stream_audio.resampleWindowedSinc(
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
        return converted;
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
