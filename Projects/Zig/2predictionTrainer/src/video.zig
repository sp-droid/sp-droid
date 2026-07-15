const std = @import("std");
const rl = @import("raylib");
const mf = @import("media_foundation.zig");

pub const queue_capacity = 12;
pub const video_width = 1280;
pub const video_height = 720;
const y_bytes = video_width * video_height;
const uv_bytes = y_bytes / 2;
const max_path_bytes = 2048;

pub const Status = enum(u8) {
    idle,
    loading,
    ready,
    completed,
    failed,
};

const Command = struct {
    generation: u32 = 0,
    path_len: u16 = 0,
    path: [max_path_bytes]u8 = @splat(0),
    start_frame: u32 = 0,
    end_frame: u32 = 0,
    annotated_frame_count: u32 = 0,
};

const FrameSlot = struct {
    generation: u32 = 0,
    frame: u32 = 0,
    timestamp_hns: i64 = 0,
    duration_hns: i64 = 0,
    y: []u8,
    uv: []u8,
};

const SpinLock = struct {
    state: std.atomic.Value(u8) = .init(0),

    fn lock(self: *SpinLock) void {
        while (self.state.cmpxchgWeak(0, 1, .acquire, .monotonic) != null) spinPause();
    }

    fn unlock(self: *SpinLock) void {
        self.state.store(0, .release);
    }
};

pub const FrameView = struct {
    sequence: u64,
    frame: u32,
    timestamp_hns: i64,
    duration_hns: i64,
    y: []const u8,
    uv: []const u8,
};

pub const DecoderThread = struct {
    allocator: std.mem.Allocator,
    slots: []FrameSlot,
    thread: std.Thread,
    command_lock: SpinLock = .{},
    command: Command = .{},
    requested_generation: std.atomic.Value(u32) = .init(0),
    next_generation: std.atomic.Value(u32) = .init(0),
    active_generation: u32 = 0,
    read_sequence: std.atomic.Value(u64) = .init(0),
    write_sequence: std.atomic.Value(u64) = .init(0),
    shutdown_requested: std.atomic.Value(bool) = .init(false),
    status_generation: std.atomic.Value(u32) = .init(0),
    status_value: std.atomic.Value(u8) = .init(@intFromEnum(Status.idle)),

    pub fn create(allocator: std.mem.Allocator) !*DecoderThread {
        const self = try allocator.create(DecoderThread);
        errdefer allocator.destroy(self);
        const slots = try allocator.alloc(FrameSlot, queue_capacity);
        errdefer allocator.free(slots);
        var initialized: usize = 0;
        errdefer {
            for (slots[0..initialized]) |slot| {
                allocator.free(slot.y);
                allocator.free(slot.uv);
            }
        }
        for (slots) |*slot| {
            slot.* = .{
                .y = try allocator.alloc(u8, y_bytes),
                .uv = try allocator.alloc(u8, uv_bytes),
            };
            initialized += 1;
        }
        self.* = .{
            .allocator = allocator,
            .slots = slots,
            .thread = undefined,
        };
        self.thread = try std.Thread.spawn(.{}, workerMain, .{self});
        return self;
    }

    pub fn destroy(self: *DecoderThread) void {
        self.shutdown_requested.store(true, .release);
        _ = self.requested_generation.fetchAdd(1, .acq_rel);
        self.thread.join();
        for (self.slots) |slot| {
            self.allocator.free(slot.y);
            self.allocator.free(slot.uv);
        }
        self.allocator.free(self.slots);
        const allocator = self.allocator;
        allocator.destroy(self);
    }

    pub fn request(
        self: *DecoderThread,
        path: []const u8,
        start_frame: u32,
        end_frame: u32,
        annotated_frame_count: u32,
    ) !u32 {
        if (path.len + 1 > max_path_bytes) return error.VideoPathTooLong;
        if (start_frame > end_frame or end_frame >= annotated_frame_count) return error.InvalidPlaybackRange;
        const generation = self.next_generation.fetchAdd(1, .acq_rel) + 1;
        self.command_lock.lock();
        self.command = .{
            .generation = generation,
            .path_len = @intCast(path.len),
            .start_frame = start_frame,
            .end_frame = end_frame,
            .annotated_frame_count = annotated_frame_count,
        };
        @memcpy(self.command.path[0..path.len], path);
        self.command.path[path.len] = 0;
        self.command_lock.unlock();
        self.active_generation = generation;
        self.requested_generation.store(generation, .release);
        self.setStatus(generation, .loading);
        return generation;
    }

    pub fn status(self: *const DecoderThread) Status {
        const value: Status = @enumFromInt(self.status_value.load(.acquire));
        if (self.status_generation.load(.acquire) != self.active_generation) return .loading;
        return value;
    }

    pub fn peek(self: *DecoderThread) ?FrameView {
        while (true) {
            const sequence = self.read_sequence.load(.monotonic);
            if (sequence == self.write_sequence.load(.acquire)) return null;
            const slot = &self.slots[sequence % queue_capacity];
            if (slot.generation != self.active_generation) {
                self.read_sequence.store(sequence + 1, .release);
                continue;
            }
            return .{
                .sequence = sequence,
                .frame = slot.frame,
                .timestamp_hns = slot.timestamp_hns,
                .duration_hns = slot.duration_hns,
                .y = slot.y,
                .uv = slot.uv,
            };
        }
    }

    pub fn release(self: *DecoderThread, frame: FrameView) void {
        const sequence = self.read_sequence.load(.monotonic);
        std.debug.assert(sequence == frame.sequence);
        self.read_sequence.store(sequence + 1, .release);
    }

    fn setStatus(self: *DecoderThread, generation: u32, status_value: Status) void {
        self.status_generation.store(generation, .release);
        self.status_value.store(@intFromEnum(status_value), .release);
    }
};

fn workerMain(self: *DecoderThread) void {
    var handled_generation: u32 = 0;
    while (!self.shutdown_requested.load(.acquire)) {
        const requested = self.requested_generation.load(.acquire);
        if (requested == 0 or requested == handled_generation) {
            spinPause();
            continue;
        }
        handled_generation = requested;
        self.command_lock.lock();
        const command = self.command;
        self.command_lock.unlock();
        if (command.generation != requested) continue;
        decodeCommand(self, command) catch |err| {
            if (self.requested_generation.load(.acquire) == command.generation and
                !self.shutdown_requested.load(.acquire))
            {
                std.log.err("decoder worker failed for frame range {d}-{d}: {s}", .{
                    command.start_frame, command.end_frame, @errorName(err),
                });
                self.setStatus(command.generation, .failed);
            }
        };
    }
}

fn decodeCommand(self: *DecoderThread, command: Command) !void {
    const path_z: [:0]const u8 = command.path[0..command.path_len :0];
    var decoder = try mf.Decoder.open(path_z);
    defer decoder.close();
    if (decoder.info.width != video_width or decoder.info.height != video_height) return error.UnsupportedVideoResolution;
    if (decoder.info.frame_rate_num == 0 or decoder.info.frame_rate_den == 0) return error.InvalidVideoFrameRate;

    const pre_roll_frames: u32 = @intFromFloat(@ceil(
        2.0 * @as(f64, @floatFromInt(decoder.info.frame_rate_num)) /
            @as(f64, @floatFromInt(decoder.info.frame_rate_den)),
    ));
    const seek_frame = command.start_frame -| pre_roll_frames;
    try decoder.seek(frameTimestamp(seek_frame, decoder.info.frame_rate_num, decoder.info.frame_rate_den));
    var retried_from_zero = false;
    var published_any = false;

    while (!self.shutdown_requested.load(.acquire) and
        self.requested_generation.load(.acquire) == command.generation)
    {
        const write_sequence = self.write_sequence.load(.monotonic);
        if (write_sequence - self.read_sequence.load(.acquire) >= queue_capacity) {
            spinPause();
            continue;
        }
        const slot = &self.slots[write_sequence % queue_capacity];
        const frame_info = (try decoder.read(slot.y, slot.uv)) orelse {
            self.setStatus(command.generation, .completed);
            return;
        };
        const ordinal = timestampFrame(
            frame_info.timestamp_hns,
            decoder.info.frame_rate_num,
            decoder.info.frame_rate_den,
        );
        if (!published_any and ordinal > command.start_frame and !retried_from_zero) {
            try decoder.seek(0);
            retried_from_zero = true;
            continue;
        }
        if (ordinal < command.start_frame) continue;
        if (ordinal >= command.annotated_frame_count or ordinal > command.end_frame) {
            self.setStatus(command.generation, .completed);
            return;
        }

        slot.generation = command.generation;
        slot.frame = ordinal;
        slot.timestamp_hns = frame_info.timestamp_hns;
        slot.duration_hns = if (frame_info.duration_hns > 0)
            frame_info.duration_hns
        else
            frameTimestamp(1, decoder.info.frame_rate_num, decoder.info.frame_rate_den);
        self.write_sequence.store(write_sequence + 1, .release);
        published_any = true;
        self.setStatus(command.generation, .ready);
    }
}

pub fn frameTimestamp(frame: u32, rate_num: u32, rate_den: u32) i64 {
    const numerator = @as(u128, frame) * 10_000_000 * rate_den;
    return @intCast((numerator + rate_num / 2) / rate_num);
}

pub fn timestampFrame(timestamp_hns: i64, rate_num: u32, rate_den: u32) u32 {
    if (timestamp_hns <= 0) return 0;
    const numerator = @as(u128, @intCast(timestamp_hns)) * rate_num;
    const denominator = @as(u128, 10_000_000) * rate_den;
    return @intCast((numerator + denominator / 2) / denominator);
}

fn spinPause() void {
    std.Thread.yield() catch {};
}

pub const Surface = struct {
    y_texture: rl.Texture2D,
    uv_texture: rl.Texture2D,
    shader: rl.Shader,
    uv_location: i32,
    current_frame: ?u32 = null,

    pub fn init(allocator: std.mem.Allocator) !Surface {
        const y = try allocator.alloc(u8, y_bytes);
        defer allocator.free(y);
        const uv = try allocator.alloc(u8, uv_bytes);
        defer allocator.free(uv);
        @memset(y, 16);
        @memset(uv, 128);
        const y_image = rl.Image{
            .data = @ptrCast(y.ptr),
            .width = video_width,
            .height = video_height,
            .mipmaps = 1,
            .format = .uncompressed_grayscale,
        };
        const uv_image = rl.Image{
            .data = @ptrCast(uv.ptr),
            .width = video_width / 2,
            .height = video_height / 2,
            .mipmaps = 1,
            .format = .uncompressed_gray_alpha,
        };
        const y_texture = try rl.loadTextureFromImage(y_image);
        errdefer rl.unloadTexture(y_texture);
        const uv_texture = try rl.loadTextureFromImage(uv_image);
        errdefer rl.unloadTexture(uv_texture);
        rl.setTextureFilter(y_texture, .bilinear);
        rl.setTextureFilter(uv_texture, .bilinear);
        const shader = try rl.loadShaderFromMemory(null, nv12_fragment_shader);
        errdefer rl.unloadShader(shader);
        const uv_location = rl.getShaderLocation(shader, "uvTexture");
        return .{
            .y_texture = y_texture,
            .uv_texture = uv_texture,
            .shader = shader,
            .uv_location = uv_location,
        };
    }

    pub fn deinit(self: *Surface) void {
        rl.unloadShader(self.shader);
        rl.unloadTexture(self.uv_texture);
        rl.unloadTexture(self.y_texture);
        self.* = undefined;
    }

    pub fn upload(self: *Surface, frame: FrameView) void {
        rl.updateTexture(self.y_texture, @ptrCast(frame.y.ptr));
        rl.updateTexture(self.uv_texture, @ptrCast(frame.uv.ptr));
        self.current_frame = frame.frame;
    }

    pub fn draw(self: *Surface, destination: rl.Rectangle) void {
        rl.beginShaderMode(self.shader);
        defer rl.endShaderMode();
        // Binding before BeginShaderMode lets the intervening batch flush drop
        // texture unit 1, causing uvTexture to sample the luma texture instead.
        rl.setShaderValueTexture(self.shader, self.uv_location, self.uv_texture);
        rl.drawTexturePro(
            self.y_texture,
            .{ .x = 0, .y = 0, .width = video_width, .height = video_height },
            destination,
            .{ .x = 0, .y = 0 },
            0,
            .white,
        );
    }
};

const nv12_fragment_shader =
    \\#version 330
    \\in vec2 fragTexCoord;
    \\in vec4 fragColor;
    \\uniform sampler2D texture0;
    \\uniform sampler2D uvTexture;
    \\uniform vec4 colDiffuse;
    \\out vec4 finalColor;
    \\void main() {
    \\    float y = texture(texture0, fragTexCoord).r;
    \\    vec2 uv = texture(uvTexture, fragTexCoord).rg;
    \\    float luma = 1.16438356 * (y - 0.06274510);
    \\    float u = uv.x - 0.5;
    \\    float v = uv.y - 0.5;
    \\    vec3 rgb = vec3(
    \\        luma + 1.79274107 * v,
    \\        luma - 0.21324861 * u - 0.53290933 * v,
    \\        luma + 2.11240179 * u
    \\    );
    \\    finalColor = vec4(clamp(rgb, 0.0, 1.0), 1.0) * colDiffuse * fragColor;
    \\}
;
