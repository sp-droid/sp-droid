const std = @import("std");
const model = @import("model.zig");
const simulator = @import("simulator.zig");

pub const RenderWorker = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    mutex: std.Io.Mutex = .init,
    pending: ?model.Profile = null,
    completed: ?*simulator.SimulationResult = null,
    last_error: ?[]const u8 = null,
    running: bool = false,
    stopping: bool = false,
    thread: ?std.Thread = null,

    pub fn init(io: std.Io, allocator: std.mem.Allocator) RenderWorker {
        return .{ .io = io, .allocator = allocator };
    }

    pub fn start(self: *RenderWorker) !void {
        self.thread = try std.Thread.spawn(.{}, workerMain, .{self});
    }

    pub fn deinit(self: *RenderWorker) void {
        if (self.thread) |thread| {
            self.lock();
            self.stopping = true;
            self.unlock();
            thread.join();
            self.thread = null;
        }
        if (self.completed) |result| {
            result.deinit();
            self.allocator.destroy(result);
            self.completed = null;
        }
    }

    /// Queues one immutable profile snapshot. A running or uncollected render
    /// is deliberately skipped so timer ticks can never build a backlog.
    pub fn request(self: *RenderWorker, profile: model.Profile) bool {
        self.lock();
        defer self.unlock();
        if (self.stopping or self.running or
            self.pending != null or self.completed != null) return false;
        self.pending = profile;
        self.last_error = null;
        self.running = true;
        return true;
    }

    /// Transfers ownership of the completed result to the main thread.
    pub fn takeCompleted(self: *RenderWorker) ?*simulator.SimulationResult {
        self.lock();
        defer self.unlock();
        const result = self.completed;
        self.completed = null;
        return result;
    }

    pub fn takeError(self: *RenderWorker) ?[]const u8 {
        self.lock();
        defer self.unlock();
        const render_error = self.last_error;
        self.last_error = null;
        return render_error;
    }

    pub fn isRunning(self: *RenderWorker) bool {
        self.lock();
        defer self.unlock();
        return self.running;
    }

    fn workerMain(self: *RenderWorker) void {
        while (true) {
            self.lock();
            if (self.stopping) {
                self.unlock();
                return;
            }
            const next = self.pending;
            self.pending = null;
            self.unlock();

            if (next) |profile| {
                self.render(profile);
                continue;
            }
            std.Io.sleep(self.io, .fromMilliseconds(1), .awake) catch {};
        }
    }

    fn render(self: *RenderWorker, profile: model.Profile) void {
        const started = std.Io.Clock.Timestamp.now(self.io, .awake);
        const result = self.allocator.create(simulator.SimulationResult) catch {
            self.publishError("OutOfMemory");
            return;
        };
        result.* = simulator.simulate(self.allocator, profile) catch |err| {
            self.allocator.destroy(result);
            self.publishError(@errorName(err));
            return;
        };
        const finished = std.Io.Clock.Timestamp.now(self.io, .awake);
        const elapsed_ns = started.durationTo(finished).raw.nanoseconds;
        result.diagnostics.render_time_ms =
            @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

        self.lock();
        if (self.stopping) {
            self.unlock();
            result.deinit();
            self.allocator.destroy(result);
            return;
        }
        self.completed = result;
        self.running = false;
        self.unlock();
    }

    fn publishError(self: *RenderWorker, message: []const u8) void {
        self.lock();
        self.last_error = message;
        self.running = false;
        self.unlock();
    }

    fn lock(self: *RenderWorker) void {
        self.mutex.lockUncancelable(self.io);
    }

    fn unlock(self: *RenderWorker) void {
        self.mutex.unlock(self.io);
    }
};
