const std = @import("std");
const sim = @import("simulation.zig");

const fixed_step: f32 = 1.0 / @as(f32, @floatFromInt(sim.base_physics_rate_hz));
const maximum_elapsed_time: f32 = 0.05;
const maximum_steps_per_pass: usize = 8;
const worker_sleep = std.Io.Duration.fromMicroseconds(250);

pub const Snapshot = struct {
    simulation: sim.Simulation,
    pending_time: f32,
    automatic_launch_serial: u64,
};

/// Owns the mutable simulation on a dedicated thread. The renderer only sends
/// short commands and copies snapshots while the state mutex is held.
pub const PhysicsLoop = struct {
    io: std.Io,
    mutex: std.Io.Mutex = .init,
    simulation: sim.Simulation,
    accumulator: f32 = 0,
    time_scale: f32 = 1,
    automatic_launch_serial: u64 = 0,
    stopping: bool = false,
    thread: ?std.Thread = null,

    pub fn init(io: std.Io, simulation: sim.Simulation) PhysicsLoop {
        return .{ .io = io, .simulation = simulation };
    }

    /// Call only after this value is in its final memory location: the worker
    /// keeps a pointer to it until `deinit` joins the thread.
    pub fn start(self: *PhysicsLoop) !void {
        std.debug.assert(self.thread == null);
        self.thread = try std.Thread.spawn(.{}, workerMain, .{self});
    }

    pub fn deinit(self: *PhysicsLoop) void {
        const thread = self.thread orelse return;
        self.lock();
        self.stopping = true;
        self.unlock();
        thread.join();
        self.thread = null;
    }

    pub fn setTimeScale(self: *PhysicsLoop, time_scale: f32) void {
        self.lock();
        self.time_scale = @max(0, time_scale);
        self.unlock();
    }

    /// Returns the latest automatic-launch serial so the caller can suppress a
    /// stale automatic launch sound when this manual launch wins the race.
    pub fn launchShot(self: *PhysicsLoop, shot: sim.ShotType) u64 {
        self.lock();
        defer self.unlock();
        self.simulation.launchShot(shot);
        self.accumulator = 0;
        return self.automatic_launch_serial;
    }

    pub fn snapshot(self: *PhysicsLoop) Snapshot {
        self.lock();
        defer self.unlock();
        return .{
            .simulation = self.simulation,
            .pending_time = self.accumulator,
            .automatic_launch_serial = self.automatic_launch_serial,
        };
    }

    fn workerMain(self: *PhysicsLoop) void {
        var previous = std.Io.Clock.Timestamp.now(self.io, .awake);

        while (true) {
            const now = std.Io.Clock.Timestamp.now(self.io, .awake);
            const elapsed_ns = previous.durationTo(now).raw.nanoseconds;
            previous = now;
            const elapsed_seconds = if (elapsed_ns <= 0)
                0
            else
                @min(maximum_elapsed_time, @as(f32, @floatFromInt(elapsed_ns)) / 1_000_000_000.0);

            self.lock();
            if (self.stopping) {
                self.unlock();
                return;
            }

            self.accumulator += elapsed_seconds * self.time_scale;
            var steps: usize = 0;
            while (self.accumulator >= fixed_step and steps < maximum_steps_per_pass) : (steps += 1) {
                if (self.simulation.step(fixed_step)) self.automatic_launch_serial +%= 1;
                self.accumulator -= fixed_step;
            }
            const catching_up = self.accumulator >= fixed_step;
            self.unlock();

            // Stay responsive at slow motion so substep prediction receives a
            // fresh accumulator even when physical steps are 100 ms apart.
            if (!catching_up) std.Io.sleep(self.io, worker_sleep, .awake) catch {};
        }
    }

    fn lock(self: *PhysicsLoop) void {
        self.mutex.lockUncancelable(self.io);
    }

    fn unlock(self: *PhysicsLoop) void {
        self.mutex.unlock(self.io);
    }
};

test "physics advances without renderer calls" {
    var physics = PhysicsLoop.init(std.testing.io, sim.Simulation.init(12345));
    try physics.start();
    defer physics.deinit();

    try std.Io.sleep(std.testing.io, .fromMilliseconds(100), .awake);
    const state = physics.snapshot();
    try std.testing.expect(state.simulation.flight_time >= 0.005);
}
