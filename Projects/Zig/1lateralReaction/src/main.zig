// raylib-zig Reaction Ball Game
// A breakout-style game where a ball fires down, and the player controls a paddle

const std = @import("std");
const rl = @import("raylib");
const builtin = @import("builtin");

// ============================================================================
// GAME STATE STRUCTURES
// ============================================================================

const BallState = enum { idle, active, bouncing };
const FeedbackType = enum { none, hit, miss };
const AppScreen = enum { game, history };
const ChartMetric = enum { average_speed, max_speed };

const Date = struct {
    year: i32,
    month: u8,
    day: u8,
};

const WindowsSystemTime = extern struct {
    year: u16,
    month: u16,
    day_of_week: u16,
    day: u16,
    hour: u16,
    minute: u16,
    second: u16,
    milliseconds: u16,
};

const WindowsTime = struct {
    extern "kernel32" fn GetLocalTime(system_time: *WindowsSystemTime) callconv(.winapi) void;
};

const SessionSnapshot = struct {
    average_speed_kmh: f32,
    max_speed_kmh: f32,
};

const SessionStats = struct {
    launch_speed_total: f64 = 0,
    shots_launched: u32 = 0,
    max_launch_speed_kmh: f32 = 0,

    fn recordLaunch(self: *SessionStats, speed_kmh: f32) void {
        self.launch_speed_total += @as(f64, speed_kmh);
        self.shots_launched += 1;
        self.max_launch_speed_kmh = @max(self.max_launch_speed_kmh, speed_kmh);
    }

    fn snapshot(self: SessionStats) SessionSnapshot {
        const average_speed = if (self.shots_launched == 0)
            0.0
        else
            self.launch_speed_total / @as(f64, @floatFromInt(self.shots_launched));
        return .{
            .average_speed_kmh = @floatCast(average_speed),
            .max_speed_kmh = self.max_launch_speed_kmh,
        };
    }
};

const HistoryEntry = struct {
    date: Date,
    average_speed_kmh: f32,
    max_speed_kmh: f32,
};

const HistoryState = struct {
    entries: [MAX_HISTORY_ENTRIES]HistoryEntry = undefined,
    start: usize = 0,
    count: usize = 0,
    current_session: SessionSnapshot = .{ .average_speed_kmh = 0, .max_speed_kmh = 0 },
    session_finalized: bool = false,
    record_eligible: bool = false,
    save_succeeded: bool = false,
    next_start_speed_kmh: f32 = BASE_BALL_SPEED,

    fn clear(self: *HistoryState) void {
        self.start = 0;
        self.count = 0;
    }

    fn add(self: *HistoryState, entry: HistoryEntry) void {
        if (self.count < MAX_HISTORY_ENTRIES) {
            const index = (self.start + self.count) % MAX_HISTORY_ENTRIES;
            self.entries[index] = entry;
            self.count += 1;
            return;
        }

        self.entries[self.start] = entry;
        self.start = (self.start + 1) % MAX_HISTORY_ENTRIES;
    }

    fn get(self: *const HistoryState, index: usize) HistoryEntry {
        return self.entries[(self.start + index) % MAX_HISTORY_ENTRIES];
    }
};

const FixedStepClock = struct {
    accumulator_seconds: f64 = 0,

    fn pushFrame(self: *FixedStepClock, frame_time_seconds: f32) void {
        if (!std.math.isFinite(frame_time_seconds) or frame_time_seconds <= 0) return;
        self.accumulator_seconds += @as(f64, @floatCast(frame_time_seconds));
    }

    fn takeStep(self: *FixedStepClock) bool {
        if (self.accumulator_seconds < FIXED_SIMULATION_STEP_SECONDS) return false;
        self.accumulator_seconds -= FIXED_SIMULATION_STEP_SECONDS;
        return true;
    }
};

const Ball = struct {
    position: rl.Vector2,
    velocity: rl.Vector2,
    radius: f32,
    state: BallState,
    target_position: rl.Vector2, // Real target selected before the warning appears
    cue_target_position: rl.Vector2, // May intentionally disagree with target_position
};

const Paddle = struct {
    position: rl.Vector2, // Bottom-left corner
    width: f32,
    height: f32,
};

const GameSettings = struct {
    speed_multiplier: f32, // 0.5 - 3.0
    paddle_width: f32, // 50 - 300
    paddle_height: f32, // 20 (fixed for now)
};

const GameState = struct {
    ball: Ball,
    paddle: Paddle,
    settings: GameSettings,
    score: i32,
    attempts: i32,
    current_speed: f32, // Dynamic ball speed in km/h (increases on hit, decreases on miss)
    countdown_timer: f32, // Time until ball fires
    bounce_delay_timer: f32, // Delay before resetting after bounce/miss
    last_feedback: FeedbackType, // Feedback type (hit, miss, or none)
    feedback_timer: f32, // How long to display feedback
    sound_cooldown: f32, // Cooldown for sound effects
    hit_sound: rl.Sound,
    miss_sound: rl.Sound,
    session_stats: SessionStats,
    session_elapsed: f32,
    // Text caching for UI
    score_text_buf: [64:0]u8 = undefined,
    speed_text_buf: [32:0]u8 = undefined,
    paddle_text_buf: [32:0]u8 = undefined,
    score_text: [:0]const u8 = "",
    speed_text: [:0]const u8 = "",
    paddle_text: [:0]const u8 = "",
    last_score: i32 = 0,
    last_attempts: i32 = 0,
    last_current_speed: i32 = 0,
    last_paddle_width: i32 = 0,
    // Feedback text buffer
    feedback_text_buf: [64:0]u8 = undefined,
    feedback_text: [:0]const u8 = "",
    // Countdown timer (10 minutes)
    timer_countdown: f32 = COUNTDOWN_DURATION,
    timer_text_buf: [32:0]u8 = undefined,
    timer_text: [:0]const u8 = "",
    last_timer_seconds: i32 = @intFromFloat(COUNTDOWN_DURATION),
};

// ============================================================================
// CONSTANTS
// ============================================================================

const BALL_RADIUS = 15.0;
const ARROW_LEAD_TIME = 0.3;
const SLOW_SHOT_CHANCE_PERCENT = 10;
const SLOW_SHOT_SPEED_REDUCTION = 100.0; // km/h
const MIN_LAUNCH_SPEED = 10.0; // Prevent a very low setting from reversing the shot

const HISTORY_FILE_NAME = "history.csv";
const HISTORY_HEADER = "date,average_ball_speed_kmh,max_ball_speed_kmh\n";
const SETTINGS_FILE_NAME = "settings.csv";
const SETTINGS_HEADER = "starting_speed_kmh\n";
const MIN_SESSION_RECORD_SECONDS = 5.0 * 60.0;
const MAX_HISTORY_ENTRIES = 4096;
const HISTORY_READ_BUFFER_SIZE = 512 * 1024;

const MONTH_NAMES = [_][]const u8{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

var history_read_buffer: [HISTORY_READ_BUFFER_SIZE]u8 = undefined;

fn getFireDelay() f32 {
    // Always leave enough time for the complete 0.3 second direction cue.
    return randomF32(0.45, 0.75);
}

// Physics constants
// Distance from ball start to bottom = 10 meters
// Pixel distance: SCREEN_HEIGHT - BALL_START_Y = 980 pixels = 10 meters
const BASE_BALL_SPEED = 500.0; // km/h
const STARTING_SPEED_MIN = BASE_BALL_SPEED * 0.5;
const STARTING_SPEED_MAX = BASE_BALL_SPEED * 3.0;
const DRAG_COEFFICIENT = 0.45; // Badminton shuttlecock with feathered skirt
const AIR_DENSITY = 1.225; // kg/m^3 at sea level
const SHUTTLECOCK_MASS = 0.005; // kg (approximately 5 grams)
const SHUTTLECOCK_AREA = 0.0034212; // m^2 (cross-sectional area including feathered skirt)
const DRAG_CONSTANT = (AIR_DENSITY * DRAG_COEFFICIENT * SHUTTLECOCK_AREA) / (2.0 * SHUTTLECOCK_MASS);

const FEEDBACK_DISPLAY_TIME = 0.4; // How long to show "Hit"/"Miss" text
const BOUNCE_DELAY = 0.3; // Delay before resetting after collision or miss

// Countdown timer
const COUNTDOWN_DURATION = 600.0; // 10 minutes in seconds

// Fixed simulation timing. Rendering may run at any rate; gameplay always
// advances in 1/240-second ticks. Eight collision/drag substeps retain roughly
// the original integration precision without making it frame-rate dependent.
const SIMULATION_HZ: usize = 240;
const FIXED_SIMULATION_STEP_SECONDS: f64 = 1.0 / @as(f64, @floatFromInt(SIMULATION_HZ));
const FIXED_SIMULATION_DT: f32 = @floatCast(FIXED_SIMULATION_STEP_SECONDS);
const PHYSICS_SUBSTEPS_PER_TICK: usize = 8;
const MAX_FIXED_UPDATES_PER_FRAME: usize = SIMULATION_HZ * 2;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

fn randomF32(min: f32, max: f32) f32 {
    const rand_val: f32 = @floatFromInt(rl.getRandomValue(0, 10000));
    const normalized = rand_val / 10000.0;
    return min + normalized * (max - min);
}

fn clampF32(value: f32, min: f32, max: f32) f32 {
    if (value < min) return min;
    if (value > max) return max;
    return value;
}

fn normalizeStartingSpeed(value: f32) f32 {
    if (!std.math.isFinite(value)) return BASE_BALL_SPEED;
    return clampF32(value, STARTING_SPEED_MIN, STARTING_SPEED_MAX);
}

fn startingSpeedMultiplier(speed_kmh: f32) f32 {
    return normalizeStartingSpeed(speed_kmh) / BASE_BALL_SPEED;
}

fn loadStartingSpeed(io: std.Io) f32 {
    var file = std.Io.Dir.cwd().openFile(io, SETTINGS_FILE_NAME, .{}) catch return BASE_BALL_SPEED;
    defer file.close(io);

    var buffer: [256]u8 = undefined;
    const bytes_read = file.readPositionalAll(io, &buffer, 0) catch return BASE_BALL_SPEED;
    var lines = std.mem.splitScalar(u8, buffer[0..bytes_read], '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \r\t");
        if (line.len == 0 or std.mem.eql(u8, line, "starting_speed_kmh")) continue;

        const speed = std.fmt.parseFloat(f32, line) catch return BASE_BALL_SPEED;
        return normalizeStartingSpeed(speed);
    }

    return BASE_BALL_SPEED;
}

fn saveStartingSpeed(io: std.Io, speed_kmh: f32) !void {
    var file = try std.Io.Dir.cwd().createFile(io, SETTINGS_FILE_NAME, .{
        .truncate = true,
        .lock = .exclusive,
    });
    defer file.close(io);

    var buffer: [96]u8 = undefined;
    const contents = try std.fmt.bufPrint(
        &buffer,
        SETTINGS_HEADER ++ "{d:.2}\n",
        .{normalizeStartingSpeed(speed_kmh)},
    );
    try file.writePositionalAll(io, contents, 0);
}

fn applyDrag(velocity: *rl.Vector2, dt: f32) void {
    // Calculate velocity magnitude in m/s
    const speed_sq = velocity.x * velocity.x + velocity.y * velocity.y;
    if (speed_sq < 0.001) return; // Avoid division by near-zero

    const speed = @sqrt(speed_sq);

    // Apply drag: dv = -0.5 * Cd * ρ * A * v² * dt / m
    const dv = -0.5 * DRAG_COEFFICIENT * AIR_DENSITY * SHUTTLECOCK_AREA * speed_sq * dt / SHUTTLECOCK_MASS;

    if (dv <= -speed) {
        // Drag stops the ball
        velocity.x = 0;
        velocity.y = 0;
    } else {
        // Reduce velocity by dv amount (apply to both components)
        const reduction_factor = (speed + dv) / speed;
        velocity.x *= reduction_factor;
        velocity.y *= reduction_factor;
    }
}

fn dateFromUnixTimestamp(timestamp: i64) Date {
    // Civil date conversion by era. Keeping it here avoids locale-dependent
    // CSV output while producing a stable YYYY-MM-DD date on every platform.
    const days = @divFloor(timestamp, 86_400);
    const z = days + 719_468;
    const era = @divFloor(z, 146_097);
    const day_of_era = z - era * 146_097;
    const year_of_era = @divFloor(
        day_of_era - @divFloor(day_of_era, 1_460) + @divFloor(day_of_era, 36_524) - @divFloor(day_of_era, 146_096),
        365,
    );
    var year = year_of_era + era * 400;
    const day_of_year = day_of_era - (365 * year_of_era + @divFloor(year_of_era, 4) - @divFloor(year_of_era, 100));
    const month_prime = @divFloor(5 * day_of_year + 2, 153);
    const day = day_of_year - @divFloor(153 * month_prime + 2, 5) + 1;
    const month = month_prime + (if (month_prime < 10) @as(i64, 3) else -9);
    if (month <= 2) year += 1;

    return .{
        .year = @intCast(year),
        .month = @intCast(month),
        .day = @intCast(day),
    };
}

fn currentDate(io: std.Io) Date {
    if (builtin.os.tag == .windows) {
        var local_time: WindowsSystemTime = undefined;
        WindowsTime.GetLocalTime(&local_time);
        return .{
            .year = @intCast(local_time.year),
            .month = @intCast(local_time.month),
            .day = @intCast(local_time.day),
        };
    }

    return dateFromUnixTimestamp(std.Io.Clock.real.now(io).toSeconds());
}

fn parseDate(text: []const u8) ?Date {
    if (text.len != 10 or text[4] != '-' or text[7] != '-') return null;

    const year = std.fmt.parseInt(i32, text[0..4], 10) catch return null;
    const month = std.fmt.parseInt(u8, text[5..7], 10) catch return null;
    const day = std.fmt.parseInt(u8, text[8..10], 10) catch return null;
    if (month < 1 or month > 12 or day < 1 or day > 31) return null;

    return .{ .year = year, .month = month, .day = day };
}

fn historyEntry(date: Date, snapshot: SessionSnapshot) HistoryEntry {
    return .{
        .date = date,
        .average_speed_kmh = snapshot.average_speed_kmh,
        .max_speed_kmh = snapshot.max_speed_kmh,
    };
}

fn shouldRecordSession(elapsed_seconds: f32) bool {
    return elapsed_seconds > MIN_SESSION_RECORD_SECONDS;
}

fn appendHistoryEntry(io: std.Io, entry: HistoryEntry) !void {
    var file = try std.Io.Dir.cwd().createFile(io, HISTORY_FILE_NAME, .{
        .read = true,
        .truncate = false,
        .lock = .exclusive,
    });
    defer file.close(io);

    var write_position = try file.length(io);
    if (write_position == 0) {
        try file.writePositionalAll(io, HISTORY_HEADER, write_position);
        write_position += HISTORY_HEADER.len;
    }

    var row_buffer: [128]u8 = undefined;
    const row = try std.fmt.bufPrint(
        &row_buffer,
        "{d:0>4}-{d:0>2}-{d:0>2},{d:.2},{d:.2}\n",
        .{ entry.date.year, entry.date.month, entry.date.day, entry.average_speed_kmh, entry.max_speed_kmh },
    );
    try file.writePositionalAll(io, row, write_position);
}

fn loadHistory(io: std.Io, history: *HistoryState) !void {
    history.clear();

    var file = std.Io.Dir.cwd().openFile(io, HISTORY_FILE_NAME, .{}) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer file.close(io);

    const file_size = try file.length(io);
    const buffer_size: u64 = HISTORY_READ_BUFFER_SIZE;
    const read_start = if (file_size > buffer_size) file_size - buffer_size else 0;
    const bytes_read = try file.readPositionalAll(io, &history_read_buffer, read_start);

    var contents: []const u8 = history_read_buffer[0..bytes_read];
    if (read_start > 0) {
        if (std.mem.indexOfScalar(u8, contents, '\n')) |first_newline| {
            contents = contents[first_newline + 1 ..];
        } else {
            return;
        }
    }

    var lines = std.mem.splitScalar(u8, contents, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \r\t");
        if (line.len == 0 or std.mem.startsWith(u8, line, "date,")) continue;

        var columns = std.mem.splitScalar(u8, line, ',');
        const date_text = std.mem.trim(u8, columns.next() orelse continue, " \r\t");
        const average_speed_text = std.mem.trim(u8, columns.next() orelse continue, " \r\t");
        const max_speed_text = std.mem.trim(u8, columns.next() orelse continue, " \r\t");

        const date = parseDate(date_text) orelse continue;
        const average_speed = std.fmt.parseFloat(f32, average_speed_text) catch continue;
        const max_speed = std.fmt.parseFloat(f32, max_speed_text) catch continue;
        if (!std.math.isFinite(average_speed) or average_speed < 0 or !std.math.isFinite(max_speed) or max_speed < 0) continue;

        history.add(.{
            .date = date,
            .average_speed_kmh = average_speed,
            .max_speed_kmh = max_speed,
        });
    }
}

fn finalizeSession(io: std.Io, game_state: *const GameState, history: *HistoryState) void {
    if (history.session_finalized) return;

    history.session_finalized = true;
    history.current_session = game_state.session_stats.snapshot();
    const current_entry = historyEntry(currentDate(io), history.current_session);
    history.record_eligible = shouldRecordSession(game_state.session_elapsed);

    if (history.record_eligible) {
        history.save_succeeded = true;
        appendHistoryEntry(io, current_entry) catch {
            history.save_succeeded = false;
        };
    }

    var load_succeeded = true;
    loadHistory(io, history) catch {
        history.clear();
        load_succeeded = false;
    };

    // An eligible current session remains visible when disk I/O fails, while
    // short sessions are deliberately absent from both CSV and chart history.
    if (history.record_eligible and (!history.save_succeeded or !load_succeeded or history.count == 0)) {
        history.add(current_entry);
    }
}

// ============================================================================
// GAME INITIALIZATION & LOGIC
// ============================================================================

fn randomTargetX() f32 {
    return randomF32(BALL_RADIUS, SCREEN_WIDTH - BALL_RADIUS);
}

fn chooseWrongTargetX(real_target_x: f32) f32 {
    const minimum_separation = @min(180.0, SCREEN_WIDTH * 0.15);

    for (0..16) |_| {
        const candidate = randomTargetX();
        if (@abs(candidate - real_target_x) >= minimum_separation) return candidate;
    }

    // Deterministic fallback keeps the lie visibly different and on-screen.
    return if (real_target_x < SCREEN_WIDTH / 2.0)
        SCREEN_WIDTH - BALL_RADIUS
    else
        BALL_RADIUS;
}

fn prepareNextShot(game_state: *GameState) void {
    const launch_x = randomF32(LAUNCH_RECT_X, LAUNCH_RECT_X + LAUNCH_RECT_WIDTH);
    const launch_y = randomF32(LAUNCH_RECT_Y, LAUNCH_RECT_Y + LAUNCH_RECT_HEIGHT);
    const target_x = randomTargetX();
    const target_y = PADDLE_Y;

    game_state.ball.position = .{ .x = launch_x, .y = launch_y };
    game_state.ball.velocity = .{ .x = 0, .y = 0 };
    game_state.ball.state = .idle;
    game_state.ball.target_position = .{ .x = target_x, .y = target_y };

    const cue_is_correct = rl.getRandomValue(0, 1) == 1;
    game_state.ball.cue_target_position = .{
        .x = if (cue_is_correct) target_x else chooseWrongTargetX(target_x),
        .y = target_y,
    };

    game_state.countdown_timer = getFireDelay();
    game_state.bounce_delay_timer = 0;
}

fn initGame(starting_speed_kmh: f32) GameState {
    const normalized_start_speed = normalizeStartingSpeed(starting_speed_kmh);
    var game_state: GameState = undefined;

    // Initialize ball with random position in launch rectangle
    const launch_x = randomF32(LAUNCH_RECT_X, LAUNCH_RECT_X + LAUNCH_RECT_WIDTH);
    const launch_y = randomF32(LAUNCH_RECT_Y, LAUNCH_RECT_Y + LAUNCH_RECT_HEIGHT);
    game_state.ball = Ball{
        .position = rl.Vector2{ .x = launch_x, .y = launch_y },
        .velocity = rl.Vector2{ .x = 0, .y = 0 },
        .radius = BALL_RADIUS,
        .state = BallState.idle,
        .target_position = rl.Vector2{ .x = 0, .y = 0 },
        .cue_target_position = rl.Vector2{ .x = 0, .y = 0 },
    };

    // Initialize paddle
    game_state.paddle = Paddle{
        .position = rl.Vector2{ .x = SCREEN_WIDTH / 2.0, .y = PADDLE_Y },
        .width = 200.0,
        .height = PADDLE_HEIGHT,
    };

    // The persisted starting speed maps onto the existing speed multiplier,
    // leaving the hit/miss progression logic unchanged.
    game_state.settings = GameSettings{
        .speed_multiplier = startingSpeedMultiplier(normalized_start_speed),
        .paddle_width = 200.0,
        .paddle_height = PADDLE_HEIGHT,
    };

    // Initialize game state
    game_state.score = 0;
    game_state.attempts = 0;
    game_state.current_speed = BASE_BALL_SPEED; // Start at base speed
    game_state.countdown_timer = getFireDelay();
    game_state.bounce_delay_timer = 0;
    game_state.last_feedback = FeedbackType.none;
    game_state.feedback_timer = 0;
    game_state.sound_cooldown = 0;
    game_state.session_stats = .{};
    game_state.session_elapsed = 0;
    // Initialize cached text on first frame
    game_state.last_score = 0;
    game_state.last_attempts = 0;
    game_state.last_current_speed = 0;
    game_state.last_paddle_width = 0;
    game_state.timer_countdown = COUNTDOWN_DURATION;
    game_state.last_timer_seconds = @intFromFloat(COUNTDOWN_DURATION);
    game_state.timer_text = std.fmt.bufPrintZ(&game_state.timer_text_buf, "10:00", .{}) catch "Error";
    game_state.score_text = std.fmt.bufPrintZ(&game_state.score_text_buf, "Score: {d} / Attempts: {d}", .{ 0, 0 }) catch "Error";
    game_state.speed_text = std.fmt.bufPrintZ(&game_state.speed_text_buf, "{d:.0} km/h", .{normalized_start_speed}) catch "Error";
    game_state.paddle_text = std.fmt.bufPrintZ(&game_state.paddle_text_buf, "{d:.0}px", .{200.0}) catch "Error";

    // Load sound effects from resources
    game_state.hit_sound = rl.loadSound("resources/hit.wav") catch undefined;
    game_state.miss_sound = game_state.hit_sound;

    prepareNextShot(&game_state);

    return game_state;
}

fn resetBall(game_state: *GameState) void {
    prepareNextShot(game_state);
}

fn fireBall(game_state: *GameState) void {
    // The real target was selected before the cue, so the ball follows the
    // warned direction only when that cue happened to be truthful.
    const delta_x = game_state.ball.target_position.x - game_state.ball.position.x;
    const delta_y = game_state.ball.target_position.y - game_state.ball.position.y;
    const distance = @sqrt(delta_x * delta_x + delta_y * delta_y);

    const displayed_speed = game_state.current_speed * game_state.settings.speed_multiplier;
    const is_slow_shot = rl.getRandomValue(1, 100) <= SLOW_SHOT_CHANCE_PERCENT;
    const launch_speed_kmh = if (is_slow_shot)
        @max(MIN_LAUNCH_SPEED, displayed_speed - SLOW_SHOT_SPEED_REDUCTION)
    else
        displayed_speed;
    const speed = launch_speed_kmh / 3.6; // Convert km/h to m/s

    if (distance > 0) {
        game_state.ball.velocity.x = (delta_x / distance) * speed;
        game_state.ball.velocity.y = (delta_y / distance) * speed;
    }

    game_state.session_stats.recordLaunch(launch_speed_kmh);
    game_state.ball.state = BallState.active;
}

fn updateGame(game_state: *GameState, dt: f32) void {
    // Decrement feedback timer
    if (game_state.feedback_timer > 0) {
        game_state.feedback_timer -= dt;
    }

    // Decrement sound cooldown
    if (game_state.sound_cooldown > 0) {
        game_state.sound_cooldown -= dt;
    }

    // Decrement countdown timer
    if (game_state.timer_countdown > 0) {
        game_state.timer_countdown -= dt;
    }

    // Decrement bounce delay timer
    if (game_state.bounce_delay_timer > 0) {
        game_state.bounce_delay_timer -= dt;
        if (game_state.bounce_delay_timer <= 0) {
            resetBall(game_state);
        }
    }

    // Update paddle position based on mouse
    const mouse_x = @as(f32, @floatFromInt(rl.getMouseX()));
    const paddle_half_width = game_state.paddle.width / 2.0;
    game_state.paddle.position.x = clampF32(
        mouse_x - paddle_half_width,
        0,
        SCREEN_WIDTH - game_state.paddle.width,
    );

    // Handle ball state
    if (game_state.ball.state == BallState.idle) {
        game_state.countdown_timer -= dt;
        if (game_state.countdown_timer <= 0) {
            fireBall(game_state);
        }
    } else if (game_state.ball.state == BallState.active) {
        // Sub-step physics for accurate drag integration
        const time_step = dt / @as(f32, @floatFromInt(PHYSICS_SUBSTEPS_PER_TICK));

        for (0..PHYSICS_SUBSTEPS_PER_TICK) |_| {
            // Apply drag with small time step
            applyDrag(&game_state.ball.velocity, time_step);

            // Update position: convert m/s to pixels/s using PIXELS_PER_METER
            game_state.ball.position.x += game_state.ball.velocity.x * PIXELS_PER_METER * time_step;
            game_state.ball.position.y += game_state.ball.velocity.y * PIXELS_PER_METER * time_step;

            // Bounce off walls
            if (game_state.ball.position.x - game_state.ball.radius < 0 or
                game_state.ball.position.x + game_state.ball.radius > SCREEN_WIDTH)
            {
                game_state.ball.velocity.x = -game_state.ball.velocity.x;
                game_state.ball.position.x = clampF32(
                    game_state.ball.position.x,
                    game_state.ball.radius,
                    SCREEN_WIDTH - game_state.ball.radius,
                );
            }

            // Bounce off top
            if (game_state.ball.position.y - game_state.ball.radius < 0) {
                game_state.ball.velocity.y = -game_state.ball.velocity.y;
                game_state.ball.position.y = game_state.ball.radius;
            }

            // Check ball-paddle collision
            const paddle_rect = rl.Rectangle{
                .x = game_state.paddle.position.x,
                .y = game_state.paddle.position.y,
                .width = game_state.paddle.width,
                .height = game_state.paddle.height,
            };

            if (rl.checkCollisionCircleRec(game_state.ball.position, game_state.ball.radius, paddle_rect)) {
                // Ball hit the paddle
                game_state.ball.velocity.y = -game_state.ball.velocity.y; // Bounce
                game_state.ball.position.y = paddle_rect.y - game_state.ball.radius; // Move above paddle
                game_state.ball.state = BallState.bouncing;
                game_state.bounce_delay_timer = BOUNCE_DELAY;
                game_state.score += 1;
                game_state.attempts += 1;
                game_state.current_speed += 1.0; // Increase speed by 1 km/h on hit
                game_state.last_feedback = FeedbackType.hit;
                game_state.feedback_timer = FEEDBACK_DISPLAY_TIME;
                game_state.feedback_text = std.fmt.bufPrintZ(
                    &game_state.feedback_text_buf,
                    "HIT +{d:.0} km/h",
                    .{game_state.current_speed * game_state.settings.speed_multiplier},
                ) catch "Error";
                rl.playSound(game_state.hit_sound);
                break; // Exit physics loop
            }

            // Check if ball went below paddle (missed)
            if (game_state.ball.position.y > SCREEN_HEIGHT) {
                game_state.ball.state = BallState.bouncing;
                game_state.bounce_delay_timer = BOUNCE_DELAY;
                game_state.attempts += 1;
                game_state.current_speed -= 2.0; // Decrease speed by 2 km/h on miss
                game_state.current_speed = @max(game_state.current_speed, 10.0); // Keep minimum speed at 10 km/h
                game_state.last_feedback = FeedbackType.miss;
                game_state.feedback_timer = FEEDBACK_DISPLAY_TIME;
                game_state.feedback_text = std.fmt.bufPrintZ(
                    &game_state.feedback_text_buf,
                    "MISS",
                    .{},
                ) catch "Error";
                rl.playSound(game_state.miss_sound);
                break; // Exit physics loop
            }
        }
    } else if (game_state.ball.state == BallState.bouncing) {
        // Sub-step physics for accurate drag integration
        const time_step = dt / @as(f32, @floatFromInt(PHYSICS_SUBSTEPS_PER_TICK));

        for (0..PHYSICS_SUBSTEPS_PER_TICK) |_| {
            // Apply drag with small time step
            applyDrag(&game_state.ball.velocity, time_step);

            // Update position: convert m/s to pixels/s
            game_state.ball.position.x += game_state.ball.velocity.x * PIXELS_PER_METER * time_step;
            game_state.ball.position.y += game_state.ball.velocity.y * PIXELS_PER_METER * time_step;

            // Bounce off walls
            if (game_state.ball.position.x - game_state.ball.radius < 0 or
                game_state.ball.position.x + game_state.ball.radius > SCREEN_WIDTH)
            {
                game_state.ball.velocity.x = -game_state.ball.velocity.x;
                game_state.ball.position.x = clampF32(
                    game_state.ball.position.x,
                    game_state.ball.radius,
                    SCREEN_WIDTH - game_state.ball.radius,
                );
            }
        }
    }
}

// ============================================================================
// RENDERING FUNCTIONS
// ============================================================================

var ui_font: ?rl.Font = null;

fn drawUiText(text_value: [:0]const u8, x: i32, y: i32, font_size: i32, color: rl.Color) void {
    if (ui_font) |font| {
        rl.drawTextEx(
            font,
            text_value,
            .{ .x = @floatFromInt(x), .y = @floatFromInt(y) },
            @floatFromInt(font_size),
            1.0,
            color,
        );
        return;
    }

    rl.drawText(text_value, x, y, font_size, color);
}

fn measureUiText(text_value: [:0]const u8, font_size: i32) i32 {
    if (ui_font) |font| {
        const dimensions = rl.measureTextEx(font, text_value, @floatFromInt(font_size), 1.0);
        return @intFromFloat(@ceil(dimensions.x));
    }

    return rl.measureText(text_value, font_size);
}

fn drawGame(game_state: *GameState) void {
    // Draw background
    rl.clearBackground(.dark_gray);

    // Draw ball with glow effect
    // Outer glow
    rl.drawCircle(
        @intFromFloat(game_state.ball.position.x),
        @intFromFloat(game_state.ball.position.y),
        game_state.ball.radius + 5.0,
        rl.Color{ .r = 255, .g = 255, .b = 255, .a = 50 },
    );

    // Main ball
    rl.drawCircle(
        @intFromFloat(game_state.ball.position.x),
        @intFromFloat(game_state.ball.position.y),
        game_state.ball.radius,
        rl.Color.white,
    );

    // Highlight
    rl.drawCircle(
        @intFromFloat(game_state.ball.position.x - game_state.ball.radius / 3.0),
        @intFromFloat(game_state.ball.position.y - game_state.ball.radius / 3.0),
        game_state.ball.radius / 3.0,
        rl.Color{ .r = 255, .g = 255, .b = 255, .a = 200 },
    );

    if (game_state.ball.state == .idle and
        game_state.countdown_timer > 0 and
        game_state.countdown_timer <= ARROW_LEAD_TIME)
    {
        drawDirectionCue(&game_state.ball);
    }

    // Draw paddle
    rl.drawRectangle(
        @intFromFloat(game_state.paddle.position.x),
        @intFromFloat(game_state.paddle.position.y),
        @intFromFloat(game_state.paddle.width),
        @intFromFloat(game_state.paddle.height),
        rl.Color.white,
    );

    // Draw paddle border for better visibility
    rl.drawRectangleLines(
        @intFromFloat(game_state.paddle.position.x),
        @intFromFloat(game_state.paddle.position.y),
        @intFromFloat(game_state.paddle.width),
        @intFromFloat(game_state.paddle.height),
        rl.Color.yellow,
    );

    // Draw score and attempts (cached, only update if changed)
    if (game_state.score != game_state.last_score or game_state.attempts != game_state.last_attempts) {
        game_state.score_text = std.fmt.bufPrintZ(
            &game_state.score_text_buf,
            "Score: {d} / Attempts: {d}",
            .{ game_state.score, game_state.attempts },
        ) catch "Error";
        game_state.last_score = game_state.score;
        game_state.last_attempts = game_state.attempts;
    }
    drawUiText(game_state.score_text, 20, 20, 20, rl.Color.white);

    // Draw feedback text
    if (game_state.feedback_timer > 0) {
        if (game_state.feedback_text.len > 0) {
            drawUiText(game_state.feedback_text, 960 - 40, 540, 40, rl.Color.yellow);
        }
    }

    // Draw UI sliders
    drawUI(game_state);

    // Draw FPS using the same readable UI font.
    var fps_buffer: [24:0]u8 = undefined;
    const fps_text = std.fmt.bufPrintZ(&fps_buffer, "{d} FPS", .{rl.getFPS()}) catch "FPS";
    drawUiText(fps_text, 20, 60, 16, rl.Color{ .r = 136, .g = 214, .b = 148, .a = 255 });

    // Draw countdown timer
    drawCountdownTimer(game_state);
}

fn drawDirectionCue(ball: *const Ball) void {
    const dx = ball.cue_target_position.x - ball.position.x;
    const dy = ball.cue_target_position.y - ball.position.y;
    const length = @sqrt(dx * dx + dy * dy);
    if (length < 0.001) return;

    const direction = rl.Vector2{ .x = dx / length, .y = dy / length };
    const perpendicular = rl.Vector2{ .x = -direction.y, .y = direction.x };
    const start = rl.Vector2{
        .x = ball.position.x + direction.x * (ball.radius + 5.0),
        .y = ball.position.y + direction.y * (ball.radius + 5.0),
    };
    const tip = rl.Vector2{
        .x = start.x + direction.x * 43.0,
        .y = start.y + direction.y * 43.0,
    };
    const head_base = rl.Vector2{
        .x = tip.x - direction.x * 12.0,
        .y = tip.y - direction.y * 12.0,
    };
    const head_left = rl.Vector2{
        .x = head_base.x + perpendicular.x * 7.5,
        .y = head_base.y + perpendicular.y * 7.5,
    };
    const head_right = rl.Vector2{
        .x = head_base.x - perpendicular.x * 7.5,
        .y = head_base.y - perpendicular.y * 7.5,
    };

    const glow = rl.Color{ .r = 255, .g = 50, .b = 72, .a = 75 };
    const red = rl.Color{ .r = 255, .g = 54, .b = 74, .a = 255 };
    rl.drawLineEx(start, head_base, 7.0, glow);
    rl.drawLineEx(start, head_base, 3.0, red);
    rl.drawCircleV(start, 3.0, red);
    rl.drawTriangle(tip, head_right, head_left, red);
}

fn drawCenteredText(text_value: [:0]const u8, center_x: f32, y: f32, font_size: i32, color: rl.Color) void {
    const text_width = measureUiText(text_value, font_size);
    drawUiText(
        text_value,
        @intFromFloat(center_x - @as(f32, @floatFromInt(text_width)) / 2.0),
        @intFromFloat(y),
        font_size,
        color,
    );
}

fn metricValue(entry: HistoryEntry, metric: ChartMetric) f32 {
    return switch (metric) {
        .average_speed => entry.average_speed_kmh,
        .max_speed => entry.max_speed_kmh,
    };
}

fn chartPoint(plot: rl.Rectangle, index: usize, count: usize, value: f32, min_value: f32, max_value: f32) rl.Vector2 {
    const x = if (count <= 1)
        plot.x + plot.width / 2.0
    else
        plot.x + (@as(f32, @floatFromInt(index)) / @as(f32, @floatFromInt(count - 1))) * plot.width;
    const normalized_y = clampF32((value - min_value) / (max_value - min_value), 0, 1);
    return .{
        .x = x,
        .y = plot.y + plot.height - normalized_y * plot.height,
    };
}

fn drawChartTooltip(entry: HistoryEntry, metric: ChartMetric, point: rl.Vector2, card: rl.Rectangle, accent: rl.Color) void {
    var text_buffer: [96:0]u8 = undefined;
    const tooltip_text = switch (metric) {
        .average_speed => std.fmt.bufPrintZ(
            &text_buffer,
            "{d:0>4}-{d:0>2}-{d:0>2}  {d:.2} km/h",
            .{ entry.date.year, entry.date.month, entry.date.day, entry.average_speed_kmh },
        ) catch "History value",
        .max_speed => std.fmt.bufPrintZ(
            &text_buffer,
            "{d:0>4}-{d:0>2}-{d:0>2}  {d:.2} km/h",
            .{ entry.date.year, entry.date.month, entry.date.day, entry.max_speed_kmh },
        ) catch "History value",
    };

    const font_size: i32 = 17;
    const tooltip_width = @as(f32, @floatFromInt(measureUiText(tooltip_text, font_size))) + 26.0;
    const tooltip_height = 38.0;
    var tooltip_x = point.x - tooltip_width / 2.0;
    tooltip_x = clampF32(tooltip_x, card.x + 8.0, card.x + card.width - tooltip_width - 8.0);
    var tooltip_y = point.y - tooltip_height - 18.0;
    if (tooltip_y < card.y + 8.0) tooltip_y = point.y + 18.0;

    const tooltip_rect = rl.Rectangle{
        .x = tooltip_x,
        .y = tooltip_y,
        .width = tooltip_width,
        .height = tooltip_height,
    };
    const shadow_rect = rl.Rectangle{
        .x = tooltip_rect.x + 4.0,
        .y = tooltip_rect.y + 5.0,
        .width = tooltip_rect.width,
        .height = tooltip_rect.height,
    };
    rl.drawRectangleRounded(shadow_rect, 0.28, 8, rl.Color{ .r = 0, .g = 0, .b = 0, .a = 90 });
    rl.drawRectangleRounded(tooltip_rect, 0.28, 8, rl.Color{ .r = 22, .g = 28, .b = 48, .a = 250 });
    rl.drawRectangleRoundedLinesEx(tooltip_rect, 0.28, 8, 1.5, accent);
    drawUiText(
        tooltip_text,
        @intFromFloat(tooltip_rect.x + 13.0),
        @intFromFloat(tooltip_rect.y + 10.0),
        font_size,
        rl.Color{ .r = 245, .g = 248, .b = 255, .a = 255 },
    );
}

fn drawHistoryChart(history: *const HistoryState, card: rl.Rectangle, metric: ChartMetric) void {
    const card_color = rl.Color{ .r = 20, .g = 27, .b = 47, .a = 238 };
    const border_color = rl.Color{ .r = 87, .g = 103, .b = 143, .a = 100 };
    const grid_color = rl.Color{ .r = 107, .g = 122, .b = 158, .a = 46 };
    const secondary_text = rl.Color{ .r = 159, .g = 171, .b = 202, .a = 255 };
    const accent = switch (metric) {
        .average_speed => rl.Color{ .r = 69, .g = 218, .b = 203, .a = 255 },
        .max_speed => rl.Color{ .r = 255, .g = 164, .b = 92, .a = 255 },
    };
    const title: [:0]const u8 = switch (metric) {
        .average_speed => "Average ball speed",
        .max_speed => "Maximum ball speed",
    };

    const shadow = rl.Rectangle{
        .x = card.x + 8.0,
        .y = card.y + 11.0,
        .width = card.width,
        .height = card.height,
    };
    rl.drawRectangleRounded(shadow, 0.035, 12, rl.Color{ .r = 0, .g = 0, .b = 0, .a = 95 });
    rl.drawRectangleRounded(card, 0.035, 12, card_color);
    rl.drawRectangleRoundedLinesEx(card, 0.035, 12, 1.2, border_color);

    const compact_header = card.width < 480.0;
    const title_size: i32 = if (compact_header) 20 else 24;
    drawUiText(title, @intFromFloat(card.x + 28.0), @intFromFloat(card.y + 24.0), title_size, rl.Color.white);

    if (history.count == 0) {
        drawCenteredText("No sessions yet", card.x + card.width / 2.0, card.y + card.height / 2.0, 21, secondary_text);
        return;
    }

    const latest = history.get(history.count - 1);
    var latest_buffer: [48:0]u8 = undefined;
    const latest_text = switch (metric) {
        .average_speed => std.fmt.bufPrintZ(&latest_buffer, "Latest  {d:.1} km/h", .{latest.average_speed_kmh}) catch "Latest",
        .max_speed => std.fmt.bufPrintZ(&latest_buffer, "Latest  {d:.1} km/h", .{latest.max_speed_kmh}) catch "Latest",
    };
    const latest_width = measureUiText(latest_text, 16);
    const latest_y_offset: f32 = if (compact_header) 55.0 else 31.0;
    drawUiText(
        latest_text,
        @intFromFloat(card.x + card.width - 28.0 - @as(f32, @floatFromInt(latest_width))),
        @intFromFloat(card.y + latest_y_offset),
        16,
        accent,
    );

    const plot_top_padding: f32 = if (compact_header) 96.0 else 76.0;
    const plot_bottom_padding: f32 = if (compact_header) 154.0 else 142.0;
    const plot = rl.Rectangle{
        .x = card.x + 67.0,
        .y = card.y + plot_top_padding,
        .width = @max(40.0, card.width - 94.0),
        .height = @max(80.0, card.height - plot_bottom_padding),
    };

    var min_value = metricValue(history.get(0), metric);
    var max_value = min_value;
    for (1..history.count) |index| {
        const value = metricValue(history.get(index), metric);
        min_value = @min(min_value, value);
        max_value = @max(max_value, value);
    }

    const spread = max_value - min_value;
    const padding = if (spread < 0.001)
        @max(10.0, @abs(max_value) * 0.1)
    else
        @max(5.0, spread * 0.15);
    min_value = @max(0.0, min_value - padding);
    max_value += padding;
    if (max_value - min_value < 1.0) max_value = min_value + 10.0;

    const grid_line_count: usize = 5;
    for (0..grid_line_count) |grid_index| {
        const ratio = @as(f32, @floatFromInt(grid_index)) / @as(f32, @floatFromInt(grid_line_count - 1));
        const y = plot.y + ratio * plot.height;
        rl.drawLineEx(.{ .x = plot.x, .y = y }, .{ .x = plot.x + plot.width, .y = y }, 1.0, grid_color);

        const label_value = max_value - ratio * (max_value - min_value);
        var y_label_buffer: [32:0]u8 = undefined;
        const y_label = switch (metric) {
            .average_speed => std.fmt.bufPrintZ(&y_label_buffer, "{d:.0}", .{label_value}) catch "-",
            .max_speed => std.fmt.bufPrintZ(&y_label_buffer, "{d:.0}", .{label_value}) catch "-",
        };
        const label_width = measureUiText(y_label, 14);
        drawUiText(
            y_label,
            @intFromFloat(plot.x - @as(f32, @floatFromInt(label_width)) - 10.0),
            @intFromFloat(y - 7.0),
            14,
            secondary_text,
        );
    }

    rl.drawLineEx(
        .{ .x = plot.x, .y = plot.y + plot.height },
        .{ .x = plot.x + plot.width, .y = plot.y + plot.height },
        1.5,
        rl.Color{ .r = 142, .g = 155, .b = 190, .a = 120 },
    );

    if (history.count > 1) {
        for (1..history.count) |index| {
            const previous_entry = history.get(index - 1);
            const entry = history.get(index);
            const previous_point = chartPoint(plot, index - 1, history.count, metricValue(previous_entry, metric), min_value, max_value);
            const point = chartPoint(plot, index, history.count, metricValue(entry, metric), min_value, max_value);
            const glow_color = rl.Color{ .r = accent.r, .g = accent.g, .b = accent.b, .a = 42 };
            rl.drawLineEx(previous_point, point, 7.0, glow_color);
            rl.drawLineEx(previous_point, point, 2.4, accent);
        }
    }

    const mouse = rl.getMousePosition();
    var hovered_index: ?usize = null;
    for (0..history.count) |index| {
        const entry = history.get(index);
        const point = chartPoint(plot, index, history.count, metricValue(entry, metric), min_value, max_value);
        const dx = mouse.x - point.x;
        const dy = mouse.y - point.y;
        if (dx * dx + dy * dy <= 13.0 * 13.0) hovered_index = index;

        rl.drawCircleV(point, 8.0, rl.Color{ .r = accent.r, .g = accent.g, .b = accent.b, .a = 45 });
        rl.drawCircleV(point, 5.0, accent);
        rl.drawCircleV(point, 1.7, rl.Color.white);
    }

    const x_label_count = @min(history.count, @as(usize, 4));
    for (0..x_label_count) |label_index| {
        const history_index = if (x_label_count <= 1)
            0
        else
            (label_index * (history.count - 1)) / (x_label_count - 1);
        const entry = history.get(history_index);
        const point_x = chartPoint(plot, history_index, history.count, metricValue(entry, metric), min_value, max_value).x;
        var x_label_buffer: [32:0]u8 = undefined;
        const month_index: usize = @intCast(entry.date.month - 1);
        const x_label = std.fmt.bufPrintZ(
            &x_label_buffer,
            "{s} {d}",
            .{ MONTH_NAMES[month_index], entry.date.year },
        ) catch "Date";
        const label_width = measureUiText(x_label, 14);
        const label_x = clampF32(
            point_x - @as(f32, @floatFromInt(label_width)) / 2.0,
            card.x + 6.0,
            card.x + card.width - @as(f32, @floatFromInt(label_width)) - 6.0,
        );
        drawUiText(x_label, @intFromFloat(label_x), @intFromFloat(plot.y + plot.height + 18.0), 14, secondary_text);
    }

    if (hovered_index) |index| {
        const entry = history.get(index);
        const point = chartPoint(plot, index, history.count, metricValue(entry, metric), min_value, max_value);
        rl.drawCircleV(point, 11.0, rl.Color{ .r = accent.r, .g = accent.g, .b = accent.b, .a = 58 });
        rl.drawCircleV(point, 6.0, accent);
        drawChartTooltip(entry, metric, point, card, accent);
    }
}

fn drawHistoryScreen(history: *const HistoryState) void {
    const screen_width_i = rl.getScreenWidth();
    const screen_height_i = rl.getScreenHeight();
    const screen_width = @as(f32, @floatFromInt(screen_width_i));
    const screen_height = @as(f32, @floatFromInt(screen_height_i));

    rl.drawRectangleGradientV(
        0,
        0,
        screen_width_i,
        screen_height_i,
        rl.Color{ .r = 16, .g = 23, .b = 43, .a = 255 },
        rl.Color{ .r = 7, .g = 11, .b = 24, .a = 255 },
    );
    rl.drawCircle(@intFromFloat(screen_width * 0.08), @intFromFloat(screen_height * 0.12), screen_width * 0.16, rl.Color{ .r = 47, .g = 203, .b = 190, .a = 16 });
    rl.drawCircle(@intFromFloat(screen_width * 0.93), @intFromFloat(screen_height * 0.88), screen_width * 0.18, rl.Color{ .r = 255, .g = 88, .b = 148, .a = 13 });

    drawCenteredText("Session history", screen_width / 2.0, 28.0, 36, rl.Color{ .r = 245, .g = 248, .b = 255, .a = 255 });
    drawCenteredText("Your reaction training over time", screen_width / 2.0, 72.0, 18, rl.Color{ .r = 159, .g = 171, .b = 202, .a = 255 });

    var summary_buffer: [128:0]u8 = undefined;
    const summary_text = std.fmt.bufPrintZ(
        &summary_buffer,
        "This session   {d:.1} km/h average   |   {d:.1} km/h maximum",
        .{ history.current_session.average_speed_kmh, history.current_session.max_speed_kmh },
    ) catch "This session";
    const summary_width = @as(f32, @floatFromInt(measureUiText(summary_text, 17))) + 34.0;
    const summary_rect = rl.Rectangle{
        .x = screen_width / 2.0 - summary_width / 2.0,
        .y = 105.0,
        .width = summary_width,
        .height = 38.0,
    };
    rl.drawRectangleRounded(summary_rect, 0.4, 10, rl.Color{ .r = 34, .g = 44, .b = 70, .a = 225 });
    rl.drawRectangleRoundedLinesEx(summary_rect, 0.4, 10, 1.0, rl.Color{ .r = 104, .g = 121, .b = 163, .a = 90 });
    drawCenteredText(summary_text, screen_width / 2.0, 115.0, 17, rl.Color{ .r = 225, .g = 232, .b = 247, .a = 255 });

    const speed_control_rect = rl.Rectangle{
        .x = NEXT_SPEED_SLIDER_X - 24.0,
        .y = 151.0,
        .width = NEXT_SPEED_SLIDER_WIDTH + 48.0,
        .height = 79.0,
    };
    rl.drawRectangleRounded(speed_control_rect, 0.24, 10, rl.Color{ .r = 24, .g = 33, .b = 55, .a = 225 });
    rl.drawRectangleRoundedLinesEx(speed_control_rect, 0.24, 10, 1.0, rl.Color{ .r = 81, .g = 205, .b = 193, .a = 90 });
    drawUiText(
        "Next session starting speed",
        @intFromFloat(NEXT_SPEED_SLIDER_X),
        162,
        17,
        rl.Color{ .r = 220, .g = 229, .b = 246, .a = 255 },
    );

    var next_speed_buffer: [32:0]u8 = undefined;
    const next_speed_text = std.fmt.bufPrintZ(
        &next_speed_buffer,
        "{d:.0} km/h",
        .{history.next_start_speed_kmh},
    ) catch "Speed";
    const next_speed_width = measureUiText(next_speed_text, 17);
    drawUiText(
        next_speed_text,
        @as(i32, @intFromFloat(NEXT_SPEED_SLIDER_X + NEXT_SPEED_SLIDER_WIDTH)) - next_speed_width,
        162,
        17,
        rl.Color{ .r = 75, .g = 224, .b = 207, .a = 255 },
    );
    drawSlider(
        NEXT_SPEED_SLIDER_X,
        NEXT_SPEED_SLIDER_Y,
        NEXT_SPEED_SLIDER_WIDTH,
        history.next_start_speed_kmh,
        STARTING_SPEED_MIN,
        STARTING_SPEED_MAX,
    );
    drawUiText("250", @intFromFloat(NEXT_SPEED_SLIDER_X), 207, 12, rl.Color{ .r = 137, .g = 151, .b = 182, .a = 255 });
    const max_label_width = measureUiText("1500", 12);
    drawUiText(
        "1500",
        @as(i32, @intFromFloat(NEXT_SPEED_SLIDER_X + NEXT_SPEED_SLIDER_WIDTH)) - max_label_width,
        207,
        12,
        rl.Color{ .r = 137, .g = 151, .b = 182, .a = 255 },
    );

    const minimum_margin = clampF32(screen_width * 0.04, 28.0, 72.0);
    const chart_gap = clampF32(screen_width * 0.02, 20.0, 38.0);
    const earliest_chart_top = clampF32(screen_height * 0.23, 246.0, 270.0);
    const bottom_margin = clampF32(screen_height * 0.075, 58.0, 82.0);
    const chart_group_width = @min(1800.0, screen_width - minimum_margin * 2.0);
    const chart_left = (screen_width - chart_group_width) / 2.0;
    const chart_width = (chart_group_width - chart_gap) / 2.0;
    const available_chart_height = screen_height - earliest_chart_top - bottom_margin;
    const chart_height = @min(820.0, @max(260.0, available_chart_height));
    const chart_top = earliest_chart_top + @max(0.0, (available_chart_height - chart_height) / 2.0);

    const speed_card = rl.Rectangle{
        .x = chart_left,
        .y = chart_top,
        .width = chart_width,
        .height = chart_height,
    };
    const max_speed_card = rl.Rectangle{
        .x = chart_left + chart_width + chart_gap,
        .y = chart_top,
        .width = chart_width,
        .height = chart_height,
    };
    drawHistoryChart(history, speed_card, .average_speed);
    drawHistoryChart(history, max_speed_card, .max_speed);

    const save_text: [:0]const u8 = if (!history.record_eligible)
        "Session did not exceed 5 minutes - not added to history.csv"
    else if (history.save_succeeded)
        "Saved to history.csv"
    else
        "Could not save history.csv - showing this session in memory";
    const save_color = if (!history.record_eligible)
        rl.Color{ .r = 244, .g = 190, .b = 92, .a = 255 }
    else if (history.save_succeeded)
        rl.Color{ .r = 104, .g = 218, .b = 169, .a = 255 }
    else
        rl.Color{ .r = 255, .g = 139, .b = 139, .a = 255 };
    drawUiText(save_text, @intFromFloat(chart_left), screen_height_i - 35, 15, save_color);

    const exit_text: [:0]const u8 = "Press Esc again to close";
    const exit_width = measureUiText(exit_text, 16);
    drawUiText(exit_text, screen_width_i - exit_width - @as(i32, @intFromFloat(chart_left)), screen_height_i - 36, 16, rl.Color{ .r = 174, .g = 185, .b = 211, .a = 255 });
}

fn updateUIInput(game_state: *GameState) void {
    updateSliderInput(
        SPEED_SLIDER_X,
        SPEED_SLIDER_Y + 25,
        SPEED_SLIDER_WIDTH,
        &game_state.settings.speed_multiplier,
        0.5,
        3.0,
    );

    const paddle_y = SPEED_SLIDER_Y + 70;
    updateSliderInput(
        SPEED_SLIDER_X,
        paddle_y + 25,
        SPEED_SLIDER_WIDTH,
        &game_state.settings.paddle_width,
        50.0,
        300.0,
    );
    game_state.paddle.width = game_state.settings.paddle_width;
}

fn updateHistoryInput(history: *HistoryState) void {
    updateSliderInput(
        NEXT_SPEED_SLIDER_X,
        NEXT_SPEED_SLIDER_Y,
        NEXT_SPEED_SLIDER_WIDTH,
        &history.next_start_speed_kmh,
        STARTING_SPEED_MIN,
        STARTING_SPEED_MAX,
    );
}

fn updateSliderInput(x: f32, y: f32, width: f32, value: *f32, min: f32, max: f32) void {
    const range = max - min;
    const mouse_pos = rl.getMousePosition();
    const hitbox = rl.Rectangle{ .x = x - 10.0, .y = y - 14.0, .width = width + 20.0, .height = 28.0 };

    if (rl.isMouseButtonDown(.left) and rl.checkCollisionPointRec(mouse_pos, hitbox)) {
        const new_normalized = clampF32((mouse_pos.x - x) / width, 0, 1);
        value.* = min + new_normalized * range;
    }
}

fn drawUI(game_state: *GameState) void {
    // Draw speed label and slider
    drawUiText("Speed (km/h):", @intFromFloat(SPEED_SLIDER_X), @intFromFloat(SPEED_SLIDER_Y), 16, rl.Color.white);
    drawSlider(
        SPEED_SLIDER_X,
        SPEED_SLIDER_Y + 25,
        SPEED_SLIDER_WIDTH,
        game_state.settings.speed_multiplier,
        0.5,
        3.0,
    );

    // Display effective ball speed in km/h (cached, only update if changed)
    const effective_speed: f32 = game_state.current_speed * game_state.settings.speed_multiplier;
    const effective_speed_int: i32 = @intFromFloat(effective_speed);
    if (effective_speed_int != game_state.last_current_speed) {
        game_state.speed_text = std.fmt.bufPrintZ(&game_state.speed_text_buf, "{d:.0} km/h", .{effective_speed}) catch "Error";
        game_state.last_current_speed = effective_speed_int;
    }
    drawUiText(game_state.speed_text, @intFromFloat(SPEED_SLIDER_X + SPEED_SLIDER_WIDTH + 15), @intFromFloat(SPEED_SLIDER_Y + 25), 14, rl.Color.yellow);

    // Draw paddle size label and slider
    const paddle_y = SPEED_SLIDER_Y + 70;
    drawUiText("Paddle Size:", @intFromFloat(SPEED_SLIDER_X), @intFromFloat(paddle_y), 16, rl.Color.white);
    drawSlider(
        SPEED_SLIDER_X,
        paddle_y + 25,
        SPEED_SLIDER_WIDTH,
        game_state.settings.paddle_width,
        50.0,
        300.0,
    );

    // Display paddle size value (cached, only update if changed)
    const paddle_width_int: i32 = @intFromFloat(game_state.settings.paddle_width);
    if (paddle_width_int != game_state.last_paddle_width) {
        game_state.paddle_text = std.fmt.bufPrintZ(&game_state.paddle_text_buf, "{d:.0}px", .{game_state.settings.paddle_width}) catch "Error";
        game_state.last_paddle_width = paddle_width_int;
    }
    drawUiText(game_state.paddle_text, @intFromFloat(SPEED_SLIDER_X + SPEED_SLIDER_WIDTH + 15), @intFromFloat(paddle_y + 25), 14, rl.Color.yellow);
}

fn drawCountdownTimer(game_state: *GameState) void {
    const total_seconds = @as(i32, @intFromFloat(game_state.timer_countdown));

    // Update text only when seconds change
    if (total_seconds != game_state.last_timer_seconds) {
        const minutes = @divTrunc(total_seconds, 60);
        const seconds = @mod(total_seconds, 60);
        game_state.timer_text = std.fmt.bufPrintZ(
            &game_state.timer_text_buf,
            "{d}:{d:0>2}",
            .{ minutes, seconds },
        ) catch "Error";
        game_state.last_timer_seconds = total_seconds;
    }

    // Determine color based on timer state
    const timer_color: rl.Color = if (game_state.timer_countdown <= 0)
        rl.Color.red // Red when finished
    else if (game_state.timer_countdown <= 30)
        rl.Color.orange // Orange warning (last 30 seconds)
    else
        rl.Color.white; // White while counting

    // Draw timer slightly below FPS counter
    drawUiText(game_state.timer_text, 20, 85, 20, timer_color);
}

fn drawSlider(x: f32, y: f32, width: f32, value: f32, min: f32, max: f32) void {
    const range = max - min;
    const normalized = clampF32((value - min) / range, 0, 1);
    const knob_x = x + normalized * width;

    // A thicker track and filled section stay clear at high resolutions.
    rl.drawLineEx(
        rl.Vector2{ .x = x, .y = y },
        rl.Vector2{ .x = x + width, .y = y },
        6.0,
        rl.Color{ .r = 75, .g = 86, .b = 111, .a = 220 },
    );
    rl.drawLineEx(
        rl.Vector2{ .x = x, .y = y },
        rl.Vector2{ .x = knob_x, .y = y },
        6.0,
        rl.Color{ .r = 75, .g = 224, .b = 207, .a = 255 },
    );
    rl.drawCircle(@intFromFloat(knob_x), @intFromFloat(y), 11, rl.Color{ .r = 75, .g = 224, .b = 207, .a = 45 });
    rl.drawCircle(@intFromFloat(knob_x), @intFromFloat(y), 7, rl.Color.white);
    rl.drawCircle(@intFromFloat(knob_x), @intFromFloat(y), 3, rl.Color{ .r = 35, .g = 166, .b = 157, .a = 255 });
}

var SCREEN_WIDTH: f32 = undefined;
var SCREEN_HEIGHT: f32 = undefined;
var BALL_START_X: f32 = undefined;
var BALL_START_Y: f32 = undefined;
var PADDLE_HEIGHT: f32 = undefined;
var PADDLE_Y: f32 = undefined;
var PIXELS_PER_METER: f32 = undefined;
var METERS_PER_PIXEL: f32 = undefined;
var LAUNCH_RECT_WIDTH: f32 = undefined;
var LAUNCH_RECT_HEIGHT: f32 = undefined;
var LAUNCH_RECT_X: f32 = undefined;
var LAUNCH_RECT_Y: f32 = undefined;
var SPEED_SLIDER_X: f32 = undefined;
var SPEED_SLIDER_Y: f32 = undefined;
var SPEED_SLIDER_WIDTH: f32 = undefined;
var NEXT_SPEED_SLIDER_X: f32 = undefined;
var NEXT_SPEED_SLIDER_Y: f32 = undefined;
var NEXT_SPEED_SLIDER_WIDTH: f32 = undefined;

fn defineConstants() void {
    SCREEN_WIDTH = @floatFromInt(rl.getScreenWidth());
    SCREEN_HEIGHT = @floatFromInt(rl.getScreenHeight());

    BALL_START_X = SCREEN_WIDTH / 2.0;
    BALL_START_Y = 100.0;
    PADDLE_HEIGHT = 20.0;
    PADDLE_Y = SCREEN_HEIGHT - 60.0;
    PIXELS_PER_METER = (SCREEN_HEIGHT - BALL_START_Y) / 10.0; // ~98 pixels per meter
    METERS_PER_PIXEL = 10.0 / (SCREEN_HEIGHT - BALL_START_Y); // ~0.0102 meters per pixel
    LAUNCH_RECT_WIDTH = SCREEN_WIDTH / 4.0; // Launch rectangle (1/4 screen width, 40 pixels tall, centered at top with 10px margin)
    LAUNCH_RECT_HEIGHT = 40.0;
    LAUNCH_RECT_X = SCREEN_WIDTH / 2.0 - LAUNCH_RECT_WIDTH / 2.0;
    LAUNCH_RECT_Y = 10.0;
    // UI positions
    SPEED_SLIDER_X = SCREEN_WIDTH - 300.0; // Right side
    SPEED_SLIDER_Y = 20.0;
    SPEED_SLIDER_WIDTH = 200.0;
    NEXT_SPEED_SLIDER_WIDTH = clampF32(SCREEN_WIDTH * 0.2, 300.0, 420.0);
    NEXT_SPEED_SLIDER_X = SCREEN_WIDTH / 2.0 - NEXT_SPEED_SLIDER_WIDTH / 2.0;
    NEXT_SPEED_SLIDER_Y = 198.0;
}

// ============================================================================
// MAIN
// ============================================================================

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    rl.setConfigFlags(.{ .vsync_hint = true }); // Enable vsync

    rl.initWindow(1, 1, "Reaction Ball Game");
    defer rl.closeWindow();
    rl.setExitKey(.null); // Esc is a two-step save/history/exit flow.
    rl.toggleFullscreen();

    defineConstants();

    ui_font = rl.loadFontEx("resources/AnonymousPro-Bold.ttf", 64, null) catch null;
    if (ui_font) |font| rl.setTextureFilter(font.texture, .bilinear);
    defer {
        if (ui_font) |font| rl.unloadFont(font);
    }

    // Initialize audio device for sound playback
    rl.initAudioDevice();
    defer rl.closeAudioDevice();

    // rl.setTargetFPS(0); // Enables vsync, syncs to monitor refresh rate

    const starting_speed_kmh = loadStartingSpeed(io);
    var game_state = initGame(starting_speed_kmh);
    defer {
        rl.unloadSound(game_state.hit_sound);
    }

    var history_state: HistoryState = .{};
    history_state.next_start_speed_kmh = starting_speed_kmh;
    var simulation_clock: FixedStepClock = .{};
    var current_screen: AppScreen = .game;
    var should_exit = false;

    // Main game loop
    while (!rl.windowShouldClose() and !should_exit) {
        const frame_dt = rl.getFrameTime();
        if (current_screen == .game) game_state.session_elapsed += frame_dt;

        if (rl.isKeyPressed(.escape)) {
            switch (current_screen) {
                .game => {
                    finalizeSession(io, &game_state, &history_state);
                    current_screen = .history;
                },
                .history => should_exit = true,
            }
        }
        if (should_exit) break;

        switch (current_screen) {
            .game => {
                updateUIInput(&game_state);
                simulation_clock.pushFrame(frame_dt);

                var fixed_updates: usize = 0;
                while (fixed_updates < MAX_FIXED_UPDATES_PER_FRAME and simulation_clock.takeStep()) : (fixed_updates += 1) {
                    updateGame(&game_state, FIXED_SIMULATION_DT);
                }
            },
            .history => updateHistoryInput(&history_state),
        }

        rl.beginDrawing();
        switch (current_screen) {
            .game => drawGame(&game_state),
            .history => drawHistoryScreen(&history_state),
        }
        rl.endDrawing();
    }

    // Preserve the session if the window-close control is used instead of Esc.
    if (!history_state.session_finalized) finalizeSession(io, &game_state, &history_state);
    saveStartingSpeed(io, history_state.next_start_speed_kmh) catch {};
}

test "session snapshot tracks average and maximum launch speeds" {
    var stats: SessionStats = .{};
    stats.recordLaunch(500);
    stats.recordLaunch(400);
    stats.recordLaunch(650);

    const result = stats.snapshot();
    try std.testing.expectApproxEqAbs(@as(f32, 516.666_7), result.average_speed_kmh, 0.001);
    try std.testing.expectApproxEqAbs(@as(f32, 650), result.max_speed_kmh, 0.001);
}

test "civil date conversion handles epoch boundaries" {
    try std.testing.expectEqual(Date{ .year = 1970, .month = 1, .day = 1 }, dateFromUnixTimestamp(0));
    try std.testing.expectEqual(Date{ .year = 1970, .month = 1, .day = 2 }, dateFromUnixTimestamp(86_400));
    try std.testing.expectEqual(Date{ .year = 1969, .month = 12, .day = 31 }, dateFromUnixTimestamp(-1));
}

test "history dates require the CSV date shape" {
    try std.testing.expectEqual(Date{ .year = 2026, .month = 7, .day = 13 }, parseDate("2026-07-13").?);
    try std.testing.expect(parseDate("2026/07/13") == null);
    try std.testing.expect(parseDate("2026-13-01") == null);
}

test "current session date is a valid calendar date" {
    const date = currentDate(std.testing.io);
    try std.testing.expect(date.year >= 1970);
    try std.testing.expect(date.month >= 1 and date.month <= 12);
    try std.testing.expect(date.day >= 1 and date.day <= 31);
}

test "only sessions longer than five minutes are recorded" {
    try std.testing.expect(!shouldRecordSession(299.999));
    try std.testing.expect(!shouldRecordSession(300.0));
    try std.testing.expect(shouldRecordSession(300.001));
}

test "persisted starting speed is finite and stays inside the slider range" {
    try std.testing.expectEqual(@as(f32, 250), normalizeStartingSpeed(100));
    try std.testing.expectEqual(@as(f32, 875), normalizeStartingSpeed(875));
    try std.testing.expectEqual(@as(f32, 1500), normalizeStartingSpeed(2000));
    try std.testing.expectEqual(BASE_BALL_SPEED, normalizeStartingSpeed(std.math.nan(f32)));
    try std.testing.expectApproxEqAbs(@as(f32, 1.5), startingSpeedMultiplier(750), 0.0001);
}

test "fixed simulation clock is independent of renderer rate" {
    var clock_at_60_fps: FixedStepClock = .{};
    var steps_at_60_fps: usize = 0;
    for (0..10) |_| {
        clock_at_60_fps.pushFrame(1.0 / 60.0);
        while (clock_at_60_fps.takeStep()) steps_at_60_fps += 1;
    }

    var clock_at_120_fps: FixedStepClock = .{};
    var steps_at_120_fps: usize = 0;
    for (0..20) |_| {
        clock_at_120_fps.pushFrame(1.0 / 120.0);
        while (clock_at_120_fps.takeStep()) steps_at_120_fps += 1;
    }

    try std.testing.expectEqual(@as(usize, 40), steps_at_60_fps);
    try std.testing.expectEqual(steps_at_60_fps, steps_at_120_fps);
}
