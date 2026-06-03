// raylib-zig Reaction Ball Game
// A breakout-style game where a ball fires down, and the player controls a paddle

const std = @import("std");
const rl = @import("raylib");

// ============================================================================
// GAME STATE STRUCTURES
// ============================================================================

const BallState = enum { idle, firing, active, bouncing };
const FeedbackType = enum { none, hit, miss };

const Ball = struct {
    position: rl.Vector2,
    velocity: rl.Vector2,
    radius: f32,
    state: BallState,
    target_position: rl.Vector2, // Target position when firing
};

const Paddle = struct {
    position: rl.Vector2, // Bottom-left corner
    width: f32,
    height: f32,
};

const GameSettings = struct {
    speed_multiplier: f32, // 1.0 - 10.0
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
    // Countdown timer (3 minutes)
    timer_countdown: f32 = 180.0,
    timer_text_buf: [32:0]u8 = undefined,
    timer_text: [:0]const u8 = "",
    last_timer_seconds: i32 = 180,
};

// ============================================================================
// CONSTANTS
// ============================================================================

const BALL_RADIUS = 15.0;

const FIRE_DELAY = 0.3; // Before ball fires

// Physics constants
// Distance from ball start to bottom = 10 meters
// Pixel distance: SCREEN_HEIGHT - BALL_START_Y = 980 pixels = 10 meters
const BASE_BALL_SPEED = 450.0; // km/h
const DRAG_COEFFICIENT = 0.45; // Badminton shuttlecock with feathered skirt
const AIR_DENSITY = 1.225; // kg/m^3 at sea level
const SHUTTLECOCK_MASS = 0.005; // kg (approximately 5 grams)
const SHUTTLECOCK_AREA = 0.0034212; // m^2 (cross-sectional area including feathered skirt)
const DRAG_CONSTANT = (AIR_DENSITY * DRAG_COEFFICIENT * SHUTTLECOCK_AREA) / (2.0 * SHUTTLECOCK_MASS);

const FEEDBACK_DISPLAY_TIME = 0.4; // How long to show "Hit"/"Miss" text
const BOUNCE_DELAY = 0.3; // Delay before resetting after collision or miss

// Countdown timer
const COUNTDOWN_DURATION = 180.0; // 3 minutes in seconds

// Physics sub-stepping
const PHYSICS_TIME_RATIO = 30; // Apply physics 30x per frame for accurate drag integration

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

// ============================================================================
// GAME INITIALIZATION & LOGIC
// ============================================================================

fn initGame() GameState {
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
    };

    // Initialize paddle
    game_state.paddle = Paddle{
        .position = rl.Vector2{ .x = SCREEN_WIDTH / 2.0, .y = PADDLE_Y },
        .width = 200.0,
        .height = PADDLE_HEIGHT,
    };

    // Initialize settings with defaults
    game_state.settings = GameSettings{
        .speed_multiplier = 1.0,
        .paddle_width = 200.0,
        .paddle_height = PADDLE_HEIGHT,
    };

    // Initialize game state
    game_state.score = 0;
    game_state.attempts = 0;
    game_state.current_speed = BASE_BALL_SPEED; // Start at base speed
    game_state.countdown_timer = FIRE_DELAY;
    game_state.bounce_delay_timer = 0;
    game_state.last_feedback = FeedbackType.none;
    game_state.feedback_timer = 0;
    game_state.sound_cooldown = 0;
    // Initialize cached text on first frame
    game_state.last_score = 0;
    game_state.last_attempts = 0;
    game_state.last_current_speed = 0;
    game_state.last_paddle_width = 0;
    game_state.timer_countdown = COUNTDOWN_DURATION;
    game_state.last_timer_seconds = @intFromFloat(COUNTDOWN_DURATION);
    game_state.timer_text = std.fmt.bufPrintZ(&game_state.timer_text_buf, "3:00", .{}) catch "Error";
    game_state.score_text = std.fmt.bufPrintZ(&game_state.score_text_buf, "Score: {d} / Attempts: {d}", .{ 0, 0 }) catch "Error";
    game_state.speed_text = std.fmt.bufPrintZ(&game_state.speed_text_buf, "{d:.0} km/h", .{BASE_BALL_SPEED * game_state.settings.speed_multiplier}) catch "Error";
    game_state.paddle_text = std.fmt.bufPrintZ(&game_state.paddle_text_buf, "{d:.0}px", .{200.0}) catch "Error";

    // Load sound effects from resources
    game_state.hit_sound = rl.loadSound("resources/hit.wav") catch undefined;
    game_state.miss_sound = game_state.hit_sound;

    return game_state;
}

fn resetBall(game_state: *GameState) void {
    const launch_x = randomF32(LAUNCH_RECT_X, LAUNCH_RECT_X + LAUNCH_RECT_WIDTH);
    const launch_y = randomF32(LAUNCH_RECT_Y, LAUNCH_RECT_Y + LAUNCH_RECT_HEIGHT);
    game_state.ball.position = rl.Vector2{ .x = launch_x, .y = launch_y };
    game_state.ball.velocity = rl.Vector2{ .x = 0, .y = 0 };
    game_state.ball.state = BallState.idle;
    game_state.countdown_timer = FIRE_DELAY;
    game_state.bounce_delay_timer = 0;
    game_state.attempts += 1;
}

fn fireBall(game_state: *GameState) void {
    // Choose random X position at the bottom - allow close to walls but not touching
    const min_x = BALL_RADIUS;
    const max_x = SCREEN_WIDTH - BALL_RADIUS;
    const target_x = randomF32(min_x, max_x);
    game_state.ball.target_position = rl.Vector2{
        .x = target_x,
        .y = SCREEN_HEIGHT - 100.0,
    };

    // Calculate velocity vector from current position to target
    const delta_x = game_state.ball.target_position.x - game_state.ball.position.x;
    const delta_y = game_state.ball.target_position.y - game_state.ball.position.y;
    const distance = @sqrt(delta_x * delta_x + delta_y * delta_y);

    const speed = (game_state.current_speed / 3.6) * game_state.settings.speed_multiplier; // Convert km/h to m/s
    if (distance > 0) {
        game_state.ball.velocity.x = (delta_x / distance) * speed;
        game_state.ball.velocity.y = (delta_y / distance) * speed;
    }

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
        const time_step = dt / @as(f32, @floatFromInt(PHYSICS_TIME_RATIO));

        for (0..PHYSICS_TIME_RATIO) |_| {
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
                game_state.current_speed += 1.0; // Increase speed by 1 km/h on hit
                game_state.last_feedback = FeedbackType.hit;
                game_state.feedback_timer = FEEDBACK_DISPLAY_TIME;
                rl.playSound(game_state.hit_sound);
                break; // Exit physics loop
            }

            // Check if ball went below paddle (missed)
            if (game_state.ball.position.y > SCREEN_HEIGHT) {
                game_state.ball.state = BallState.bouncing;
                game_state.bounce_delay_timer = BOUNCE_DELAY;
                game_state.current_speed -= 2.0; // Decrease speed by 2 km/h on miss
                game_state.current_speed = @max(game_state.current_speed, 10.0); // Keep minimum speed at 10 km/h
                game_state.last_feedback = FeedbackType.miss;
                game_state.feedback_timer = FEEDBACK_DISPLAY_TIME;
                rl.playSound(game_state.miss_sound);
                break; // Exit physics loop
            }
        }
    } else if (game_state.ball.state == BallState.bouncing) {
        // Sub-step physics for accurate drag integration
        const time_step = dt / @as(f32, @floatFromInt(PHYSICS_TIME_RATIO));

        for (0..PHYSICS_TIME_RATIO) |_| {
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
    rl.drawText(game_state.score_text, 20, 20, 20, rl.Color.white);

    // Draw feedback text
    if (game_state.feedback_timer > 0) {
        const feedback_text: [:0]const u8 = switch (game_state.last_feedback) {
            FeedbackType.hit => "HIT +1",
            FeedbackType.miss => "MISS",
            FeedbackType.none => "",
        };
        if (feedback_text.len > 0) {
            rl.drawText(feedback_text, 960 - 40, 540, 40, rl.Color.yellow);
        }
    }

    // Draw UI sliders
    drawUI(game_state);

    // Draw FPS
    rl.drawFPS(20, 60);

    // Draw countdown timer
    drawCountdownTimer(game_state);
}

fn drawUI(game_state: *GameState) void {
    // Draw speed label and slider
    rl.drawText("Speed (km/h):", @intFromFloat(SPEED_SLIDER_X), @intFromFloat(SPEED_SLIDER_Y), 16, rl.Color.white);
    drawSlider(
        SPEED_SLIDER_X,
        SPEED_SLIDER_Y + 25,
        SPEED_SLIDER_WIDTH,
        &game_state.settings.speed_multiplier,
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
    rl.drawText(game_state.speed_text, @intFromFloat(SPEED_SLIDER_X + SPEED_SLIDER_WIDTH + 15), @intFromFloat(SPEED_SLIDER_Y + 25), 14, rl.Color.yellow);

    // Draw paddle size label and slider
    const paddle_y = SPEED_SLIDER_Y + 70;
    rl.drawText("Paddle Size:", @intFromFloat(SPEED_SLIDER_X), @intFromFloat(paddle_y), 16, rl.Color.white);
    drawSlider(
        SPEED_SLIDER_X,
        paddle_y + 25,
        SPEED_SLIDER_WIDTH,
        &game_state.settings.paddle_width,
        50.0,
        300.0,
    );
    game_state.paddle.width = game_state.settings.paddle_width;

    // Display paddle size value (cached, only update if changed)
    const paddle_width_int: i32 = @intFromFloat(game_state.settings.paddle_width);
    if (paddle_width_int != game_state.last_paddle_width) {
        game_state.paddle_text = std.fmt.bufPrintZ(&game_state.paddle_text_buf, "{d:.0}px", .{game_state.settings.paddle_width}) catch "Error";
        game_state.last_paddle_width = paddle_width_int;
    }
    rl.drawText(game_state.paddle_text, @intFromFloat(SPEED_SLIDER_X + SPEED_SLIDER_WIDTH + 15), @intFromFloat(paddle_y + 25), 14, rl.Color.yellow);
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
    rl.drawText(game_state.timer_text, 20, 85, 20, timer_color);
}

fn drawSlider(x: f32, y: f32, width: f32, value: *f32, min: f32, max: f32) void {
    // Draw slider background
    rl.drawLineEx(
        rl.Vector2{ .x = x, .y = y },
        rl.Vector2{ .x = x + width, .y = y },
        2.0,
        rl.Color.gray,
    );

    // Calculate knob position
    const range = max - min;
    const normalized = (value.* - min) / range;
    const knob_x = x + normalized * width;

    // Draw knob
    rl.drawCircle(@intFromFloat(knob_x), @intFromFloat(y), 6, rl.Color.white);

    // Check for mouse interaction
    const mouse_pos = rl.Vector2{
        .x = @floatFromInt(rl.getMouseX()),
        .y = @floatFromInt(rl.getMouseY()),
    };
    const mouse_distance_sq = (mouse_pos.x - knob_x) * (mouse_pos.x - knob_x) +
        (mouse_pos.y - y) * (mouse_pos.y - y);

    if (mouse_distance_sq < 100 and rl.isMouseButtonDown(.left)) {
        // Dragging knob
        const new_normalized = clampF32((mouse_pos.x - x) / width, 0, 1);
        value.* = min + new_normalized * range;
    }
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
}

// ============================================================================
// MAIN
// ============================================================================

pub fn main() !void {
    rl.setConfigFlags(.{ .vsync_hint = true }); // Enable vsync

    rl.initWindow(1, 1, "Reaction Ball Game");
    defer rl.closeWindow();
    rl.toggleFullscreen();

    defineConstants();

    // Initialize audio device for sound playback
    rl.initAudioDevice();
    defer rl.closeAudioDevice();

    // rl.setTargetFPS(0); // Enables vsync, syncs to monitor refresh rate

    var game_state = initGame();
    defer {
        rl.unloadSound(game_state.hit_sound);
    }

    // Main game loop
    while (!rl.windowShouldClose()) {
        const dt = rl.getFrameTime();

        // Update
        updateGame(&game_state, dt);

        // Draw
        rl.beginDrawing();
        defer rl.endDrawing();

        drawGame(&game_state);
    }
}
