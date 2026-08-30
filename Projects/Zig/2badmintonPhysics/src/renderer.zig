const std = @import("std");
const rl = @import("raylib");
const sim = @import("simulation.zig");

const background = color(8, 11, 18, 255);
const panel_background = color(13, 18, 27, 255);
const plot_background = color(8, 12, 18, 255);
const panel_border = color(45, 56, 70, 255);
const muted = color(139, 151, 164, 255);
const grid = color(47, 59, 72, 150);
const trajectory = color(255, 177, 61, 255);
const net_white = color(245, 247, 250, 255);
const axis_magenta = color(255, 72, 178, 255);
const plot_x_extent: f32 = 12.0;
const plot_z_max: f32 = 7.5;
const velocity_time_max_ms: f32 = 5000.0;
const velocity_max: f32 = 90.0;

pub const SpeedMode = enum(u2) {
    stopped,
    x001,
    x01,
    x1,
};

pub const Renderer = struct {
    main_target: rl.RenderTexture2D,
    close_target: rl.RenderTexture2D,
    net_texture: rl.Texture2D,
    main_camera: rl.Camera3D,
    close_camera: rl.Camera3D,
    screen_width: i32,
    screen_height: i32,
    main_height: i32,
    panel_height: i32,
    panel_width: i32,
    close_width: i32,
    speed_mode: SpeedMode = .x1,
    main_dragging: bool = false,
    main_last_mouse: rl.Vector2 = .{ .x = 0, .y = 0 },
    main_focus: sim.Vec3 = .{ .z = 1.05 },
    main_yaw: f32 = -0.993,
    main_elevation: f32 = 0.418,
    main_radius: f32 = 17.62,

    pub fn init(screen_width: i32, screen_height: i32) !Renderer {
        const main_height = @divTrunc(screen_height * 7, 10);
        const panel_height = screen_height - main_height;
        const panel_width = @divTrunc(screen_width, 3);
        const close_width = screen_width - panel_width * 2;
        const main_target = try rl.loadRenderTexture(screen_width, main_height);
        errdefer rl.unloadRenderTexture(main_target);
        const close_target = try rl.loadRenderTexture(close_width, panel_height);
        errdefer rl.unloadRenderTexture(close_target);
        const net_texture = try createNetTexture();
        errdefer rl.unloadTexture(net_texture);

        return .{
            .main_target = main_target,
            .close_target = close_target,
            .net_texture = net_texture,
            .main_camera = .{
                .position = toRender(.{ .x = 8.8, .y = -13.5, .z = 8.2 }),
                .target = toRender(.{ .x = 0, .y = 0, .z = 1.05 }),
                .up = .{ .x = 0, .y = 1, .z = 0 },
                .fovy = 34,
                .projection = .perspective,
            },
            .close_camera = .{
                .position = toRender(.{ .x = 0.72, .y = -1.08, .z = 0.48 }),
                .target = toRender(.{ .x = 0, .y = 0, .z = 0.04 }),
                .up = .{ .x = 0, .y = 1, .z = 0 },
                .fovy = 31,
                .projection = .perspective,
            },
            .screen_width = screen_width,
            .screen_height = screen_height,
            .main_height = main_height,
            .panel_height = panel_height,
            .panel_width = panel_width,
            .close_width = close_width,
        };
    }

    pub fn deinit(self: Renderer) void {
        rl.unloadTexture(self.net_texture);
        rl.unloadRenderTexture(self.close_target);
        rl.unloadRenderTexture(self.main_target);
    }

    pub fn updateControls(self: *Renderer, frame_time: f32) ?sim.ShotType {
        const mouse = rl.getMousePosition();
        var requested_shot: ?sim.ShotType = null;
        const main_view = rl.Rectangle{
            .x = 0,
            .y = 0,
            .width = @floatFromInt(self.screen_width),
            .height = @floatFromInt(self.main_height),
        };
        const overlay = rl.Rectangle{ .x = 12, .y = 12, .width = 492, .height = 138 };
        const shot_controls = shotButtonBarBounds(self.screen_width);

        if (rl.isMouseButtonPressed(.left)) requested_shot = shotAtPoint(mouse, self.screen_width);

        if (rl.isMouseButtonPressed(.left) and pointInRectangle(mouse, main_view) and
            !pointInRectangle(mouse, overlay) and !pointInRectangle(mouse, shot_controls))
        {
            self.main_dragging = true;
            self.main_last_mouse = mouse;
        }
        if (self.main_dragging and rl.isMouseButtonDown(.left)) {
            const delta = rl.Vector2{
                .x = mouse.x - self.main_last_mouse.x,
                .y = mouse.y - self.main_last_mouse.y,
            };
            self.main_yaw += delta.x * 0.005;
            self.main_elevation = @max(0.03, @min(1.40, self.main_elevation + delta.y * 0.005));
            self.main_last_mouse = mouse;
        }
        if (!rl.isMouseButtonDown(.left)) self.main_dragging = false;

        self.moveMainCamera(frame_time);
        self.updateMainCameraTransform();
        if (!self.main_dragging) self.updateCloseCamera(mouse);

        const slider = speedSliderBounds();
        if (rl.isMouseButtonDown(.left) and
            mouse.x >= slider.x - 10 and mouse.x <= slider.x + slider.width + 10 and
            mouse.y >= slider.y - 14 and mouse.y <= slider.y + 18)
        {
            const normalized = @max(0, @min(1, (mouse.x - slider.x) / slider.width));
            const index: u2 = @intFromFloat(@round(normalized * 3.0));
            self.speed_mode = @enumFromInt(index);
        }
        return requested_shot;
    }

    pub fn timeScale(self: Renderer) f32 {
        return switch (self.speed_mode) {
            .stopped => 0,
            .x001 => 0.01,
            .x01 => 0.10,
            .x1 => 1.0,
        };
    }

    pub fn focusContactPreview(self: *Renderer, position: sim.Vec3, reverse_side: bool) void {
        self.main_focus = position;
        self.main_yaw = if (reverse_side) -0.82 + std.math.pi else -0.82;
        self.main_elevation = 0.24;
        self.main_radius = 1.15;
        self.main_camera.fovy = 32;
        self.updateMainCameraTransform();
    }

    fn closeViewBounds(self: Renderer) rl.Rectangle {
        return .{
            .x = @floatFromInt(self.panel_width * 2),
            .y = @floatFromInt(self.main_height + 42),
            .width = @floatFromInt(self.close_width),
            .height = @floatFromInt(self.panel_height - 77),
        };
    }

    fn moveMainCamera(self: *Renderer, frame_time: f32) void {
        const radial = sim.Vec3{
            .x = @cos(self.main_yaw) * @cos(self.main_elevation),
            .y = @sin(self.main_yaw) * @cos(self.main_elevation),
            .z = @sin(self.main_elevation),
        };
        const forward = radial.scale(-1);
        const right = forward.cross(.{ .z = 1 }).normalized();
        var movement = sim.Vec3{};
        if (rl.isKeyDown(.w)) movement = movement.add(forward);
        if (rl.isKeyDown(.s)) movement = movement.subtract(forward);
        if (rl.isKeyDown(.d)) movement = movement.add(right);
        if (rl.isKeyDown(.a)) movement = movement.subtract(right);
        if (movement.length() > 0.001) {
            self.main_focus = self.main_focus.add(movement.normalized().scale(8.0 * frame_time));
        }
    }

    fn updateMainCameraTransform(self: *Renderer) void {
        const ground_radius = @cos(self.main_elevation) * self.main_radius;
        const offset = sim.Vec3{
            .x = @cos(self.main_yaw) * ground_radius,
            .y = @sin(self.main_yaw) * ground_radius,
            .z = @sin(self.main_elevation) * self.main_radius,
        };
        self.main_camera.position = toRender(self.main_focus.add(offset));
        self.main_camera.target = toRender(self.main_focus);
    }

    fn updateCloseCamera(self: *Renderer, mouse: rl.Vector2) void {
        const view = self.closeViewBounds();
        if (!pointInRectangle(mouse, view)) return;

        const horizontal = (mouse.x - view.x) / view.width;
        const vertical = (mouse.y - view.y) / view.height;
        const yaw = -0.98 + (horizontal - 0.5) * 2.0 * std.math.pi;
        const elevation = 1.0 - vertical * 1.22;
        const radius: f32 = 1.38;
        const ground_radius = @cos(elevation) * radius;
        self.close_camera.position = toRender(.{
            .x = @cos(yaw) * ground_radius,
            .y = @sin(yaw) * ground_radius,
            .z = 0.04 + @sin(elevation) * radius,
        });
        self.close_camera.target = toRender(.{ .z = 0.04 });
    }

    pub fn draw(self: *const Renderer, simulation: *const sim.Simulation, visible_shuttle: *const sim.Shuttle) void {
        self.drawMainTarget(simulation, visible_shuttle);
        self.drawCloseTarget(visible_shuttle);

        rl.clearBackground(background);
        drawTarget(self.main_target, .{ .x = 0, .y = 0, .width = @floatFromInt(self.screen_width), .height = @floatFromInt(self.main_height) });
        drawMainOverlay(simulation, visible_shuttle.position, self.speed_mode, self.screen_width);

        drawVerticalPlot(simulation, visible_shuttle.position, .{ .x = 0, .y = @floatFromInt(self.main_height), .width = @floatFromInt(self.panel_width), .height = @floatFromInt(self.panel_height) });
        drawVelocityPlot(simulation, visible_shuttle, .{ .x = @floatFromInt(self.panel_width), .y = @floatFromInt(self.main_height), .width = @floatFromInt(self.panel_width), .height = @floatFromInt(self.panel_height) });
        drawClosePanel(self.close_target, simulation, .{ .x = @floatFromInt(self.panel_width * 2), .y = @floatFromInt(self.main_height), .width = @floatFromInt(self.close_width), .height = @floatFromInt(self.panel_height) });
    }

    fn drawMainTarget(self: *const Renderer, simulation: *const sim.Simulation, shuttle: *const sim.Shuttle) void {
        rl.beginTextureMode(self.main_target);
        rl.clearBackground(background);
        rl.beginMode3D(self.main_camera);
        drawCourt();
        drawShuttleShadow(shuttle.position);
        drawShuttle(shuttle.position, shuttle.orientation, 1.0);
        // Draw the transparent net last. Empty texels then reveal the shuttle
        // instead of writing depth first and incorrectly hiding it.
        drawNet(&simulation.net, self.net_texture);
        rl.endMode3D();
        rl.endTextureMode();
    }

    fn drawCloseTarget(self: *const Renderer, shuttle: *const sim.Shuttle) void {
        rl.beginTextureMode(self.close_target);
        rl.clearBackground(color(7, 10, 16, 255));
        rl.beginMode3D(self.close_camera);
        drawInertialAxes(shuttle.angular_velocity.normalized());
        drawShuttle(.{}, shuttle.orientation, 4.8);
        rl.endMode3D();
        rl.endTextureMode();
    }
};

fn color(r: u8, g: u8, b: u8, a: u8) rl.Color {
    return .{ .r = r, .g = g, .b = b, .a = a };
}

fn createNetTexture() !rl.Texture2D {
    const size: usize = 24;
    const cord_pixels: usize = 2;
    var pixels = [_]rl.Color{color(0, 0, 0, 0)} ** (size * size);
    for (&pixels, 0..) |*pixel, index| {
        const x = index % size;
        const y = index / size;
        if (x < cord_pixels or y < cord_pixels) pixel.* = color(184, 196, 204, 128);
    }

    const image = rl.Image{
        .data = @ptrCast(&pixels[0]),
        .width = @intCast(size),
        .height = @intCast(size),
        .mipmaps = 1,
        .format = .uncompressed_r8g8b8a8,
    };
    var texture = try rl.loadTextureFromImage(image);
    rl.genTextureMipmaps(&texture);
    rl.setTextureFilter(texture, .trilinear);
    rl.setTextureWrap(texture, .repeat);
    return texture;
}

fn pointInRectangle(point: rl.Vector2, rectangle: rl.Rectangle) bool {
    return point.x >= rectangle.x and point.x <= rectangle.x + rectangle.width and
        point.y >= rectangle.y and point.y <= rectangle.y + rectangle.height;
}

const ShotButton = struct {
    shot: sim.ShotType,
    label: [:0]const u8,
};

const shot_buttons = [_]ShotButton{
    .{ .shot = .smash, .label = "SMASH" },
    .{ .shot = .net_roll, .label = "NET ROLL" },
    .{ .shot = .serve, .label = "SERVE" },
    .{ .shot = .clear, .label = "CLEAR" },
    .{ .shot = .drop, .label = "DROP" },
};
const shot_button_width: f32 = 80;
const shot_button_height: f32 = 32;
const shot_button_gap: f32 = 6;

fn shotButtonBarBounds(screen_width: i32) rl.Rectangle {
    const count: f32 = @floatFromInt(shot_buttons.len);
    const gaps: f32 = @floatFromInt(shot_buttons.len - 1);
    const width = count * shot_button_width + gaps * shot_button_gap;
    return .{ .x = @as(f32, @floatFromInt(screen_width)) - width - 18, .y = 72, .width = width, .height = shot_button_height };
}

fn shotButtonBounds(screen_width: i32, index: usize) rl.Rectangle {
    const bar = shotButtonBarBounds(screen_width);
    return .{
        .x = bar.x + @as(f32, @floatFromInt(index)) * (shot_button_width + shot_button_gap),
        .y = bar.y,
        .width = shot_button_width,
        .height = shot_button_height,
    };
}

fn shotAtPoint(mouse: rl.Vector2, screen_width: i32) ?sim.ShotType {
    for (shot_buttons, 0..) |button, index| {
        if (pointInRectangle(mouse, shotButtonBounds(screen_width, index))) return button.shot;
    }
    return null;
}

/// Simulation coordinates are right-handed X/Y/Z with Z up. Raylib uses Y up.
fn toRender(v: sim.Vec3) rl.Vector3 {
    return .{ .x = v.x, .y = v.z, .z = -v.y };
}

fn drawTarget(target: rl.RenderTexture2D, destination: rl.Rectangle) void {
    rl.drawTexturePro(
        target.texture,
        .{ .x = 0, .y = 0, .width = @floatFromInt(target.texture.width), .height = -@as(f32, @floatFromInt(target.texture.height)) },
        destination,
        .{ .x = 0, .y = 0 },
        0,
        rl.Color.white,
    );
}

fn courtLine(x1: f32, y1: f32, x2: f32, y2: f32, tint: rl.Color) void {
    rl.drawLine3D(toRender(.{ .x = x1, .y = y1, .z = 0.012 }), toRender(.{ .x = x2, .y = y2, .z = 0.012 }), tint);
}

fn netRenderPoint(net: *const sim.NetState, y: f32, z: f32) rl.Vector3 {
    return toRender(.{ .x = net.displacementAt(y, z), .y = y, .z = z });
}

fn drawCourt() void {
    rl.drawPlane(toRender(.{ .z = -0.025 }), .{ .x = 23, .y = 14 }, color(20, 24, 31, 255));
    rl.drawPlane(toRender(.{}), .{ .x = sim.court_length, .y = sim.court_width }, color(29, 118, 88, 255));

    rl.gl.rlSetLineWidth(2.4);
    const hx = sim.court_length * 0.5;
    const hy = sim.court_width * 0.5;
    const line = color(236, 241, 238, 255);
    courtLine(-hx, -hy, hx, -hy, line);
    courtLine(hx, -hy, hx, hy, line);
    courtLine(hx, hy, -hx, hy, line);
    courtLine(-hx, hy, -hx, -hy, line);
    courtLine(-hx, -2.59, hx, -2.59, line);
    courtLine(-hx, 2.59, hx, 2.59, line);
    courtLine(-1.98, -hy, -1.98, hy, line);
    courtLine(1.98, -hy, 1.98, hy, line);
    courtLine(-5.93, -hy, -5.93, hy, line);
    courtLine(5.93, -hy, 5.93, hy, line);
    courtLine(-hx, 0, -1.98, 0, line);
    courtLine(1.98, 0, hx, 0, line);
    rl.gl.rlSetLineWidth(1.0);
}

fn drawNet(net: *const sim.NetState, net_texture: rl.Texture2D) void {
    const hy = sim.court_width * 0.5;

    const horizontal_segments = 64;
    const vertical_segments = 14;
    const mesh_depth = sim.net_depth - sim.net_tape_depth;

    // A repeating cell texture is mapped in physical metres, so the visible and
    // colliding cords share the same 18 mm pitch without thousands of line calls.
    // Each cell is emitted in both winding orders; this stays two-sided even
    // when rlgl defers the actual draw until after this function returns.
    rl.gl.rlSetTexture(net_texture.id);
    rl.gl.rlBegin(rl.gl.rl_quads);
    rl.gl.rlColor4ub(255, 255, 255, 255);
    for (0..horizontal_segments) |horizontal| {
        const start_fraction = @as(f32, @floatFromInt(horizontal)) / @as(f32, @floatFromInt(horizontal_segments));
        const end_fraction = @as(f32, @floatFromInt(horizontal + 1)) / @as(f32, @floatFromInt(horizontal_segments));
        const y0 = -hy + sim.court_width * start_fraction;
        const y1 = -hy + sim.court_width * end_fraction;
        const top0 = sim.netTopHeight(y0) - sim.net_tape_depth;
        const top1 = sim.netTopHeight(y1) - sim.net_tape_depth;
        const texture_u0 = (y0 + hy) / sim.net_mesh_pitch;
        const texture_u1 = (y1 + hy) / sim.net_mesh_pitch;
        for (0..vertical_segments) |segment| {
            const a = @as(f32, @floatFromInt(segment)) / @as(f32, @floatFromInt(vertical_segments));
            const b = @as(f32, @floatFromInt(segment + 1)) / @as(f32, @floatFromInt(vertical_segments));
            const depth0 = mesh_depth * a;
            const depth1 = mesh_depth * b;
            const texture_v0 = depth0 / sim.net_mesh_pitch;
            const texture_v1 = depth1 / sim.net_mesh_pitch;
            emitNetVertex(netRenderPoint(net, y0, top0 - depth0), texture_u0, texture_v0);
            emitNetVertex(netRenderPoint(net, y0, top0 - depth1), texture_u0, texture_v1);
            emitNetVertex(netRenderPoint(net, y1, top1 - depth1), texture_u1, texture_v1);
            emitNetVertex(netRenderPoint(net, y1, top1 - depth0), texture_u1, texture_v0);
            emitNetVertex(netRenderPoint(net, y1, top1 - depth0), texture_u1, texture_v0);
            emitNetVertex(netRenderPoint(net, y1, top1 - depth1), texture_u1, texture_v1);
            emitNetVertex(netRenderPoint(net, y0, top0 - depth1), texture_u0, texture_v1);
            emitNetVertex(netRenderPoint(net, y0, top0 - depth0), texture_u0, texture_v0);
        }
    }
    rl.gl.rlEnd();
    rl.gl.rlSetTexture(0);

    const tape = color(242, 244, 244, 255);
    const tape_segments = 64;
    for (0..tape_segments) |segment| {
        const a = @as(f32, @floatFromInt(segment)) / @as(f32, @floatFromInt(tape_segments));
        const b = @as(f32, @floatFromInt(segment + 1)) / @as(f32, @floatFromInt(tape_segments));
        const y0 = -hy + sim.court_width * a;
        const y1 = -hy + sim.court_width * b;
        const top0 = sim.netTopHeight(y0);
        const top1 = sim.netTopHeight(y1);
        const p00 = netRenderPoint(net, y0, top0);
        const p01 = netRenderPoint(net, y0, top0 - sim.net_tape_depth);
        const p10 = netRenderPoint(net, y1, top1);
        const p11 = netRenderPoint(net, y1, top1 - sim.net_tape_depth);
        rl.drawTriangle3D(p00, p01, p11, tape);
        rl.drawTriangle3D(p00, p11, p10, tape);
        rl.drawTriangle3D(p11, p01, p00, tape);
        rl.drawTriangle3D(p10, p11, p00, tape);
    }

    rl.gl.rlSetLineWidth(3.2);
    for (0..tape_segments) |segment| {
        const a = @as(f32, @floatFromInt(segment)) / @as(f32, @floatFromInt(tape_segments));
        const b = @as(f32, @floatFromInt(segment + 1)) / @as(f32, @floatFromInt(tape_segments));
        const y0 = -hy + sim.court_width * a;
        const y1 = -hy + sim.court_width * b;
        rl.drawLine3D(netRenderPoint(net, y0, sim.netTopHeight(y0)), netRenderPoint(net, y1, sim.netTopHeight(y1)), net_white);
    }
    rl.gl.rlSetLineWidth(1.0);

    const post = color(222, 226, 231, 255);
    rl.drawCylinderEx(toRender(.{ .y = -3.12 }), toRender(.{ .y = -3.12, .z = 1.62 }), 0.026, 0.026, 12, post);
    rl.drawCylinderEx(toRender(.{ .y = 3.12 }), toRender(.{ .y = 3.12, .z = 1.62 }), 0.026, 0.026, 12, post);
}

fn emitNetVertex(point: rl.Vector3, u: f32, v: f32) void {
    rl.gl.rlTexCoord2f(u, v);
    rl.gl.rlVertex3f(point.x, point.y, point.z);
}

fn drawShuttleShadow(position: sim.Vec3) void {
    const alpha_float = @max(0.08, 0.34 - position.z * 0.055);
    const alpha: u8 = @intFromFloat(alpha_float * 255.0);
    const center = toRender(.{ .x = position.x, .y = position.y, .z = 0.018 });
    const radius: f32 = 0.04 + position.z * 0.008;
    const segments = 24;
    for (0..segments) |index| {
        const a0 = 2.0 * std.math.pi * @as(f32, @floatFromInt(index)) / segments;
        const a1 = 2.0 * std.math.pi * @as(f32, @floatFromInt(index + 1)) / segments;
        const p0 = toRender(.{ .x = position.x + @cos(a0) * radius, .y = position.y + @sin(a0) * radius, .z = 0.018 });
        const p1 = toRender(.{ .x = position.x + @cos(a1) * radius, .y = position.y + @sin(a1) * radius, .z = 0.018 });
        rl.drawTriangle3D(center, p0, p1, color(0, 0, 0, alpha));
    }
}

fn drawShuttle(position: sim.Vec3, orientation: sim.Quaternion, scale: f32) void {
    const render_position = toRender(position);
    const render_rotation = rl.Quaternion{
        .x = orientation.x,
        .y = orientation.z,
        .z = -orientation.y,
        .w = orientation.w,
    };
    const matrix = rl.math.matrixToFloatV(rl.math.quaternionToMatrix(render_rotation));

    rl.gl.rlPushMatrix();
    rl.gl.rlTranslatef(render_position.x, render_position.y, render_position.z);
    rl.gl.rlMultMatrixf(matrix.v[0..]);
    rl.gl.rlScalef(scale, scale, scale);

    const cork = color(210, 184, 137, 255);
    const cork_side = color(190, 158, 112, 255);
    const cork_band = color(113, 91, 65, 255);
    rl.drawSphereEx(.{ .x = 0, .y = -0.027, .z = 0 }, sim.cork_radius, 8, 16, cork);
    rl.drawCylinderEx(.{ .x = 0, .y = -0.027, .z = 0 }, .{ .x = 0, .y = -0.006, .z = 0 }, sim.cork_radius, 0.0123, 18, cork_side);
    rl.drawCylinderEx(.{ .x = 0, .y = -0.009, .z = 0 }, .{ .x = 0, .y = -0.005, .z = 0 }, 0.0125, 0.0125, 18, cork_band);

    const feather_count = 16;
    for (0..feather_count) |index| {
        const angle = 2.0 * std.math.pi * @as(f32, @floatFromInt(index)) / feather_count;
        drawFeather(angle, index);
    }
    const binding = color(191, 176, 147, 255);
    drawRing(0.011, 0.016, binding);
    drawRing(0.029, 0.023, binding);

    rl.gl.rlPopMatrix();
}

const FeatherSection = struct {
    radius: f32,
    height: f32,
    half_angle: f32,
};

fn drawFeather(angle: f32, index: usize) void {
    const profile = [_]FeatherSection{
        .{ .radius = 0.0105, .height = -0.004, .half_angle = 0.025 },
        .{ .radius = 0.0170, .height = 0.015, .half_angle = 0.105 },
        .{ .radius = 0.0270, .height = 0.043, .half_angle = 0.145 },
        .{ .radius = 0.0320, .height = 0.061, .half_angle = 0.105 },
        .{ .radius = sim.skirt_radius, .height = 0.065, .half_angle = 0.030 },
    };
    const vane = if ((index & 1) == 0) color(244, 241, 222, 245) else color(235, 232, 214, 245);
    const vane_detail = color(207, 202, 182, 235);
    const shaft = color(184, 174, 146, 255);

    for (0..profile.len - 1) |section_index| {
        const a = profile[section_index];
        const b = profile[section_index + 1];
        const a_left = ringPoint(a.radius, a.height, angle - a.half_angle);
        const a_right = ringPoint(a.radius, a.height, angle + a.half_angle);
        const b_left = ringPoint(b.radius, b.height, angle - b.half_angle);
        const b_right = ringPoint(b.radius, b.height, angle + b.half_angle);
        rl.drawTriangle3D(a_left, b_left, b_right, vane);
        rl.drawTriangle3D(a_left, b_right, a_right, vane);
    }

    const shaft_start = ringPoint(profile[0].radius, profile[0].height, angle);
    const shaft_end = ringPoint(profile[4].radius, profile[4].height, angle);
    rl.drawCylinderEx(shaft_start, shaft_end, 0.00032, 0.00018, 6, shaft);
    for (1..4) |section_index| {
        const section = profile[section_index];
        const center = ringPoint(section.radius, section.height, angle);
        rl.drawLine3D(center, ringPoint(section.radius, section.height, angle - section.half_angle), vane_detail);
        rl.drawLine3D(center, ringPoint(section.radius, section.height, angle + section.half_angle), vane_detail);
    }
}

fn ringPoint(radius: f32, height: f32, angle: f32) rl.Vector3 {
    return .{ .x = radius * @cos(angle), .y = height, .z = -radius * @sin(angle) };
}

fn drawRing(height: f32, radius: f32, tint: rl.Color) void {
    const segments = 32;
    for (0..segments) |index| {
        const a0 = 2.0 * std.math.pi * @as(f32, @floatFromInt(index)) / segments;
        const a1 = 2.0 * std.math.pi * @as(f32, @floatFromInt(index + 1)) / segments;
        rl.drawLine3D(ringPoint(radius, height, a0), ringPoint(radius, height, a1), tint);
    }
}

fn drawInertialAxes(rotation_axis: sim.Vec3) void {
    drawPositiveAxis(.{ .x = 1 }, 0.27, color(229, 79, 84, 255));
    drawPositiveAxis(.{ .y = 1 }, 0.27, color(76, 205, 126, 255));
    drawPositiveAxis(.{ .z = 1 }, 0.27, color(73, 139, 244, 255));

    const axis = rotation_axis.normalized();
    const start = toRender(axis.scale(-0.35));
    const end = toRender(axis.scale(0.35));
    rl.drawCylinderEx(start, end, 0.006, 0.006, 10, axis_magenta);
    rl.drawCylinderEx(toRender(axis.scale(0.29)), toRender(axis.scale(0.40)), 0.021, 0, 12, axis_magenta);
}

fn drawPositiveAxis(axis: sim.Vec3, length: f32, tint: rl.Color) void {
    rl.drawCylinderEx(toRender(.{}), toRender(axis.scale(length)), 0.0035, 0.0035, 8, tint);
    rl.drawCylinderEx(toRender(axis.scale(length * 0.82)), toRender(axis.scale(length * 1.08)), 0.012, 0, 8, tint);
}

fn speedSliderBounds() rl.Rectangle {
    return .{ .x = 34, .y = 104, .width = 446, .height = 1 };
}

fn speedLabel(mode: SpeedMode) [:0]const u8 {
    return switch (mode) {
        .stopped => "Stopped",
        .x001 => "0.01x",
        .x01 => "0.1x",
        .x1 => "1x",
    };
}

fn drawSpeedSlider(mode: SpeedMode) void {
    const slider = speedSliderBounds();
    const labels = [_][:0]const u8{ "Stopped", "0.01x", "0.1x", "1x" };
    const index: u2 = @intFromEnum(mode);
    const knob_x = slider.x + slider.width * @as(f32, @floatFromInt(index)) / 3.0;

    var label_buffer: [64:0]u8 = undefined;
    const current = std.fmt.bufPrintZ(&label_buffer, "SIMULATION SPEED: {s}", .{speedLabel(mode)}) catch "SIMULATION SPEED";
    rl.drawText(current, 34, 78, 15, rl.Color.white);
    rl.drawLineEx(.{ .x = slider.x, .y = slider.y }, .{ .x = slider.x + slider.width, .y = slider.y }, 2, muted);
    rl.drawLineEx(.{ .x = slider.x, .y = slider.y }, .{ .x = knob_x, .y = slider.y }, 3, trajectory);

    for (labels, 0..) |label, label_index| {
        const x = slider.x + slider.width * @as(f32, @floatFromInt(label_index)) / 3.0;
        const selected = label_index == @as(usize, index);
        rl.drawCircleV(.{ .x = x, .y = slider.y }, if (selected) 6 else 3, if (selected) trajectory else muted);
        const width = rl.measureText(label, 11);
        rl.drawText(label, @intFromFloat(x - @as(f32, @floatFromInt(width)) * 0.5), @intFromFloat(slider.y + 11), 11, muted);
    }
}

fn drawMainOverlay(simulation: *const sim.Simulation, visible_position: sim.Vec3, speed_mode: SpeedMode, screen_width: i32) void {
    rl.drawRectangleRounded(.{ .x = 18, .y = 18, .width = 480, .height = 126 }, 0.10, 8, color(5, 8, 13, 220));
    var title_buffer: [128:0]u8 = undefined;
    const title = std.fmt.bufPrintZ(&title_buffer, "THROW {d}  |  {s}  |  AERODYNAMIC RIGID BODY", .{ simulation.launch_count, sim.shotLabel(simulation.current_shot) }) catch "AERODYNAMIC RIGID BODY";
    rl.drawText(title, 34, 30, 20, rl.Color.white);
    rl.drawText("X cross-court  |  Y parallel  |  Z vertical", 34, 56, 15, muted);
    drawSpeedSlider(speed_mode);
    drawShotButtons(simulation.current_shot, screen_width);

    var state_buffer: [128:0]u8 = undefined;
    const state_text = if (simulation.phase == .flying)
        if (simulation.net.contact_timer > 0)
            std.fmt.bufPrintZ(&state_buffer, "{s}  |  {d} kHz", .{ sim.netMaterialLabel(simulation.net.last_material), simulation.physics_rate_hz / 1000 }) catch "NET CONTACT"
        else
            std.fmt.bufPrintZ(&state_buffer, "FLIGHT  {d:.0} ms    z {d:.2} m  |  {d} kHz", .{ simulation.flight_time * 1000.0, visible_position.z, simulation.physics_rate_hz / 1000 }) catch "FLIGHT"
    else
        std.fmt.bufPrintZ(&state_buffer, "GROUND    next throw in {d:.1}s", .{simulation.resetRemaining()}) catch "GROUND";
    const width = rl.measureText(state_text, 19);
    rl.drawRectangleRounded(.{ .x = @floatFromInt(screen_width - width - 54), .y = 20, .width = @floatFromInt(width + 34), .height = 40 }, 0.18, 8, color(5, 8, 13, 210));
    rl.drawText(state_text, screen_width - width - 37, 31, 19, rl.Color.white);
}

fn drawShotButtons(current_shot: sim.ShotType, screen_width: i32) void {
    const mouse = rl.getMousePosition();
    for (shot_buttons, 0..) |button, index| {
        const bounds = shotButtonBounds(screen_width, index);
        const selected = button.shot == current_shot;
        const hovered = pointInRectangle(mouse, bounds);
        const fill = if (selected) trajectory else if (hovered) color(50, 60, 73, 235) else color(5, 8, 13, 215);
        const text_color = if (selected) color(25, 20, 13, 255) else rl.Color.white;
        rl.drawRectangleRounded(bounds, 0.20, 6, fill);
        rl.drawRectangleRoundedLinesEx(bounds, 0.20, 6, 1, if (selected) trajectory else panel_border);
        const label_width = rl.measureText(button.label, 13);
        rl.drawText(button.label, @intFromFloat(bounds.x + (bounds.width - @as(f32, @floatFromInt(label_width))) * 0.5), @intFromFloat(bounds.y + 9), 13, text_color);
    }
}

fn panelShell(bounds: rl.Rectangle) void {
    rl.drawRectangleRec(bounds, panel_background);
    rl.drawRectangleLinesEx(bounds, 1, panel_border);
}

fn plotBounds(panel: rl.Rectangle) rl.Rectangle {
    return .{ .x = panel.x + 42, .y = panel.y + 42, .width = panel.width - 58, .height = panel.height - 70 };
}

fn mapX(value: f32, plot: rl.Rectangle) f32 {
    return plot.x + (value + plot_x_extent) / (2.0 * plot_x_extent) * plot.width;
}

fn mapZ(value: f32, plot: rl.Rectangle) f32 {
    return plot.y + plot.height - value / plot_z_max * plot.height;
}

fn mapTimeMs(value: f32, plot: rl.Rectangle) f32 {
    const clamped = @max(0, @min(velocity_time_max_ms, value));
    return plot.x + clamped / velocity_time_max_ms * plot.width;
}

fn mapVelocity(value: f32, plot: rl.Rectangle) f32 {
    const clamped = @max(0, @min(velocity_max, value));
    return plot.y + plot.height - clamped / velocity_max * plot.height;
}

fn drawVerticalPlot(simulation: *const sim.Simulation, visible_position: sim.Vec3, panel: rl.Rectangle) void {
    panelShell(panel);
    rl.drawText("VERTICAL POSITION  X-Z", @intFromFloat(panel.x + 14), @intFromFloat(panel.y + 13), 18, rl.Color.white);
    const plot = plotBounds(panel);
    rl.drawRectangleRec(plot, plot_background);

    for (0..8) |index| {
        const z: f32 = @floatFromInt(index);
        const y = mapZ(z, plot);
        rl.drawLineEx(.{ .x = plot.x, .y = y }, .{ .x = plot.x + plot.width, .y = y }, 1, grid);
    }
    const half_court = sim.court_length * 0.5;
    rl.drawLineEx(.{ .x = mapX(-half_court, plot), .y = plot.y }, .{ .x = mapX(-half_court, plot), .y = plot.y + plot.height }, 1, grid);
    rl.drawLineEx(.{ .x = mapX(half_court, plot), .y = plot.y }, .{ .x = mapX(half_court, plot), .y = plot.y + plot.height }, 1, grid);
    const net_x = mapX(0, plot);
    rl.drawLineEx(.{ .x = net_x, .y = plot.y }, .{ .x = net_x, .y = plot.y + plot.height }, 2, net_white);
    rl.drawText("NET", @intFromFloat(net_x + 5), @intFromFloat(plot.y + 5), 12, net_white);

    drawPositionHistory(simulation, visible_position, plot);
    rl.drawText("Z (m)", @intFromFloat(panel.x + 6), @intFromFloat(plot.y + 4), 12, muted);
    rl.drawText("-12", @intFromFloat(plot.x - 2), @intFromFloat(plot.y + plot.height + 7), 12, muted);
    rl.drawText("X (m)", @intFromFloat(plot.x + plot.width - 34), @intFromFloat(plot.y + plot.height + 7), 12, muted);
}

fn drawVelocityPlot(simulation: *const sim.Simulation, visible_shuttle: *const sim.Shuttle, panel: rl.Rectangle) void {
    panelShell(panel);
    rl.drawText("VELOCITY VS TIME FROM IMPACT", @intFromFloat(panel.x + 14), @intFromFloat(panel.y + 13), 18, rl.Color.white);
    const plot = plotBounds(panel);
    rl.drawRectangleRec(plot, plot_background);
    rl.drawRectangleLinesEx(plot, 1, grid);

    for (0..7) |index| {
        const speed = @as(f32, @floatFromInt(index)) * 15.0;
        const y = mapVelocity(speed, plot);
        rl.drawLineEx(.{ .x = plot.x, .y = y }, .{ .x = plot.x + plot.width, .y = y }, 1, grid);
        var speed_buffer: [8:0]u8 = undefined;
        const speed_label = std.fmt.bufPrintZ(&speed_buffer, "{d:.0}", .{speed}) catch "";
        const width = rl.measureText(speed_label, 11);
        rl.drawText(speed_label, @intFromFloat(plot.x - @as(f32, @floatFromInt(width)) - 6), @intFromFloat(y - 5), 11, muted);
    }
    for (0..6) |index| {
        const time_ms = @as(f32, @floatFromInt(index)) * 1000.0;
        const x = mapTimeMs(time_ms, plot);
        rl.drawLineEx(.{ .x = x, .y = plot.y }, .{ .x = x, .y = plot.y + plot.height }, 1, grid);
        var time_buffer: [8:0]u8 = undefined;
        const time_label = std.fmt.bufPrintZ(&time_buffer, "{d:.0}", .{time_ms}) catch "";
        const width = rl.measureText(time_label, 11);
        const label_x = @max(plot.x, @min(plot.x + plot.width - @as(f32, @floatFromInt(width)), x - @as(f32, @floatFromInt(width)) * 0.5));
        rl.drawText(time_label, @intFromFloat(label_x), @intFromFloat(plot.y + plot.height + 7), 11, muted);
    }

    if (simulation.history_len > 0) {
        for (1..simulation.history_len) |index| {
            const previous = simulation.history[index - 1];
            const current = simulation.history[index];
            const p0 = rl.Vector2{ .x = mapTimeMs(previous.time * 1000.0, plot), .y = mapVelocity(previous.speed, plot) };
            const p1 = rl.Vector2{ .x = mapTimeMs(current.time * 1000.0, plot), .y = mapVelocity(current.speed, plot) };
            rl.drawLineEx(p0, p1, 2.4, trajectory);
        }
        const last = simulation.history[simulation.history_len - 1];
        const last_point = rl.Vector2{ .x = mapTimeMs(last.time * 1000.0, plot), .y = mapVelocity(last.speed, plot) };
        const point = rl.Vector2{ .x = mapTimeMs(simulation.flight_time * 1000.0, plot), .y = mapVelocity(visible_shuttle.velocity.length(), plot) };
        rl.drawLineEx(last_point, point, 2.4, trajectory);
        rl.drawCircleV(point, 4.2, trajectory);
    }

    rl.drawText("m/s", @intFromFloat(panel.x + 8), @intFromFloat(plot.y + 4), 12, muted);
    rl.drawText("time (ms)", @intFromFloat(plot.x + plot.width - 52), @intFromFloat(panel.y + 16), 12, muted);
}

fn drawPositionHistory(simulation: *const sim.Simulation, visible_position: sim.Vec3, plot: rl.Rectangle) void {
    if (simulation.history_len == 0) return;
    for (1..simulation.history_len) |index| {
        const previous = simulation.history[index - 1].position;
        const current = simulation.history[index].position;
        const p0 = rl.Vector2{ .x = mapX(previous.x, plot), .y = mapZ(previous.z, plot) };
        const p1 = rl.Vector2{ .x = mapX(current.x, plot), .y = mapZ(current.z, plot) };
        rl.drawLineEx(p0, p1, 2.4, trajectory);
    }
    const last = simulation.history[simulation.history_len - 1].position;
    const last_point = rl.Vector2{ .x = mapX(last.x, plot), .y = mapZ(last.z, plot) };
    const point = rl.Vector2{ .x = mapX(visible_position.x, plot), .y = mapZ(visible_position.z, plot) };
    rl.drawLineEx(last_point, point, 2.4, trajectory);
    rl.drawCircleV(point, 4.2, trajectory);
}

fn drawClosePanel(target: rl.RenderTexture2D, simulation: *const sim.Simulation, panel: rl.Rectangle) void {
    panelShell(panel);
    drawTarget(target, panel);
    rl.drawRectangle(@intFromFloat(panel.x), @intFromFloat(panel.y), @intFromFloat(panel.width), 42, color(7, 10, 16, 205));
    rl.drawRectangle(@intFromFloat(panel.x), @intFromFloat(panel.y + panel.height - 35), @intFromFloat(panel.width), 35, color(7, 10, 16, 205));
    rl.drawText("INERTIAL ORIENTATION", @intFromFloat(panel.x + 14), @intFromFloat(panel.y + 13), 18, rl.Color.white);
    const orbit_hint = "MOVE MOUSE TO ORBIT";
    const hint_width = rl.measureText(orbit_hint, 12);
    rl.drawText(orbit_hint, @as(i32, @intFromFloat(panel.x + panel.width)) - hint_width - 14, @intFromFloat(panel.y + 16), 12, muted);

    var omega_buffer: [96:0]u8 = undefined;
    const omega = std.fmt.bufPrintZ(&omega_buffer, "rotation axis    omega {d:.1} rad/s", .{simulation.shuttle.angular_velocity.length()}) catch "rotation axis";
    rl.drawCircle(@intFromFloat(panel.x + 18), @intFromFloat(panel.y + panel.height - 18), 5, axis_magenta);
    rl.drawText(omega, @intFromFloat(panel.x + 31), @intFromFloat(panel.y + panel.height - 26), 14, rl.Color.white);
    rl.drawText("X", @intFromFloat(panel.x + panel.width - 76), @intFromFloat(panel.y + panel.height - 26), 14, color(229, 79, 84, 255));
    rl.drawText("Y", @intFromFloat(panel.x + panel.width - 53), @intFromFloat(panel.y + panel.height - 26), 14, color(76, 205, 126, 255));
    rl.drawText("Z", @intFromFloat(panel.x + panel.width - 30), @intFromFloat(panel.y + panel.height - 26), 14, color(73, 139, 244, 255));
}
