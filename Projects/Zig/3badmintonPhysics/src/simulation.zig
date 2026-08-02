const std = @import("std");

pub const gravity: f32 = 9.81;
pub const court_length: f32 = 13.40;
pub const court_width: f32 = 6.10;
pub const ground_height: f32 = 0.035;
pub const reset_delay: f32 = 5.0;
pub const history_capacity: usize = 512;

pub const net_half_width: f32 = court_width * 0.5;
pub const net_post_height: f32 = 1.55;
pub const net_center_height: f32 = 1.524;
pub const net_depth: f32 = 0.760;
pub const net_tape_depth: f32 = 0.075;
pub const net_mesh_pitch: f32 = 0.018;
pub const net_cord_radius: f32 = 0.00075;
pub const collision_ring_count: usize = 3;
pub const collision_points_per_ring: usize = 16;
pub const collision_point_count: usize = collision_ring_count * collision_points_per_ring;
pub const collision_longitudinal_edge_count: usize = (collision_ring_count - 1) * collision_points_per_ring;
pub const collision_circumferential_edge_count: usize = collision_ring_count * collision_points_per_ring;
pub const collision_edge_count: usize = collision_longitudinal_edge_count + collision_circumferential_edge_count;
pub const base_physics_rate_hz: u32 = 1000;
pub const approach_physics_rate_hz: u32 = 8000;
pub const contact_physics_rate_hz: u32 = 16_000;

// Regulation feather shuttle and wind-tunnel values (Cohen et al., 2015).
pub const shuttle_mass: f32 = 0.005;
pub const shuttle_length: f32 = 0.100;
pub const feather_length: f32 = 0.066;
pub const skirt_radius: f32 = 0.0325;
pub const cork_radius: f32 = 0.0135;
pub const reference_area: f32 = 0.0028;
pub const drag_coefficient: f32 = 0.65;
pub const air_density: f32 = 1.204;
pub const center_of_pressure_offset: f32 = 0.030;
pub const axial_inertia: f32 = 1.2e-6;
pub const transverse_inertia: f32 = 2.0e-6;
const pitch_damping_coefficient: f32 = 0.15;
const feather_spin_ratio: f32 = 0.04;
const spin_response_time: f32 = 0.08;
const contact_position_slop: f32 = 0.00020;
const contact_position_correction_fraction: f32 = 0.25;
const maximum_contact_correction_speed: f32 = 2.0;

pub fn netTopHeight(y: f32) f32 {
    const normalized = @min(1.0, @abs(y) / net_half_width);
    return net_center_height + (net_post_height - net_center_height) * normalized * normalized;
}

pub const Vec3 = struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,

    pub fn add(a: Vec3, b: Vec3) Vec3 {
        return .{ .x = a.x + b.x, .y = a.y + b.y, .z = a.z + b.z };
    }

    pub fn subtract(a: Vec3, b: Vec3) Vec3 {
        return .{ .x = a.x - b.x, .y = a.y - b.y, .z = a.z - b.z };
    }

    pub fn scale(v: Vec3, scalar: f32) Vec3 {
        return .{ .x = v.x * scalar, .y = v.y * scalar, .z = v.z * scalar };
    }

    pub fn length(v: Vec3) f32 {
        return @sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
    }

    pub fn dot(a: Vec3, b: Vec3) f32 {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    pub fn cross(a: Vec3, b: Vec3) Vec3 {
        return .{
            .x = a.y * b.z - a.z * b.y,
            .y = a.z * b.x - a.x * b.z,
            .z = a.x * b.y - a.y * b.x,
        };
    }

    pub fn normalized(v: Vec3) Vec3 {
        const len = v.length();
        return if (len > 0.00001) v.scale(1.0 / len) else .{ .z = 1.0 };
    }
};

pub const Quaternion = struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,
    w: f32 = 1,

    pub fn fromAxisAngle(axis_unscaled: Vec3, angle: f32) Quaternion {
        const axis = axis_unscaled.normalized();
        const half = angle * 0.5;
        const sine = @sin(half);
        return .{
            .x = axis.x * sine,
            .y = axis.y * sine,
            .z = axis.z * sine,
            .w = @cos(half),
        };
    }

    pub fn fromTo(from_unscaled: Vec3, to_unscaled: Vec3) Quaternion {
        const from = from_unscaled.normalized();
        const to = to_unscaled.normalized();
        const cosine = from.dot(to);
        if (cosine > 0.99999) return .{};
        if (cosine < -0.99999) {
            const helper = if (@abs(from.x) < 0.8) Vec3{ .x = 1 } else Vec3{ .y = 1 };
            return Quaternion.fromAxisAngle(from.cross(helper).normalized(), std.math.pi);
        }
        const axis = from.cross(to);
        return (Quaternion{ .x = axis.x, .y = axis.y, .z = axis.z, .w = 1.0 + cosine }).normalized();
    }

    pub fn multiply(a: Quaternion, b: Quaternion) Quaternion {
        return .{
            .x = a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y,
            .y = a.w * b.y - a.x * b.z + a.y * b.w + a.z * b.x,
            .z = a.w * b.z + a.x * b.y - a.y * b.x + a.z * b.w,
            .w = a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z,
        };
    }

    pub fn normalized(q: Quaternion) Quaternion {
        const len = @sqrt(q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w);
        if (len <= 0.00001) return .{};
        return .{ .x = q.x / len, .y = q.y / len, .z = q.z / len, .w = q.w / len };
    }

    pub fn rotateVector(q: Quaternion, vector: Vec3) Vec3 {
        const q_vector = Vec3{ .x = q.x, .y = q.y, .z = q.z };
        const twice_cross = q_vector.cross(vector).scale(2.0);
        return vector.add(twice_cross.scale(q.w)).add(q_vector.cross(twice_cross));
    }
};

const Rng = struct {
    state: u64,

    fn next(self: *Rng) u64 {
        var value = self.state;
        value ^= value << 13;
        value ^= value >> 7;
        value ^= value << 17;
        self.state = value;
        return value;
    }

    fn unit(self: *Rng) f32 {
        const bits: u24 = @truncate(self.next() >> 40);
        return @as(f32, @floatFromInt(bits)) / 16_777_215.0;
    }

    fn range(self: *Rng, min: f32, max: f32) f32 {
        return min + (max - min) * self.unit();
    }

    fn sign(self: *Rng) f32 {
        return if ((self.next() & 1) == 0) -1.0 else 1.0;
    }

    fn unitVector(self: *Rng) Vec3 {
        return (Vec3{
            .x = self.range(-1.0, 1.0),
            .y = self.range(-1.0, 1.0),
            .z = self.range(-1.0, 1.0),
        }).normalized();
    }
};

pub const Phase = enum { flying, waiting };

pub const ShotType = enum(u3) {
    smash,
    net_roll,
    serve,
    clear,
    drop,
};

pub fn shotLabel(shot: ShotType) [:0]const u8 {
    return switch (shot) {
        .smash => "SMASH",
        .net_roll => "NET ROLL",
        .serve => "SERVE",
        .clear => "CLEAR",
        .drop => "DROP",
    };
}

const ShotProfile = struct {
    start_x: [2]f32,
    start_y: [2]f32,
    start_z: [2]f32,
    target_x: [2]f32,
    target_y: [2]f32,
    speed: [2]f32,
    elevation_degrees: [2]f32,
    initial_tilt_degrees: [2]f32,
    turnover_rate: [2]f32,
    axial_spin_rate: [2]f32,
    tape_crossing_offset: ?[2]f32 = null,
    landing_depth: ?[2]f32 = null,
};

fn shotProfile(shot: ShotType) ShotProfile {
    return switch (shot) {
        .smash => .{
            .start_x = .{ 4.9, 6.1 },
            .start_y = .{ -2.25, 2.25 },
            .start_z = .{ 2.7, 3.1 },
            .target_x = .{ 3.1, 5.7 },
            .target_y = .{ -2.35, 2.35 },
            .speed = .{ 68.0, 82.0 },
            .elevation_degrees = .{ -8.0, -4.0 },
            .initial_tilt_degrees = .{ 3.0, 10.0 },
            .turnover_rate = .{ 35.0, 100.0 },
            .axial_spin_rate = .{ 0, 8.0 },
        },
        .net_roll => .{
            .start_x = .{ 0.55, 0.95 },
            .start_y = .{ -2.15, 2.15 },
            .start_z = .{ 1.24, 1.43 },
            .target_x = .{ 0.55, 1.00 },
            .target_y = .{ -2.20, 2.20 },
            .speed = .{ 5.8, 8.8 },
            .elevation_degrees = .{ 10.0, 42.0 },
            .initial_tilt_degrees = .{ 18.0, 55.0 },
            .turnover_rate = .{ 95.0, 165.0 },
            .axial_spin_rate = .{ 45.0, 120.0 },
            .tape_crossing_offset = .{ 0.0030, 0.0250 },
        },
        .serve => .{
            .start_x = .{ 1.15, 1.85 },
            .start_y = .{ -2.15, 2.15 },
            .start_z = .{ 1.05, 1.15 },
            .target_x = .{ 1.95, 2.55 },
            .target_y = .{ -2.20, 2.20 },
            .speed = .{ 8.0, 13.0 },
            .elevation_degrees = .{ 16.0, 27.0 },
            .initial_tilt_degrees = .{ 2.0, 6.0 },
            .turnover_rate = .{ 12.0, 35.0 },
            .axial_spin_rate = .{ 0, 5.0 },
            .tape_crossing_offset = .{ 0.055, 0.090 },
            .landing_depth = .{ 2.10, 2.85 },
        },
        .clear => .{
            .start_x = .{ 4.9, 6.0 },
            .start_y = .{ -2.25, 2.25 },
            .start_z = .{ 2.1, 2.8 },
            .target_x = .{ 4.8, 6.1 },
            .target_y = .{ -2.35, 2.35 },
            .speed = .{ 44.0, 56.0 },
            .elevation_degrees = .{ 32.0, 42.0 },
            .initial_tilt_degrees = .{ 3.0, 12.0 },
            .turnover_rate = .{ 28.0, 90.0 },
            .axial_spin_rate = .{ 0, 8.0 },
        },
        .drop => .{
            .start_x = .{ 4.9, 6.0 },
            .start_y = .{ -2.25, 2.25 },
            .start_z = .{ 2.3, 2.9 },
            .target_x = .{ 0.9, 1.8 },
            .target_y = .{ -2.25, 2.25 },
            .speed = .{ 19.0, 27.0 },
            .elevation_degrees = .{ 1.0, 12.0 },
            .initial_tilt_degrees = .{ 3.0, 14.0 },
            .turnover_rate = .{ 25.0, 80.0 },
            .axial_spin_rate = .{ 0, 10.0 },
        },
    };
}

pub const Shuttle = struct {
    position: Vec3,
    velocity: Vec3,
    orientation: Quaternion,
    angular_velocity: Vec3,
};

pub const HistorySample = struct {
    position: Vec3,
    speed: f32,
    time: f32,
};

pub const NetMaterial = enum {
    none,
    tape,
    mesh,
};

pub fn netMaterialLabel(material: NetMaterial) [:0]const u8 {
    return switch (material) {
        .none => "NET",
        .tape => "TAPE CONTACT",
        .mesh => "MESH CONTACT",
    };
}

const net_patch_size: usize = 13;
const net_patch_node_count: usize = net_patch_size * net_patch_size;
const net_patch_half_extent: f32 = @as(f32, @floatFromInt(net_patch_size - 1)) * net_mesh_pitch * 0.5;

fn netNodeMass(material: NetMaterial) f32 {
    return switch (material) {
        .tape => 0.0035,
        .mesh => 0.0015,
        .none => 0.0035,
    };
}

const NetWeights = struct {
    indices: [4]usize,
    values: [4]f32,
};

pub const NetState = struct {
    displacement: [net_patch_node_count]f32 = [_]f32{0} ** net_patch_node_count,
    velocity: [net_patch_node_count]f32 = [_]f32{0} ** net_patch_node_count,
    origin_y: f32 = 0,
    origin_z: f32 = 0,
    patch_active: bool = false,
    last_material: NetMaterial = .none,
    contact_y: f32 = 0,
    contact_z: f32 = 0,
    contact_timer: f32 = 0,
    contact_count: u32 = 0,

    fn nodeIndex(x: usize, y: usize) usize {
        return y * net_patch_size + x;
    }

    fn weightsAt(self: *const NetState, y: f32, z: f32) ?NetWeights {
        if (!self.patch_active) return null;
        const u = (y - self.origin_y) / net_mesh_pitch;
        const v = (z - self.origin_z) / net_mesh_pitch;
        const limit: f32 = @floatFromInt(net_patch_size - 1);
        if (u < 0 or v < 0 or u >= limit or v >= limit) return null;

        const x0: usize = @intFromFloat(@floor(u));
        const y0: usize = @intFromFloat(@floor(v));
        const tx = u - @as(f32, @floatFromInt(x0));
        const ty = v - @as(f32, @floatFromInt(y0));
        return .{
            .indices = .{
                nodeIndex(x0, y0),
                nodeIndex(x0 + 1, y0),
                nodeIndex(x0, y0 + 1),
                nodeIndex(x0 + 1, y0 + 1),
            },
            .values = .{
                (1.0 - tx) * (1.0 - ty),
                tx * (1.0 - ty),
                (1.0 - tx) * ty,
                tx * ty,
            },
        };
    }

    fn ensurePatch(self: *NetState, y: f32, z: f32, material: NetMaterial) void {
        const inside_reusable_patch = self.patch_active and material == self.last_material and
            y > self.origin_y + 2.0 * net_mesh_pitch and
            y < self.origin_y + 10.0 * net_mesh_pitch and
            z > self.origin_z + 2.0 * net_mesh_pitch and
            z < self.origin_z + 10.0 * net_mesh_pitch;
        if (inside_reusable_patch) return;

        self.displacement = [_]f32{0} ** net_patch_node_count;
        self.velocity = [_]f32{0} ** net_patch_node_count;
        self.origin_y = y - net_patch_half_extent;
        self.origin_z = z - net_patch_half_extent;
        self.patch_active = true;
        self.last_material = material;
    }

    pub fn displacementAt(self: *const NetState, y: f32, z: f32) f32 {
        const weights = self.weightsAt(y, z) orelse return 0;
        var result: f32 = 0;
        for (weights.indices, weights.values) |index, weight| result += self.displacement[index] * weight;
        return result;
    }

    fn velocityAt(self: *const NetState, y: f32, z: f32) f32 {
        const weights = self.weightsAt(y, z) orelse return 0;
        var result: f32 = 0;
        for (weights.indices, weights.values) |index, weight| result += self.velocity[index] * weight;
        return result;
    }

    fn inverseMassAt(self: *const NetState, y: f32, z: f32) f32 {
        const weights = self.weightsAt(y, z) orelse return 1.0 / netNodeMass(self.last_material);
        var result: f32 = 0;
        const mass = netNodeMass(self.last_material);
        for (weights.values) |weight| result += weight * weight / mass;
        return result;
    }

    fn applyImpulse(self: *NetState, y: f32, z: f32, impulse_x: f32) void {
        const weights = self.weightsAt(y, z) orelse return;
        const mass = netNodeMass(self.last_material);
        for (weights.indices, weights.values) |index, weight| {
            self.velocity[index] += impulse_x * weight / mass;
        }
    }

    fn integrate(self: *NetState, dt: f32) void {
        if (!self.patch_active) return;
        var acceleration = [_]f32{0} ** net_patch_node_count;
        const wave_speed: f32 = if (self.last_material == .tape) 18.0 else 7.0;
        const damping: f32 = if (self.last_material == .tape) 30.0 else 18.0;
        const anchoring: f32 = if (self.last_material == .tape) 220.0 else 70.0;
        const inverse_spacing_squared = 1.0 / (net_mesh_pitch * net_mesh_pitch);

        for (1..net_patch_size - 1) |row| {
            for (1..net_patch_size - 1) |column| {
                const index = nodeIndex(column, row);
                const laplacian = (self.displacement[nodeIndex(column - 1, row)] +
                    self.displacement[nodeIndex(column + 1, row)] +
                    self.displacement[nodeIndex(column, row - 1)] +
                    self.displacement[nodeIndex(column, row + 1)] -
                    4.0 * self.displacement[index]) * inverse_spacing_squared;
                acceleration[index] = wave_speed * wave_speed * laplacian -
                    damping * self.velocity[index] - anchoring * self.displacement[index];
            }
        }

        var maximum_motion: f32 = 0;
        for (1..net_patch_size - 1) |row| {
            for (1..net_patch_size - 1) |column| {
                const index = nodeIndex(column, row);
                self.velocity[index] += acceleration[index] * dt;
                self.displacement[index] += self.velocity[index] * dt;
                self.displacement[index] = @max(-0.12, @min(0.12, self.displacement[index]));
                maximum_motion = @max(maximum_motion, @abs(self.velocity[index]) + 20.0 * @abs(self.displacement[index]));
            }
        }

        self.contact_timer = @max(0, self.contact_timer - dt);
        if (self.contact_timer <= 0 and maximum_motion < 0.002) self.clearPatch();
    }

    fn relax(self: *NetState, dt: f32) void {
        if (!self.patch_active) return;
        const decay = @exp(-18.0 * dt);
        for (&self.displacement, &self.velocity) |*position, *speed| {
            position.* *= decay;
            speed.* *= decay;
        }
        self.contact_timer = @max(0, self.contact_timer - dt);
        if (decay < 0.001 or self.contact_timer <= 0) self.clearPatch();
    }

    fn clearPatch(self: *NetState) void {
        const previous_count = self.contact_count;
        const previous_material = self.last_material;
        self.* = .{};
        self.contact_count = previous_count;
        self.last_material = previous_material;
    }
};

const Accelerations = struct {
    linear: Vec3,
    angular: Vec3,
};

fn aerodynamicAccelerations(shuttle: Shuttle) Accelerations {
    var linear = Vec3{ .z = -gravity };
    var angular = Vec3{};
    const speed = shuttle.velocity.length();
    if (speed < 0.01) return .{ .linear = linear, .angular = angular };

    const drag_force = shuttle.velocity.scale(-0.5 * air_density * drag_coefficient * reference_area * speed);
    linear = linear.add(drag_force.scale(1.0 / shuttle_mass));

    // Local +Z points from cork to skirt. Stable flight has this axis opposite velocity.
    const back_axis = shuttle.orientation.rotateVector(.{ .z = 1 }).normalized();
    const pressure_arm = back_axis.scale(center_of_pressure_offset);
    const restoring_torque = pressure_arm.cross(drag_force);

    const axial_rate = shuttle.angular_velocity.dot(back_axis);
    const perpendicular_rate = shuttle.angular_velocity.subtract(back_axis.scale(axial_rate));
    const pitch_damping = pitch_damping_coefficient * air_density * reference_area * shuttle_length * shuttle_length * speed;
    const total_pitch_torque = restoring_torque.add(perpendicular_rate.scale(-pitch_damping));
    const parallel_torque = total_pitch_torque.dot(back_axis);
    const perpendicular_torque = total_pitch_torque.subtract(back_axis.scale(parallel_torque));

    angular = perpendicular_torque.scale(1.0 / transverse_inertia)
        .add(back_axis.scale(parallel_torque / axial_inertia));

    // Overlapping feathers make a real feather shuttle spin as R*omega/U ~= 0.04.
    const target_axial_rate = feather_spin_ratio * speed / skirt_radius;
    angular = angular.add(back_axis.scale((target_axial_rate - axial_rate) / spin_response_time));
    return .{ .linear = linear, .angular = angular };
}

fn integrateRigidBody(shuttle: *Shuttle, dt: f32) void {
    const acceleration = aerodynamicAccelerations(shuttle.*);
    shuttle.position = shuttle.position.add(shuttle.velocity.scale(dt)).add(acceleration.linear.scale(0.5 * dt * dt));
    shuttle.velocity = shuttle.velocity.add(acceleration.linear.scale(dt));

    const middle_angular_velocity = shuttle.angular_velocity.add(acceleration.angular.scale(0.5 * dt));
    shuttle.angular_velocity = shuttle.angular_velocity.add(acceleration.angular.scale(dt));
    const angular_speed = middle_angular_velocity.length();
    if (angular_speed > 0.0001) {
        const delta = Quaternion.fromAxisAngle(middle_angular_velocity.scale(1.0 / angular_speed), angular_speed * dt);
        shuttle.orientation = delta.multiply(shuttle.orientation).normalized();
    }
}

const CollisionPart = enum { cork, feather };

const ContactCandidate = struct {
    material: NetMaterial,
    part: CollisionPart,
    world_point: Vec3,
    normal: Vec3,
    penetration: f32,
};

const StepChoice = struct {
    dt: f32,
    rate_hz: u32,
};

fn collisionLocalPoint(index: usize) Vec3 {
    // These three rings coincide with the rendered feather profile at its base,
    // widest body section, and tip. Swept edges below connect the rings.
    const radii = [_]f32{ 0.0105, 0.0270, skirt_radius };
    const heights = [_]f32{ -0.004, 0.043, 0.065 };
    const ring = index / collision_points_per_ring;
    const feather = index % collision_points_per_ring;
    const angle = 2.0 * std.math.pi * @as(f32, @floatFromInt(feather)) / @as(f32, @floatFromInt(collision_points_per_ring));
    return .{
        .x = radii[ring] * @cos(angle),
        .y = radii[ring] * @sin(angle),
        .z = heights[ring],
    };
}

fn worldPoint(shuttle: Shuttle, local_point: Vec3) Vec3 {
    return shuttle.position.add(shuttle.orientation.rotateVector(local_point));
}

fn interpolate(a: Vec3, b: Vec3, amount: f32) Vec3 {
    return a.add(b.subtract(a).scale(amount));
}

fn pointVelocity(shuttle: Shuttle, world_point: Vec3) Vec3 {
    const lever = world_point.subtract(shuttle.position);
    return shuttle.velocity.add(shuttle.angular_velocity.cross(lever));
}

fn inverseInertiaApply(shuttle: Shuttle, vector: Vec3) Vec3 {
    const back_axis = shuttle.orientation.rotateVector(.{ .z = 1 }).normalized();
    const parallel = back_axis.scale(vector.dot(back_axis));
    const perpendicular = vector.subtract(parallel);
    return parallel.scale(1.0 / axial_inertia).add(perpendicular.scale(1.0 / transverse_inertia));
}

fn contactInverseMass(shuttle: Shuttle, world_point: Vec3, direction: Vec3) f32 {
    const lever = world_point.subtract(shuttle.position);
    const angular_impulse = lever.cross(direction);
    const angular_response = inverseInertiaApply(shuttle, angular_impulse).cross(lever).dot(direction);
    return 1.0 / shuttle_mass + @max(0, angular_response);
}

fn netMaterialAt(y: f32, z: f32, contact_radius: f32) ?NetMaterial {
    if (@abs(y) > net_half_width + contact_radius) return null;
    const clamped_y = @max(-net_half_width, @min(net_half_width, y));
    const top = netTopHeight(clamped_y);
    const tape_bottom = top - net_tape_depth;
    // The folded tape is nearly planar; allow only the small top cord radius,
    // rather than the old invisible 10 mm collision lip above the rendering.
    if (z + contact_radius >= tape_bottom and z - contact_radius <= top + 0.0035) return .tape;

    const bottom = top - net_depth;
    if (z + contact_radius < bottom or z - contact_radius > tape_bottom) return null;

    const vertical_index = @round((clamped_y + net_half_width) / net_mesh_pitch);
    const nearest_vertical = -net_half_width + vertical_index * net_mesh_pitch;
    const vertical_distance = @abs(clamped_y - nearest_vertical);
    const depth = tape_bottom - z;
    const horizontal_distance = @abs(depth - @round(depth / net_mesh_pitch) * net_mesh_pitch);
    const reach = contact_radius + net_cord_radius;
    return if (@min(vertical_distance, horizontal_distance) <= reach) .mesh else null;
}

fn considerContact(
    best: *?ContactCandidate,
    previous_point: Vec3,
    current_point: Vec3,
    contact_radius: f32,
    part: CollisionPart,
    side: f32,
    net: *const NetState,
) void {
    const plane_x = net.displacementAt(current_point.y, current_point.z);
    const previous_plane_x = net.displacementAt(previous_point.y, previous_point.z);
    const previous_distance = side * (previous_point.x - previous_plane_x);
    const current_distance = side * (current_point.x - plane_x);
    const approaching = current_distance < previous_distance - 0.000001;
    if (current_distance > contact_radius or (!approaching and current_distance >= 0) or previous_distance < -2.0 * contact_radius) return;
    const material = netMaterialAt(current_point.y, current_point.z, contact_radius) orelse return;
    const penetration = contact_radius - current_distance;
    const normal = Vec3{ .x = side };
    const contact_point = if (part == .cork) current_point.subtract(normal.scale(contact_radius)) else current_point;
    const candidate = ContactCandidate{
        .material = material,
        .part = part,
        .world_point = contact_point,
        .normal = normal,
        .penetration = penetration,
    };
    if (best.* == null or penetration > best.*.?.penetration) best.* = candidate;
}

fn considerSweptEdge(
    best: *?ContactCandidate,
    previous_a: Vec3,
    previous_b: Vec3,
    current_a: Vec3,
    current_b: Vec3,
    side: f32,
    net: *const NetState,
) void {
    const contact_radius: f32 = 0.0012;
    const maximum_spacing: f32 = 0.0030;
    const edge_length = @max(previous_b.subtract(previous_a).length(), current_b.subtract(current_a).length());
    const subdivision_count: usize = @max(1, @as(usize, @intFromFloat(@ceil(edge_length / maximum_spacing))));

    // Endpoints are already covered by the 48 cage nodes. Interior samples are
    // close enough that their swept spheres overlap every 18 mm net cord.
    for (1..subdivision_count) |sample| {
        const amount = @as(f32, @floatFromInt(sample)) / @as(f32, @floatFromInt(subdivision_count));
        considerContact(
            best,
            interpolate(previous_a, previous_b, amount),
            interpolate(current_a, current_b, amount),
            contact_radius,
            .feather,
            side,
            net,
        );
    }
}

fn findContact(previous: Shuttle, current: Shuttle, net: *const NetState) ?ContactCandidate {
    var best: ?ContactCandidate = null;
    const current_plane_x = net.displacementAt(current.position.y, current.position.z);
    const previous_plane_x = net.displacementAt(previous.position.y, previous.position.z);
    const current_center_distance = current.position.x - current_plane_x;
    const previous_center_distance = previous.position.x - previous_plane_x;
    const side_hysteresis: f32 = 0.008;
    // Treat the thin net as one-sided only for the current manifold. Once the
    // center has genuinely moved through the deformed plane, switch sides; do
    // not keep the launch-side normal alive for the 180 ms visual contact timer.
    const side: f32 = if (current_center_distance > side_hysteresis)
        1
    else if (current_center_distance < -side_hysteresis)
        -1
    else if (previous_center_distance >= 0)
        1
    else
        -1;

    const cork_local = Vec3{ .z = -0.027 };
    considerContact(
        &best,
        worldPoint(previous, cork_local),
        worldPoint(current, cork_local),
        cork_radius,
        .cork,
        side,
        net,
    );

    for (0..collision_point_count) |index| {
        const local_point = collisionLocalPoint(index);
        considerContact(
            &best,
            worldPoint(previous, local_point),
            worldPoint(current, local_point),
            0.0012,
            .feather,
            side,
            net,
        );
    }

    // Each feather shaft joins the three profile rings (32 edges), while each
    // ring is closed circumferentially (48 edges). This removes gaps through
    // which the rendered rigid skirt could previously pass.
    for (0..collision_ring_count - 1) |ring| {
        for (0..collision_points_per_ring) |feather| {
            const start_index = ring * collision_points_per_ring + feather;
            const end_index = (ring + 1) * collision_points_per_ring + feather;
            considerSweptEdge(
                &best,
                worldPoint(previous, collisionLocalPoint(start_index)),
                worldPoint(previous, collisionLocalPoint(end_index)),
                worldPoint(current, collisionLocalPoint(start_index)),
                worldPoint(current, collisionLocalPoint(end_index)),
                side,
                net,
            );
        }
    }
    for (0..collision_ring_count) |ring| {
        for (0..collision_points_per_ring) |feather| {
            const start_index = ring * collision_points_per_ring + feather;
            const end_index = ring * collision_points_per_ring + (feather + 1) % collision_points_per_ring;
            considerSweptEdge(
                &best,
                worldPoint(previous, collisionLocalPoint(start_index)),
                worldPoint(previous, collisionLocalPoint(end_index)),
                worldPoint(current, collisionLocalPoint(start_index)),
                worldPoint(current, collisionLocalPoint(end_index)),
                side,
                net,
            );
        }
    }
    return best;
}

fn applyShuttleImpulse(shuttle: *Shuttle, world_point: Vec3, impulse: Vec3) void {
    shuttle.velocity = shuttle.velocity.add(impulse.scale(1.0 / shuttle_mass));
    const lever = world_point.subtract(shuttle.position);
    shuttle.angular_velocity = shuttle.angular_velocity.add(inverseInertiaApply(shuttle.*, lever.cross(impulse)));
}

fn resolveContact(shuttle: *Shuttle, net: *NetState, contact: ContactCandidate, correction_budget: *f32) void {
    const is_new_event = net.contact_timer <= 0;
    net.ensurePatch(contact.world_point.y, contact.world_point.z, contact.material);
    net.last_material = contact.material;
    net.contact_y = contact.world_point.y;
    net.contact_z = contact.world_point.z;
    net.contact_timer = 0.18;
    if (is_new_event) net.contact_count += 1;

    const net_velocity = Vec3{ .x = net.velocityAt(contact.world_point.y, contact.world_point.z) };
    const relative_velocity = pointVelocity(shuttle.*, contact.world_point).subtract(net_velocity);
    const normal_speed = relative_velocity.dot(contact.normal);
    var normal_impulse_magnitude: f32 = 0;
    if (normal_speed < 0) {
        const shuttle_inverse_mass = contactInverseMass(shuttle.*, contact.world_point, contact.normal);
        const net_inverse_mass = net.inverseMassAt(contact.world_point.y, contact.world_point.z);
        const restitution: f32 = if (contact.material == .tape) 0.16 else 0.025;
        normal_impulse_magnitude = -(1.0 + restitution) * normal_speed / (shuttle_inverse_mass + net_inverse_mass);

        const normal_impulse = contact.normal.scale(normal_impulse_magnitude);
        applyShuttleImpulse(shuttle, contact.world_point, normal_impulse);
        net.applyImpulse(contact.world_point.y, contact.world_point.z, -normal_impulse.x);

        const updated_relative_velocity = pointVelocity(shuttle.*, contact.world_point)
            .subtract(Vec3{ .x = net.velocityAt(contact.world_point.y, contact.world_point.z) });
        const tangent_velocity = updated_relative_velocity.subtract(contact.normal.scale(updated_relative_velocity.dot(contact.normal)));
        const tangent_speed = tangent_velocity.length();
        if (tangent_speed > 0.0001) {
            const tangent_direction = tangent_velocity.scale(-1.0 / tangent_speed);
            const tangent_inverse_mass = contactInverseMass(shuttle.*, contact.world_point, tangent_direction);
            const friction: f32 = if (contact.material == .tape) 0.09 else 0.42;
            const friction_impulse_magnitude = @min(friction * normal_impulse_magnitude, tangent_speed / tangent_inverse_mass);
            applyShuttleImpulse(shuttle, contact.world_point, tangent_direction.scale(friction_impulse_magnitude));
        }
    }

    // Penetration projection is only numerical stabilization. Without a rate
    // limit, several skirt contacts can translate the whole rigid body by
    // centimetres in one millisecond even when its physical speed is low.
    const part_scale: f32 = if (contact.part == .feather) 0.90 else 0.95;
    const desired_correction = @max(0, contact.penetration - contact_position_slop) *
        contact_position_correction_fraction * part_scale;
    const correction = @min(correction_budget.*, desired_correction);
    shuttle.position = shuttle.position.add(contact.normal.scale(correction));
    correction_budget.* -= correction;
}

fn chooseStep(shuttle: Shuttle, net: *const NetState) StepChoice {
    if (@abs(shuttle.position.y) > net_half_width + 0.12 or
        shuttle.position.z < net_center_height - net_depth - 0.10 or
        shuttle.position.z > net_post_height + 0.14)
    {
        return .{ .dt = 1.0 / @as(f32, @floatFromInt(base_physics_rate_hz)), .rate_hz = base_physics_rate_hz };
    }

    const plane_x = net.displacementAt(shuttle.position.y, shuttle.position.z);
    const distance = shuttle.position.x - plane_x;
    const time_to_plane = if (@abs(shuttle.velocity.x) > 0.001) -distance / shuttle.velocity.x else 1000.0;
    const approaching = time_to_plane >= 0 and time_to_plane < 0.020;
    const nearby = @abs(distance) < 0.22;
    if (!approaching and !nearby and !net.patch_active)
        return .{ .dt = 1.0 / @as(f32, @floatFromInt(base_physics_rate_hz)), .rate_hz = base_physics_rate_hz };

    if (@abs(distance) < 0.09 or net.patch_active or net.contact_timer > 0)
        return .{ .dt = 1.0 / @as(f32, @floatFromInt(contact_physics_rate_hz)), .rate_hz = contact_physics_rate_hz };
    return .{ .dt = 1.0 / @as(f32, @floatFromInt(approach_physics_rate_hz)), .rate_hz = approach_physics_rate_hz };
}

fn advanceAdaptive(shuttle: *Shuttle, net: *NetState, total_dt: f32, rate_out: ?*u32) void {
    var remaining = total_dt;
    while (remaining > 0.00000001) {
        const choice = chooseStep(shuttle.*, net);
        const dt = @min(remaining, choice.dt);
        if (rate_out) |rate| rate.* = @max(rate.*, choice.rate_hz);

        net.integrate(dt);
        const previous = shuttle.*;
        integrateRigidBody(shuttle, dt);
        var correction_budget = maximum_contact_correction_speed * dt;
        // A tilted skirt can touch at several places in one substep. Resolve a
        // short impulse manifold, while all contacts share one bounded position
        // stabilization budget for this substep.
        for (0..4) |_| {
            const contact = findContact(previous, shuttle.*, net) orelse break;
            resolveContact(shuttle, net, contact, &correction_budget);
        }
        remaining -= dt;
    }
}

fn crossingHeight(start: Vec3, horizontal_direction: Vec3, speed: f32, elevation: f32) f32 {
    var shuttle = Shuttle{
        .position = start,
        .velocity = horizontal_direction.scale(speed * @cos(elevation)).add(.{ .z = speed * @sin(elevation) }),
        .orientation = .{},
        .angular_velocity = .{},
    };
    const start_side = if (start.x >= 0) @as(f32, 1) else -1.0;
    var previous = shuttle.position;
    for (0..8000) |_| {
        integrateRigidBody(&shuttle, 0.00025);
        if (start_side * shuttle.position.x <= 0) {
            const span = previous.x - shuttle.position.x;
            const interpolation = if (@abs(span) > 0.000001) previous.x / span else 0;
            return previous.z + (shuttle.position.z - previous.z) * interpolation;
        }
        previous = shuttle.position;
    }
    return shuttle.position.z;
}

fn elevationForCrossingHeight(start: Vec3, horizontal_direction: Vec3, speed: f32, desired_height: f32) f32 {
    var low: f32 = -10.0 * std.math.pi / 180.0;
    var high: f32 = 60.0 * std.math.pi / 180.0;
    for (0..14) |_| {
        const middle = (low + high) * 0.5;
        if (crossingHeight(start, horizontal_direction, speed, middle) < desired_height)
            low = middle
        else
            high = middle;
    }
    return (low + high) * 0.5;
}

fn landingDepthFor(start: Vec3, horizontal_direction: Vec3, speed: f32, elevation: f32) f32 {
    var shuttle = Shuttle{
        .position = start,
        .velocity = horizontal_direction.scale(speed * @cos(elevation)).add(.{ .z = speed * @sin(elevation) }),
        .orientation = .{},
        .angular_velocity = .{},
    };
    const start_side: f32 = if (start.x >= 0) 1 else -1;
    for (0..6000) |_| {
        integrateRigidBody(&shuttle, 0.0005);
        if (shuttle.position.z <= ground_height) return -start_side * shuttle.position.x;
    }
    return -start_side * shuttle.position.x;
}

const LaunchSolution = struct {
    speed: f32,
    elevation: f32,
};

fn solutionForLandingDepth(
    start: Vec3,
    horizontal_direction: Vec3,
    speed_range: [2]f32,
    desired_crossing_height: f32,
    desired_landing_depth: f32,
) LaunchSolution {
    var low = speed_range[0];
    var high = speed_range[1];
    for (0..14) |_| {
        const speed = (low + high) * 0.5;
        const elevation = elevationForCrossingHeight(start, horizontal_direction, speed, desired_crossing_height);
        if (landingDepthFor(start, horizontal_direction, speed, elevation) < desired_landing_depth)
            low = speed
        else
            high = speed;
    }
    const speed = (low + high) * 0.5;
    return .{
        .speed = speed,
        .elevation = elevationForCrossingHeight(start, horizontal_direction, speed, desired_crossing_height),
    };
}

pub const Simulation = struct {
    shuttle: Shuttle = undefined,
    phase: Phase = .flying,
    flight_time: f32 = 0,
    wait_time: f32 = 0,
    history: [history_capacity]HistorySample = undefined,
    history_len: usize = 0,
    history_clock: f32 = 0,
    launch_count: u32 = 0,
    current_shot: ShotType = .smash,
    net: NetState = .{},
    physics_rate_hz: u32 = base_physics_rate_hz,
    rng: Rng,

    pub fn init(seed: u64) Simulation {
        var result = Simulation{ .rng = .{ .state = if (seed == 0) 0x63d8_35f7_a12c_9b41 else seed } };
        result.launch();
        return result;
    }

    pub fn launch(self: *Simulation) void {
        self.launchShot(self.current_shot);
    }

    pub fn launchShot(self: *Simulation, shot: ShotType) void {
        const profile = shotProfile(shot);
        const side = self.rng.sign();
        const start = Vec3{
            .x = side * self.rng.range(profile.start_x[0], profile.start_x[1]),
            .y = self.rng.range(profile.start_y[0], profile.start_y[1]),
            .z = self.rng.range(profile.start_z[0], profile.start_z[1]),
        };
        const target = Vec3{
            .x = -side * self.rng.range(profile.target_x[0], profile.target_x[1]),
            .y = self.rng.range(profile.target_y[0], profile.target_y[1]),
            .z = start.z,
        };
        const horizontal_direction = target.subtract(start).normalized();
        var launch_speed = self.rng.range(profile.speed[0], profile.speed[1]);
        var elevation = self.rng.range(profile.elevation_degrees[0], profile.elevation_degrees[1]) * std.math.pi / 180.0;
        if (profile.tape_crossing_offset) |offset_range| {
            const distance_to_net = -start.x / horizontal_direction.x;
            const crossing_y = start.y + horizontal_direction.y * distance_to_net;
            const selected_tape_offset = self.rng.range(offset_range[0], offset_range[1]);
            const desired_height = netTopHeight(crossing_y) + selected_tape_offset;
            if (profile.landing_depth) |landing_range| {
                const desired_landing = self.rng.range(landing_range[0], landing_range[1]);
                const solution = solutionForLandingDepth(start, horizontal_direction, profile.speed, desired_height, desired_landing);
                launch_speed = solution.speed;
                elevation = solution.elevation;
            } else {
                elevation = elevationForCrossingHeight(start, horizontal_direction, launch_speed, desired_height);
            }
        }
        const velocity = horizontal_direction.scale(launch_speed * @cos(elevation))
            .add(Vec3{ .z = launch_speed * @sin(elevation) });

        // A racket impact leaves the skirt axis nearly backwards. A small perturbation
        // and measured-scale angular rate trigger the observed turnover transient.
        const velocity_direction = velocity.normalized();
        const turnover_axis = velocity_direction.cross(self.rng.unitVector()).normalized();
        const initial_tilt = self.rng.range(profile.initial_tilt_degrees[0], profile.initial_tilt_degrees[1]) * std.math.pi / 180.0;
        const initial_back_axis = Quaternion.fromAxisAngle(turnover_axis, initial_tilt).rotateVector(velocity_direction);
        const initial_orientation = Quaternion.fromTo(.{ .z = 1 }, initial_back_axis);
        const turnover_rate = self.rng.sign() * self.rng.range(profile.turnover_rate[0], profile.turnover_rate[1]);
        const axial_spin_rate = self.rng.sign() * self.rng.range(profile.axial_spin_rate[0], profile.axial_spin_rate[1]);

        self.shuttle = .{
            .position = start,
            .velocity = velocity,
            .orientation = initial_orientation,
            .angular_velocity = turnover_axis.scale(turnover_rate).add(initial_back_axis.scale(axial_spin_rate)),
        };
        self.phase = .flying;
        self.flight_time = 0;
        self.wait_time = 0;
        self.history_clock = 0;
        self.history_len = 0;
        self.current_shot = shot;
        self.net = .{};
        self.physics_rate_hz = base_physics_rate_hz;
        self.appendHistory();
        self.launch_count += 1;
    }

    /// Advances the rigid-body state. Returns true only when a new throw starts.
    pub fn step(self: *Simulation, dt: f32) bool {
        if (self.phase == .waiting) {
            self.net.relax(dt);
            self.wait_time += dt;
            if (self.wait_time >= reset_delay) {
                self.launch();
                return true;
            }
            return false;
        }

        self.flight_time += dt;
        self.physics_rate_hz = base_physics_rate_hz;
        advanceAdaptive(&self.shuttle, &self.net, dt, &self.physics_rate_hz);

        self.history_clock += dt;
        if (self.history_clock >= 1.0 / 60.0) {
            self.history_clock -= 1.0 / 60.0;
            self.appendHistory();
        }

        if (self.shuttle.position.z <= ground_height) {
            self.shuttle.position.z = ground_height;
            self.shuttle.velocity = .{};
            self.phase = .waiting;
            self.wait_time = 0;
            self.appendHistory();
        }
        return false;
    }

    /// Predicts the visible rigid state through the unconsumed fixed-step time.
    /// This keeps slow-motion rendering fluid without altering physical history.
    pub fn predictedShuttle(self: *const Simulation, pending_time: f32) Shuttle {
        var predicted = self.shuttle;
        if (self.phase == .flying and pending_time > 0) {
            var predicted_net = self.net;
            advanceAdaptive(&predicted, &predicted_net, pending_time, null);
            if (predicted.position.z < ground_height) {
                predicted.position.z = ground_height;
                predicted.velocity = .{};
            }
        }
        return predicted;
    }

    pub fn resetRemaining(self: *const Simulation) f32 {
        return if (self.phase == .waiting) @max(0, reset_delay - self.wait_time) else reset_delay;
    }

    fn appendHistory(self: *Simulation) void {
        if (self.history_len < self.history.len) {
            self.history[self.history_len] = .{
                .position = self.shuttle.position,
                .speed = self.shuttle.velocity.length(),
                .time = self.flight_time,
            };
            self.history_len += 1;
        }
    }
};

fn alignmentCosine(shuttle: Shuttle) f32 {
    const back_axis = shuttle.orientation.rotateVector(.{ .z = 1 }).normalized();
    return back_axis.dot(shuttle.velocity.normalized().scale(-1));
}

fn alignmentAngle(shuttle: Shuttle) f32 {
    return std.math.acos(@max(-1, @min(1, alignmentCosine(shuttle))));
}

test "regulation mass and drag decelerate an opposite-side launch" {
    var simulation = Simulation.init(12345);
    const start_x = simulation.shuttle.position.x;
    const initial_speed = simulation.shuttle.velocity.length();
    try std.testing.expectEqual(@as(f32, 0.005), shuttle_mass);
    try std.testing.expect(start_x * simulation.shuttle.velocity.x < 0);
    _ = simulation.step(0.001);
    try std.testing.expect(simulation.shuttle.velocity.length() < initial_speed);
}

test "a launched shuttle crosses the net above tape height" {
    var simulation = Simulation.init(12345);
    const start_x = simulation.shuttle.position.x;
    var crossed = false;
    for (0..5000) |_| {
        _ = simulation.step(0.001);
        if (start_x * simulation.shuttle.position.x <= 0) {
            crossed = true;
            try std.testing.expect(simulation.shuttle.position.z > 1.55);
            break;
        }
        if (simulation.phase == .waiting) break;
    }
    try std.testing.expect(crossed);
}

test "ground wait lasts five seconds and history resets on launch" {
    var simulation = Simulation.init(9981);
    var steps: usize = 0;
    while (simulation.phase == .flying and steps < 20_000) : (steps += 1) _ = simulation.step(1.0 / 1000.0);
    try std.testing.expectEqual(Phase.waiting, simulation.phase);
    const landed_launch = simulation.launch_count;
    _ = simulation.step(4.99);
    try std.testing.expectEqual(landed_launch, simulation.launch_count);
    try std.testing.expect(simulation.step(0.02));
    try std.testing.expectEqual(landed_launch + 1, simulation.launch_count);
    try std.testing.expectEqual(@as(usize, 1), simulation.history_len);
}

test "aerodynamic turnover aligns and keeps a unit quaternion" {
    var simulation = Simulation.init(7782);
    const initial_alignment = alignmentCosine(simulation.shuttle);
    var smallest_angle: f32 = std.math.pi;
    var saw_damped_rebound = false;
    for (0..300) |_| {
        const angle = alignmentAngle(simulation.shuttle);
        if (angle < smallest_angle) smallest_angle = angle;
        if (smallest_angle < 30.0 * std.math.pi / 180.0 and angle > smallest_angle + 3.0 * std.math.pi / 180.0)
            saw_damped_rebound = true;
        _ = simulation.step(1.0 / 1000.0);
    }
    const q = simulation.shuttle.orientation;
    const length = @sqrt(q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w);
    try std.testing.expectApproxEqAbs(@as(f32, 1), length, 0.0001);
    try std.testing.expect(alignmentCosine(simulation.shuttle) > initial_alignment + 0.5);
    try std.testing.expect(saw_damped_rebound);
    try std.testing.expect(alignmentAngle(simulation.shuttle) < 10.0 * std.math.pi / 180.0);
}

test "shot presets reset impact time and history" {
    var simulation = Simulation.init(91827);
    const shots = [_]ShotType{ .smash, .net_roll, .serve, .clear, .drop };
    for (shots) |shot| {
        _ = simulation.step(0.012);
        simulation.launchShot(shot);
        try std.testing.expectEqual(shot, simulation.current_shot);
        try std.testing.expectEqual(@as(f32, 0), simulation.flight_time);
        try std.testing.expectEqual(@as(usize, 1), simulation.history_len);
        try std.testing.expectEqual(@as(f32, 0), simulation.history[0].time);
        try std.testing.expect(simulation.history[0].speed > 0);
        try std.testing.expect(simulation.shuttle.position.x * simulation.shuttle.velocity.x < 0);
    }
}

test "shot presets stay inside the displayed flight envelope" {
    var simulation = Simulation.init(24680);
    const shots = [_]ShotType{ .smash, .net_roll, .serve, .clear, .drop };
    for (shots) |shot| {
        simulation.launchShot(shot);
        const start_x = simulation.shuttle.position.x;
        var crossed_net = false;
        var maximum_height = simulation.shuttle.position.z;
        for (0..5000) |_| {
            _ = simulation.step(1.0 / 1000.0);
            maximum_height = @max(maximum_height, simulation.shuttle.position.z);
            if (!crossed_net and start_x * simulation.shuttle.position.x <= 0) {
                crossed_net = true;
                if (shot != .net_roll)
                    try std.testing.expect(simulation.shuttle.position.z > netTopHeight(simulation.shuttle.position.y));
            }
            if (simulation.phase == .waiting) break;
        }
        if (shot == .net_roll)
            try std.testing.expect(crossed_net or simulation.net.contact_count > 0)
        else
            try std.testing.expect(crossed_net);
        try std.testing.expectEqual(Phase.waiting, simulation.phase);
        try std.testing.expect(maximum_height < 7.5);
        try std.testing.expect(simulation.flight_time < 5.0);
    }
}

test "connected skirt cage and adaptive substeps resolve tape and mesh separately" {
    try std.testing.expectEqual(@as(usize, 48), collision_point_count);
    try std.testing.expectEqual(@as(usize, 80), collision_edge_count);

    var simulation = Simulation.init(6612);
    simulation.shuttle = .{
        .position = .{ .x = 0.150, .z = 1.530 },
        .velocity = .{ .x = -8.0 },
        .orientation = .{},
        .angular_velocity = .{},
    };
    simulation.net = .{};
    simulation.phase = .flying;
    _ = simulation.step(1.0 / 1000.0);
    try std.testing.expectEqual(approach_physics_rate_hz, simulation.physics_rate_hz);
    try std.testing.expectEqual(@as(u32, 0), simulation.net.contact_count);

    simulation.shuttle = .{
        .position = .{ .x = 0.018, .z = 1.530 },
        .velocity = .{ .x = -8.0 },
        .orientation = .{},
        .angular_velocity = .{},
    };
    simulation.net = .{};
    simulation.phase = .flying;
    _ = simulation.step(1.0 / 1000.0);
    try std.testing.expectEqual(contact_physics_rate_hz, simulation.physics_rate_hz);
    try std.testing.expect(simulation.net.contact_count > 0);
    try std.testing.expectEqual(NetMaterial.tape, simulation.net.last_material);
    try std.testing.expect(simulation.net.patch_active);
    var maximum_tape_deflection: f32 = 0;
    for (simulation.net.displacement) |value| maximum_tape_deflection = @max(maximum_tape_deflection, @abs(value));
    try std.testing.expect(maximum_tape_deflection > 0.00001);

    simulation.shuttle = .{
        .position = .{ .x = 0.018, .z = 1.200 },
        .velocity = .{ .x = -8.0 },
        .orientation = .{},
        .angular_velocity = .{},
    };
    simulation.net = .{};
    _ = simulation.step(1.0 / 1000.0);
    try std.testing.expectEqual(contact_physics_rate_hz, simulation.physics_rate_hz);
    try std.testing.expect(simulation.net.contact_count > 0);
    try std.testing.expectEqual(NetMaterial.mesh, simulation.net.last_material);
}

test "mesh holes pass feather points while swept edge catches a cord" {
    const net = NetState{};
    const contact_radius: f32 = 0.0012;
    const mesh_row_z = net_center_height - net_tape_depth - 14.0 * net_mesh_pitch;
    const lower = Vec3{ .x = 0.0005, .y = 0, .z = mesh_row_z - 0.0032 };
    const upper = Vec3{ .x = 0.0005, .y = 0, .z = mesh_row_z + 0.0032 };
    try std.testing.expect(netMaterialAt(lower.y, lower.z, contact_radius) == null);
    try std.testing.expect(netMaterialAt(upper.y, upper.z, contact_radius) == null);

    var contact: ?ContactCandidate = null;
    considerSweptEdge(
        &contact,
        Vec3{ .x = 0.0030, .y = lower.y, .z = lower.z },
        Vec3{ .x = 0.0030, .y = upper.y, .z = upper.z },
        lower,
        upper,
        1.0,
        &net,
    );
    try std.testing.expect(contact != null);
    try std.testing.expectEqual(NetMaterial.mesh, contact.?.material);
}

test "net roll carries substantially more tumble than the short serve" {
    var simulation = Simulation.init(7719);
    simulation.launchShot(.serve);
    const serve_rotation = simulation.shuttle.angular_velocity.length();
    simulation.launchShot(.net_roll);
    const net_spin_rotation = simulation.shuttle.angular_velocity.length();
    try std.testing.expect(net_spin_rotation > serve_rotation * 2.0);
}

test "net roll contacts the tape and splits crossing outcomes" {
    var simulation = Simulation.init(81273);
    var tape_contacts: usize = 0;
    var crossings: usize = 0;
    var rebounds: usize = 0;
    var returned_after_entering: usize = 0;
    var maximum_tape_contact_steps: usize = 0;
    for (0..40) |_| {
        simulation.launchShot(.net_roll);
        const starting_side: f32 = if (simulation.shuttle.position.x >= 0) 1 else -1;
        var touched_tape = false;
        var crossed = false;
        var entered_opponent_side = false;
        var returned = false;
        var tape_contact_steps: usize = 0;
        for (0..1800) |_| {
            _ = simulation.step(1.0 / 1000.0);
            touched_tape = touched_tape or (simulation.net.contact_timer > 0 and simulation.net.last_material == .tape);
            if (simulation.net.contact_timer > 0.1799 and simulation.net.last_material == .tape)
                tape_contact_steps += 1;
            const signed_x = starting_side * simulation.shuttle.position.x;
            if (touched_tape and signed_x < -0.02) entered_opponent_side = true;
            if (entered_opponent_side and signed_x > 0.02) returned = true;
            if (touched_tape and signed_x < -0.12) crossed = true;
            if (simulation.phase == .waiting) break;
        }
        if (!touched_tape) continue;
        tape_contacts += 1;
        if (crossed)
            crossings += 1
        else
            rebounds += 1;
        if (returned) returned_after_entering += 1;
        maximum_tape_contact_steps = @max(maximum_tape_contact_steps, tape_contact_steps);
    }
    try std.testing.expectEqual(@as(usize, 40), tape_contacts);
    try std.testing.expectEqual(tape_contacts, crossings + rebounds);
    try std.testing.expect(crossings >= 17);
    try std.testing.expect(crossings <= 23);
    try std.testing.expectEqual(@as(usize, 0), returned_after_entering);
    try std.testing.expect(maximum_tape_contact_steps < 50);
}

test "net contact stabilization cannot teleport the rigid body" {
    const dt: f32 = 1.0 / 1000.0;
    var simulation = Simulation.init(81273);
    var maximum_solver_displacement: f32 = 0;
    var contacts: usize = 0;

    for (0..4) |_| {
        simulation.launchShot(.net_roll);
        for (0..500) |_| {
            const before = simulation.shuttle;
            const contact_was_active = simulation.net.contact_timer > 0;
            _ = simulation.step(dt);
            if (contact_was_active or simulation.net.contact_timer > 0) {
                const displacement = simulation.shuttle.position.subtract(before.position);
                const average_velocity_motion = before.velocity.add(simulation.shuttle.velocity).scale(0.5 * dt);
                maximum_solver_displacement = @max(
                    maximum_solver_displacement,
                    displacement.subtract(average_velocity_motion).length(),
                );
            }
        }
        if (simulation.net.contact_count > 0) contacts += 1;
    }

    try std.testing.expectEqual(@as(usize, 4), contacts);
    try std.testing.expect(maximum_solver_displacement < 0.004);
}

test "short serve clears the tape and lands in the front service court" {
    var simulation = Simulation.init(55319);
    var shallowest_landing: f32 = 1000;
    var deepest_landing: f32 = 0;
    var net_contacts: usize = 0;
    for (0..20) |_| {
        simulation.launchShot(.serve);
        const starting_side: f32 = if (simulation.shuttle.position.x >= 0) 1 else -1;
        var crossed = false;
        for (0..3000) |_| {
            _ = simulation.step(1.0 / 1000.0);
            if (!crossed and starting_side * simulation.shuttle.position.x <= 0) {
                crossed = true;
                try std.testing.expect(simulation.shuttle.position.z > netTopHeight(simulation.shuttle.position.y));
            }
            if (simulation.phase == .waiting) break;
        }
        try std.testing.expect(crossed);
        if (simulation.net.contact_count > 0) net_contacts += 1;
        const opponent_depth = -starting_side * simulation.shuttle.position.x;
        shallowest_landing = @min(shallowest_landing, opponent_depth);
        deepest_landing = @max(deepest_landing, opponent_depth);
    }
    try std.testing.expect(shallowest_landing > 1.98);
    try std.testing.expect(deepest_landing < 4.0);
    try std.testing.expect(net_contacts <= 2);
}

test "render prediction advances smoothly without mutating physics" {
    const simulation = Simulation.init(4312);
    const original = simulation.shuttle;
    const predicted = simulation.predictedShuttle(0.0005);

    try std.testing.expectEqual(original.position.x, simulation.shuttle.position.x);
    try std.testing.expect(predicted.position.x != original.position.x);
    const q = predicted.orientation;
    const length = @sqrt(q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w);
    try std.testing.expectApproxEqAbs(@as(f32, 1), length, 0.0001);
}
