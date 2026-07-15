const std = @import("std");
const types = @import("types.zig");

pub fn invertHomography(h: types.Homography) !types.Homography {
    const m = h.m;
    const c00 = m[4] * m[8] - m[5] * m[7];
    const c01 = -(m[3] * m[8] - m[5] * m[6]);
    const c02 = m[3] * m[7] - m[4] * m[6];
    const determinant = m[0] * c00 + m[1] * c01 + m[2] * c02;
    if (@abs(determinant) < 1e-12) return error.DegenerateCalibration;
    const inverse = 1.0 / determinant;
    return .{ .m = .{
        c00 * inverse,
        -(m[1] * m[8] - m[2] * m[7]) * inverse,
        (m[1] * m[5] - m[2] * m[4]) * inverse,
        c01 * inverse,
        (m[0] * m[8] - m[2] * m[6]) * inverse,
        -(m[0] * m[5] - m[2] * m[3]) * inverse,
        c02 * inverse,
        -(m[0] * m[7] - m[1] * m[6]) * inverse,
        (m[0] * m[4] - m[1] * m[3]) * inverse,
    } };
}

const Vec3 = struct { x: f64, y: f64, z: f64 };

/// Derive an image-to-court homography for a plane parallel to the floor at
/// `height_m`. Broadcast footage only calibrates the floor, but racket contacts
/// are airborne; using this plane prevents overhead near-side contacts from
/// being projected into the far court.
pub fn elevatedPlaneHomography(image_to_ground: types.Homography, height_m: f64) !types.Homography {
    var ground_to_image = try invertHomography(image_to_ground);
    const normalization = ground_to_image.m[8];
    if (@abs(normalization) < 1e-9) return error.InvalidCameraCalibration;
    for (&ground_to_image.m) |*coefficient| coefficient.* /= normalization;

    const center_x = 640.0;
    const center_y = 360.0;
    const h1 = Vec3{ .x = ground_to_image.m[0], .y = ground_to_image.m[3], .z = ground_to_image.m[6] };
    const h2 = Vec3{ .x = ground_to_image.m[1], .y = ground_to_image.m[4], .z = ground_to_image.m[7] };
    const a1 = h1.x - center_x * h1.z;
    const b1 = h1.y - center_y * h1.z;
    const a2 = h2.x - center_x * h2.z;
    const b2 = h2.y - center_y * h2.z;
    const constraint_a = [2]f64{
        a1 * a2 + b1 * b2,
        a1 * a1 + b1 * b1 - a2 * a2 - b2 * b2,
    };
    const constraint_b = [2]f64{
        h1.z * h2.z,
        h1.z * h1.z - h2.z * h2.z,
    };
    const denominator = constraint_a[0] * constraint_a[0] + constraint_a[1] * constraint_a[1];
    if (denominator < 1e-18) return error.InvalidCameraCalibration;
    const inverse_focal_squared = -(constraint_a[0] * constraint_b[0] +
        constraint_a[1] * constraint_b[1]) / denominator;
    if (!(inverse_focal_squared > 0)) return error.InvalidCameraCalibration;
    const focal_length = @sqrt(1.0 / inverse_focal_squared);
    if (focal_length < 300 or focal_length > 10_000) return error.InvalidCameraCalibration;

    const q1 = inverseIntrinsic(h1, focal_length, center_x, center_y);
    const q2 = inverseIntrinsic(h2, focal_length, center_x, center_y);
    const q1_length = length3(q1);
    const q2_length = length3(q2);
    if (q1_length < 1e-12 or q2_length < 1e-12) return error.InvalidCameraCalibration;
    const camera_scale = 2.0 / (q1_length + q2_length);
    const r1 = scale3(q1, 1.0 / q1_length);
    const q2_orthogonal = subtract3(q2, scale3(r1, dot3(q2, r1)));
    const q2_orthogonal_length = length3(q2_orthogonal);
    if (q2_orthogonal_length < 1e-12) return error.InvalidCameraCalibration;
    const r2 = scale3(q2_orthogonal, 1.0 / q2_orthogonal_length);
    const r3 = scale3(cross3(r1, r2), -1.0);
    const vertical_image = scale3(applyIntrinsic(r3, focal_length, center_x, center_y), 1.0 / camera_scale);

    var elevated_ground_to_image = ground_to_image;
    elevated_ground_to_image.m[2] += vertical_image.x * height_m;
    elevated_ground_to_image.m[5] += vertical_image.y * height_m;
    elevated_ground_to_image.m[8] += vertical_image.z * height_m;
    return invertHomography(elevated_ground_to_image);
}

fn inverseIntrinsic(value: Vec3, focal_length: f64, center_x: f64, center_y: f64) Vec3 {
    return .{
        .x = (value.x - center_x * value.z) / focal_length,
        .y = (value.y - center_y * value.z) / focal_length,
        .z = value.z,
    };
}

fn applyIntrinsic(value: Vec3, focal_length: f64, center_x: f64, center_y: f64) Vec3 {
    return .{
        .x = focal_length * value.x + center_x * value.z,
        .y = focal_length * value.y + center_y * value.z,
        .z = value.z,
    };
}

fn dot3(a: Vec3, b: Vec3) f64 {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

fn cross3(a: Vec3, b: Vec3) Vec3 {
    return .{
        .x = a.y * b.z - a.z * b.y,
        .y = a.z * b.x - a.x * b.z,
        .z = a.x * b.y - a.y * b.x,
    };
}

fn subtract3(a: Vec3, b: Vec3) Vec3 {
    return .{ .x = a.x - b.x, .y = a.y - b.y, .z = a.z - b.z };
}

fn scale3(value: Vec3, scale: f64) Vec3 {
    return .{ .x = value.x * scale, .y = value.y * scale, .z = value.z * scale };
}

fn length3(value: Vec3) f64 {
    return @sqrt(dot3(value, value));
}

pub fn solveHomography(image: [4]types.Vec2, court: [4]types.Vec2) !types.Homography {
    // Solve the eight free coefficients with h[8] fixed to one.
    var a: [8][9]f64 = undefined;
    for (image, court, 0..) |src, dst, i| {
        a[i * 2] = .{ src.x, src.y, 1, 0, 0, 0, -dst.x * src.x, -dst.x * src.y, dst.x };
        a[i * 2 + 1] = .{ 0, 0, 0, src.x, src.y, 1, -dst.y * src.x, -dst.y * src.y, dst.y };
    }

    var pivot: usize = 0;
    while (pivot < 8) : (pivot += 1) {
        var best = pivot;
        var row = pivot + 1;
        while (row < 8) : (row += 1) {
            if (@abs(a[row][pivot]) > @abs(a[best][pivot])) best = row;
        }
        if (@abs(a[best][pivot]) < 1e-10) return error.DegenerateCalibration;
        if (best != pivot) std.mem.swap([9]f64, &a[best], &a[pivot]);

        const divisor = a[pivot][pivot];
        var col: usize = pivot;
        while (col < 9) : (col += 1) a[pivot][col] /= divisor;

        row = 0;
        while (row < 8) : (row += 1) {
            if (row == pivot) continue;
            const factor = a[row][pivot];
            col = pivot;
            while (col < 9) : (col += 1) a[row][col] -= factor * a[pivot][col];
        }
    }

    return .{ .m = .{
        a[0][8], a[1][8], a[2][8],
        a[3][8], a[4][8], a[5][8],
        a[6][8], a[7][8], 1,
    } };
}
