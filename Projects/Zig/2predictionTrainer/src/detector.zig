const std = @import("std");
const geometry = @import("geometry.zig");
const types = @import("types.zig");

pub const detector_version: u32 = 7;

pub const Result = struct {
    allocator: std.mem.Allocator,
    contacts: []types.ContactEvent,

    pub fn deinit(self: *Result) void {
        self.allocator.free(self.contacts);
        self.* = undefined;
    }
};

const Velocity = struct { x: f64, y: f64 };

const playable_confidence = 0.55;

const Candidate = struct {
    frame: u32,
    timestamp_hns: i64,
    image: types.Vec2,
    impulse: f64,
    spans_gap: bool,
};

const Visit = struct {
    side: types.Side,
    start_frame: u32,
    end_frame: u32,
    terminal: bool,
};

/// Segment the rally by court-side visits, then select exactly one maximum
/// velocity discontinuity from each visit. The velocity on each side of a
/// candidate is extrapolated to the same instant with a quadratic fit. Unlike
/// two independent linear slopes, this removes the apparent "impulse" caused
/// by gravity at a perfectly smooth apex.
///
/// Every non-terminal visit is retained. The labeler deliberately shows both
/// confident and uncertain candidates so a human can determine the threshold.
pub fn detect(
    allocator: std.mem.Allocator,
    points: []const types.TrajectoryPoint,
    frame_rate_num: u32,
    frame_rate_den: u32,
    homography: types.Homography,
) !Result {
    if (points.len == 0) return emptyResult(allocator);

    const fps = @as(f64, @floatFromInt(frame_rate_num)) / @as(f64, @floatFromInt(frame_rate_den));
    const velocity_window: usize = @max(4, @as(usize, @intFromFloat(@round(fps * 0.17))));

    var raw: std.ArrayList(Candidate) = .empty;
    defer raw.deinit(allocator);
    if (points.len > velocity_window * 2 + 1) {
        var point_index: usize = velocity_window;
        while (point_index + velocity_window < points.len) : (point_index += 1) {
            if (!points[point_index].visibility or points[point_index].interpolated) continue;
            const evaluation_time = points[point_index].timestamp_hns;
            const before = fitBoundaryVelocity(
                points[point_index - velocity_window .. point_index],
                evaluation_time,
            );
            const after = fitBoundaryVelocity(
                points[point_index + 1 .. point_index + 1 + velocity_window],
                evaluation_time,
            );
            if (before == null or after == null) continue;
            const dx = after.?.x - before.?.x;
            const dy = after.?.y - before.?.y;
            try raw.append(allocator, .{
                .frame = points[point_index].frame,
                .timestamp_hns = points[point_index].timestamp_hns,
                .image = points[point_index].image,
                .impulse = @sqrt(dx * dx + dy * dy),
                .spans_gap = false,
            });
        }
        try appendGapCandidates(allocator, &raw, points, velocity_window, 12);
        std.mem.sort(Candidate, raw.items, {}, candidateFrameLessThan);
    }

    // A floor homography sends every high shuttle point toward the far end of
    // the court. Classify side on a horizontal plane at a representative
    // racket-contact height instead, otherwise overhead near-side strokes can
    // disappear entirely from the visit sequence.
    const contact_plane = geometry.elevatedPlaneHomography(homography, types.Court.nominal_contact_height_m) catch homography;
    var visits: std.ArrayList(Visit) = .empty;
    defer visits.deinit(allocator);
    try buildVisits(allocator, points, contact_plane, &visits);
    if (visits.items.len == 0) return emptyResult(allocator);

    const statistics = try impulseStatistics(allocator, raw.items);
    var contacts: std.ArrayList(types.ContactEvent) = .empty;
    errdefer contacts.deinit(allocator);

    for (visits.items) |visit| {
        if (strongestCandidate(raw.items, visit.start_frame, visit.end_frame)) |peak| {
            const robust_z = @max(0.0, (peak.impulse - statistics.median) / (1.4826 * statistics.mad));
            // A robust, rally-relative score avoids a codec, zoom, or frame-rate
            // dependent pixel threshold. z=5.35 reaches the playable boundary;
            // smooth flight remains present as an uncertainty barrier.
            const confidence = std.math.clamp((robust_z - 1.5) / 7.0, 0.05, 0.98);
            const classification: types.ContactClass = if (visit.terminal)
                .ground
            else if (confidence >= playable_confidence)
                .racket
            else
                .uncertain;
            try contacts.append(allocator, .{
                .frame = peak.frame,
                .timestamp_hns = peak.timestamp_hns,
                .image = peak.image,
                .side = visit.side,
                .impulse = peak.impulse,
                .confidence = confidence,
                .classification = classification,
                .spans_gap = peak.spans_gap,
            });
        } else if (lastVisibleInRange(points, visit.start_frame, visit.end_frame)) |point| {
            // Very short or heavily occluded visits can lack enough samples for
            // a velocity fit. Preserve their last visible point as uncertain.
            try contacts.append(allocator, .{
                .frame = point.frame,
                .timestamp_hns = point.timestamp_hns,
                .image = point.image,
                .side = visit.side,
                .impulse = 0,
                .confidence = 0.25,
                .classification = if (visit.terminal) .ground else .uncertain,
                .spans_gap = false,
            });
        }
    }

    suppressCompetingVisits(&contacts, fps);
    return .{ .allocator = allocator, .contacts = try contacts.toOwnedSlice(allocator) };
}

const ImpulseStatistics = struct { median: f64, mad: f64 };

fn impulseStatistics(allocator: std.mem.Allocator, candidates: []const Candidate) !ImpulseStatistics {
    if (candidates.len == 0) return .{ .median = 0, .mad = 1 };
    const values = try allocator.alloc(f64, candidates.len);
    defer allocator.free(values);
    for (candidates, 0..) |candidate, index| values[index] = candidate.impulse;
    sortFloats(values);
    const median = percentile(values, 0.5);
    for (candidates, 0..) |candidate, index| values[index] = @abs(candidate.impulse - median);
    sortFloats(values);
    return .{ .median = median, .mad = @max(percentile(values, 0.5), 1.0) };
}

fn buildVisits(
    allocator: std.mem.Allocator,
    points: []const types.TrajectoryPoint,
    homography: types.Homography,
    visits: *std.ArrayList(Visit),
) !void {
    // Airborne points projected through a floor homography can wobble near the
    // net. Requiring two firm observations beyond a 15 cm dead band prevents
    // that perspective noise from splitting a real visit.
    const net_margin_m = 0.15;
    const stable_observations = 2;

    var current_side: types.Side = .unknown;
    var visit_start: u32 = 0;
    var pending_side: types.Side = .unknown;
    var pending_start: u32 = 0;
    var pending_count: u8 = 0;
    var last_visible_frame: ?u32 = null;

    for (points) |point| {
        if (!point.visibility) continue;
        last_visible_frame = point.frame;
        const court = homography.project(point.image) orelse continue;
        const observed_side = firmSide(court.y, net_margin_m) orelse continue;

        if (current_side == .unknown) {
            current_side = observed_side;
            visit_start = point.frame;
            continue;
        }
        if (observed_side == current_side) {
            pending_side = .unknown;
            pending_count = 0;
            continue;
        }

        if (pending_side != observed_side) {
            pending_side = observed_side;
            pending_start = point.frame;
            pending_count = 1;
        } else {
            pending_count += 1;
        }
        if (pending_count < stable_observations) continue;

        if (pending_start > visit_start) {
            try visits.append(allocator, .{
                .side = current_side,
                .start_frame = visit_start,
                .end_frame = pending_start - 1,
                .terminal = false,
            });
        }
        current_side = pending_side;
        visit_start = pending_start;
        pending_side = .unknown;
        pending_count = 0;
    }

    if (current_side != .unknown and last_visible_frame != null and last_visible_frame.? >= visit_start) {
        try visits.append(allocator, .{
            .side = current_side,
            .start_frame = visit_start,
            .end_frame = last_visible_frame.?,
            .terminal = true,
        });
    }
}

fn firmSide(court_y: f64, margin: f64) ?types.Side {
    if (court_y < types.Court.half_length_m - margin) return .far;
    if (court_y > types.Court.half_length_m + margin) return .near;
    return null;
}

fn strongestCandidate(candidates: []const Candidate, start_frame: u32, end_frame: u32) ?Candidate {
    var strongest: ?Candidate = null;
    for (candidates) |candidate| {
        if (candidate.frame < start_frame) continue;
        if (candidate.frame > end_frame) break;
        if (strongest == null or candidate.impulse > strongest.?.impulse) strongest = candidate;
    }
    return strongest;
}

fn lastVisibleInRange(points: []const types.TrajectoryPoint, start_frame: u32, end_frame: u32) ?types.TrajectoryPoint {
    var index = points.len;
    while (index > 0) {
        index -= 1;
        const point = points[index];
        if (point.frame > end_frame) continue;
        if (point.frame < start_frame) return null;
        if (point.visibility) return point;
    }
    return null;
}

fn emptyResult(allocator: std.mem.Allocator) !Result {
    return .{ .allocator = allocator, .contacts = try allocator.alloc(types.ContactEvent, 0) };
}

fn appendGapCandidates(
    allocator: std.mem.Allocator,
    raw: *std.ArrayList(Candidate),
    points: []const types.TrajectoryPoint,
    window: usize,
    max_gap: usize,
) !void {
    var i: usize = 1;
    while (i + 1 < points.len) {
        if (points[i].visibility and !points[i].interpolated) {
            i += 1;
            continue;
        }
        const gap_start = i;
        while (i < points.len and (!points[i].visibility or points[i].interpolated)) : (i += 1) {}
        if (i >= points.len) break;
        const gap_len = i - gap_start;
        if (gap_len > max_gap or gap_start < window or i + window > points.len) continue;
        const midpoint = gap_start + gap_len / 2;
        const evaluation_time = points[midpoint].timestamp_hns;
        const before = fitBoundaryVelocity(points[gap_start - window .. gap_start], evaluation_time);
        const after = fitBoundaryVelocity(points[i .. i + window], evaluation_time);
        if (before == null or after == null) continue;
        const dx = after.?.x - before.?.x;
        const dy = after.?.y - before.?.y;
        const t = @as(f64, @floatFromInt(gap_len / 2 + 1)) / @as(f64, @floatFromInt(gap_len + 1));
        const p0 = points[gap_start - 1].image;
        const p1 = points[i].image;
        try raw.append(allocator, .{
            .frame = @intCast(midpoint),
            .timestamp_hns = points[midpoint].timestamp_hns,
            .image = .{ .x = p0.x + (p1.x - p0.x) * t, .y = p0.y + (p1.y - p0.y) * t },
            .impulse = @sqrt(dx * dx + dy * dy),
            .spans_gap = true,
        });
    }
}

fn fitBoundaryVelocity(points: []const types.TrajectoryPoint, evaluation_time_hns: i64) ?Velocity {
    var count: f64 = 0;
    var sum_t: f64 = 0;
    var sum_t2: f64 = 0;
    var sum_t3: f64 = 0;
    var sum_t4: f64 = 0;
    var sum_x: f64 = 0;
    var sum_tx: f64 = 0;
    var sum_t2x: f64 = 0;
    var sum_y: f64 = 0;
    var sum_ty: f64 = 0;
    var sum_t2y: f64 = 0;
    for (points) |point| {
        if (!point.visibility or point.interpolated) continue;
        const t = @as(f64, @floatFromInt(point.timestamp_hns - evaluation_time_hns)) / 10_000_000.0;
        const t2 = t * t;
        count += 1;
        sum_t += t;
        sum_t2 += t2;
        sum_t3 += t2 * t;
        sum_t4 += t2 * t2;
        sum_x += point.image.x;
        sum_tx += t * point.image.x;
        sum_t2x += t2 * point.image.x;
        sum_y += point.image.y;
        sum_ty += t * point.image.y;
        sum_t2y += t2 * point.image.y;
    }
    if (count < 4) return null;

    const matrix = [3][3]f64{
        .{ count, sum_t, sum_t2 },
        .{ sum_t, sum_t2, sum_t3 },
        .{ sum_t2, sum_t3, sum_t4 },
    };
    const fit_x = solveThreeByThree(matrix, .{ sum_x, sum_tx, sum_t2x }) orelse return null;
    const fit_y = solveThreeByThree(matrix, .{ sum_y, sum_ty, sum_t2y }) orelse return null;
    return .{
        // Coefficient one is the derivative of c + b*t + a*t^2 at t=0.
        .x = fit_x[1],
        .y = fit_y[1],
    };
}

fn solveThreeByThree(input: [3][3]f64, rhs_input: [3]f64) ?[3]f64 {
    var matrix = input;
    var rhs = rhs_input;
    var column: usize = 0;
    while (column < 3) : (column += 1) {
        var pivot = column;
        var row = column + 1;
        while (row < 3) : (row += 1) {
            if (@abs(matrix[row][column]) > @abs(matrix[pivot][column])) pivot = row;
        }
        if (@abs(matrix[pivot][column]) < 1e-12) return null;
        if (pivot != column) {
            std.mem.swap([3]f64, &matrix[pivot], &matrix[column]);
            std.mem.swap(f64, &rhs[pivot], &rhs[column]);
        }

        row = column + 1;
        while (row < 3) : (row += 1) {
            const factor = matrix[row][column] / matrix[column][column];
            var inner = column;
            while (inner < 3) : (inner += 1) matrix[row][inner] -= factor * matrix[column][inner];
            rhs[row] -= factor * rhs[column];
        }
    }

    var result: [3]f64 = undefined;
    var reverse: usize = 3;
    while (reverse > 0) {
        reverse -= 1;
        var value = rhs[reverse];
        var inner = reverse + 1;
        while (inner < 3) : (inner += 1) value -= matrix[reverse][inner] * result[inner];
        result[reverse] = value / matrix[reverse][reverse];
    }
    return result;
}

fn suppressCompetingVisits(contacts: *std.ArrayList(types.ContactEvent), fps: f64) void {
    if (contacts.items.len < 2) return;
    const separation_frames: u32 = @max(1, @as(u32, @intFromFloat(@round(fps * 0.15))));
    var write: usize = 0;
    for (contacts.items) |contact| {
        if (write > 0 and contact.classification != .ground and
            contacts.items[write - 1].classification != .ground and
            contact.frame - contacts.items[write - 1].frame <= separation_frames)
        {
            if (contact.impulse > contacts.items[write - 1].impulse) contacts.items[write - 1] = contact;
            continue;
        }
        contacts.items[write] = contact;
        write += 1;
    }
    contacts.shrinkRetainingCapacity(write);
}

fn candidateFrameLessThan(_: void, a: Candidate, b: Candidate) bool {
    if (a.frame != b.frame) return a.frame < b.frame;
    return a.impulse > b.impulse;
}

fn sortFloats(values: []f64) void {
    var i: usize = 1;
    while (i < values.len) : (i += 1) {
        const value = values[i];
        var j = i;
        while (j > 0 and values[j - 1] > value) : (j -= 1) values[j] = values[j - 1];
        values[j] = value;
    }
}

fn percentile(sorted: []const f64, fraction: f64) f64 {
    if (sorted.len == 1) return sorted[0];
    const position = fraction * @as(f64, @floatFromInt(sorted.len - 1));
    const low: usize = @intFromFloat(@floor(position));
    const high: usize = @min(low + 1, sorted.len - 1);
    const t = position - @as(f64, @floatFromInt(low));
    return sorted[low] + (sorted[high] - sorted[low]) * t;
}
