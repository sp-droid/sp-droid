const std = @import("std");
const calibration = @import("calibration.zig");
const dataset = @import("dataset.zig");
const detector = @import("detector.zig");
const geometry = @import("geometry.zig");
const stroke_index = @import("index.zig");
const stroke_labels = @import("stroke_labels.zig");
const types = @import("types.zig");
const video = @import("video.zig");

test "four point homography maps calibration corners" {
    const image = [4]types.Vec2{
        .{ .x = 100, .y = 80 },   .{ .x = 1100, .y = 80 },
        .{ .x = 1200, .y = 680 }, .{ .x = 30, .y = 680 },
    };
    const court = [4]types.Vec2{
        .{ .x = 0, .y = 0 },      .{ .x = 6.1, .y = 0 },
        .{ .x = 6.1, .y = 13.4 }, .{ .x = 0, .y = 13.4 },
    };
    const h = try geometry.solveHomography(image, court);
    for (image, court) |source, expected| {
        const actual = h.project(source).?;
        try std.testing.expectApproxEqAbs(expected.x, actual.x, 1e-8);
        try std.testing.expectApproxEqAbs(expected.y, actual.y, 1e-8);
    }
}

test "CSV validation and ordinary short-gap interpolation" {
    const csv =
        "Frame,Visibility,X,Y\n" ++
        "0,1,10,20\n" ++
        "1,0,0,0\n" ++
        "2,0,0,0\n" ++
        "3,1,40,50\n";
    var trajectory = try dataset.parseTrajectory(std.testing.allocator, csv, 25, 1);
    defer trajectory.deinit();
    try std.testing.expect(trajectory.points[1].visibility);
    try std.testing.expect(trajectory.points[1].interpolated);
    try std.testing.expectApproxEqAbs(@as(f64, 20), trajectory.points[1].image.x, 1e-9);
    try std.testing.expectApproxEqAbs(@as(f64, 40), trajectory.points[2].image.y, 1e-9);
    try std.testing.expectEqual(@as(i64, 400_000), trajectory.points[1].timestamp_hns);
}

test "CSV rejects non-contiguous frames and invalid invisible coordinates" {
    const skipped = "Frame,Visibility,X,Y\n0,1,10,20\n2,1,30,40\n";
    try std.testing.expectError(
        error.NonContiguousFrames,
        dataset.parseTrajectory(std.testing.allocator, skipped, 30, 1),
    );
    const invalid_hidden = "Frame,Visibility,X,Y\n0,0,1,0\n";
    try std.testing.expectError(
        error.InvisibleCoordinatesNotZero,
        dataset.parseTrajectory(std.testing.allocator, invalid_hidden, 30, 1),
    );
}

test "contact-spanning gaps remain uninterpolated" {
    var points: [15]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| point.* = .{
        .frame = @intCast(i),
        .timestamp_hns = @intCast(i * 333_333),
        .visibility = i == 0 or i >= 13,
        .image = if (i == 0)
            .{ .x = 5, .y = 5 }
        else if (i >= 13)
            .{ .x = 50, .y = 50 }
        else
            .{ .x = 0, .y = 0 },
    };
    dataset.interpolateShortGaps(&points, 3);
    try std.testing.expect(!points[6].visibility);
}

test "detector does not invent contacts in constant velocity flight" {
    var points: [90]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| point.* = .{
        .frame = @intCast(i),
        .timestamp_hns = @intCast(i * 333_333),
        .visibility = true,
        .image = .{ .x = @floatFromInt(i * 3), .y = @floatFromInt(100 + i) },
    };
    var result = try detector.detect(std.testing.allocator, &points, 30, 1, .identity);
    defer result.deinit();
    try std.testing.expectEqual(@as(usize, 1), result.contacts.len);
    try std.testing.expectEqual(types.ContactClass.ground, result.contacts[0].classification);
}

test "smooth side visit remains an uncertainty candidate" {
    var points: [120]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| point.* = .{
        .frame = @intCast(i),
        .timestamp_hns = @intCast(i * 333_333),
        .visibility = true,
        .image = .{
            .x = 3.05,
            .y = 12.0 - @as(f64, @floatFromInt(i)) * 0.08,
        },
    };
    var result = try detector.detect(std.testing.allocator, &points, 30, 1, .identity);
    defer result.deinit();
    try std.testing.expectEqual(@as(usize, 2), result.contacts.len);
    try std.testing.expectEqual(types.ContactClass.uncertain, result.contacts[0].classification);
    try std.testing.expectEqual(types.ContactClass.ground, result.contacts[1].classification);
}

test "side visits select one discontinuity per crossing window" {
    var points: [120]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| {
        const y: f64 = if (i < 12)
            11.0 - @as(f64, @floatFromInt(i)) * 0.15
        else if (i < 42)
            9.2 - @as(f64, @floatFromInt(i - 12)) * 0.19
        else if (i < 72)
            3.69 + @as(f64, @floatFromInt(i - 42)) * 0.20
        else
            9.49 + @as(f64, @floatFromInt(i - 72)) * 0.02;
        const x: f64 = if (i < 12)
            @floatFromInt(i)
        else if (i < 42)
            12.0 - @as(f64, @floatFromInt(i - 12)) * 3.0
        else if (i < 72)
            -78.0 + @as(f64, @floatFromInt(i - 42)) * 4.0
        else
            42.0 + @as(f64, @floatFromInt(i - 72));
        point.* = .{
            .frame = @intCast(i),
            .timestamp_hns = @intCast(i * 333_333),
            .visibility = true,
            .image = .{ .x = x, .y = y },
        };
    }
    var result = try detector.detect(std.testing.allocator, &points, 30, 1, .identity);
    defer result.deinit();
    try std.testing.expectEqual(@as(usize, 3), result.contacts.len);
    try std.testing.expectEqual(types.ContactClass.racket, result.contacts[0].classification);
    try std.testing.expectEqual(types.ContactClass.racket, result.contacts[1].classification);
    try std.testing.expectEqual(types.ContactClass.ground, result.contacts[2].classification);
    try std.testing.expect(result.contacts[0].frame < result.contacts[1].frame);
    try std.testing.expect(result.contacts[1].frame < result.contacts[2].frame);
}

test "quadratic apex does not become a racket contact" {
    var points: [121]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| {
        const offset = @as(f64, @floatFromInt(i)) - 60.0;
        point.* = .{
            .frame = @intCast(i),
            .timestamp_hns = @intCast(i * 333_333),
            .visibility = true,
            .image = .{
                .x = @as(f64, @floatFromInt(i)) * 0.2,
                .y = 3.5 + offset * offset * 0.0025,
            },
        };
    }
    var result = try detector.detect(std.testing.allocator, &points, 30, 1, .identity);
    defer result.deinit();
    try std.testing.expect(result.contacts.len >= 2);
    for (result.contacts[0 .. result.contacts.len - 1]) |contact| {
        try std.testing.expectEqual(types.ContactClass.uncertain, contact.classification);
    }
    try std.testing.expectEqual(types.ContactClass.ground, result.contacts[result.contacts.len - 1].classification);
}

test "velocity discontinuity hidden by a medium gap remains detectable" {
    var points: [80]types.TrajectoryPoint = undefined;
    for (&points, 0..) |*point, i| {
        const hidden = i >= 30 and i < 35;
        const after = i >= 35;
        point.* = .{
            .frame = @intCast(i),
            .timestamp_hns = @intCast(i * 333_333),
            .visibility = !hidden,
            .image = if (hidden)
                .{ .x = 0, .y = 0 }
            else
                .{
                    .x = if (after)
                        30.0 - @as(f64, @floatFromInt(i - 35)) * 4.0
                    else
                        @floatFromInt(i),
                    .y = if (after)
                        9.4 - @as(f64, @floatFromInt(i - 35)) * 0.2
                    else
                        10.0 - @as(f64, @floatFromInt(i)) * 0.02,
                },
        };
    }
    var result = try detector.detect(std.testing.allocator, &points, 30, 1, .identity);
    defer result.deinit();
    try std.testing.expectEqual(types.ContactClass.racket, result.contacts[0].classification);
    try std.testing.expect(result.contacts[0].spans_gap);
}

test "video timestamps preserve all observed fractional rates" {
    const rates = [_][2]u32{ .{ 25, 1 }, .{ 29, 1 }, .{ 30, 1 }, .{ 30_000, 1001 } };
    for (rates) |rate| {
        var frame: u32 = 0;
        while (frame < 1000) : (frame += 1) {
            const timestamp = video.frameTimestamp(frame, rate[0], rate[1]);
            try std.testing.expectEqual(frame, video.timestampFrame(timestamp, rate[0], rate[1]));
        }
    }
}

test "elevated homography round-trips contact coordinates" {
    const entry = try calibration.fromCorners(1, null, .{
        .{ .x = 457, .y = 184 },  .{ .x = 823, .y = 184 },
        .{ .x = 1135, .y = 704 }, .{ .x = 145, .y = 704 },
    });
    const image_to_contact = try geometry.elevatedPlaneHomography(
        entry.homography,
        types.Court.nominal_contact_height_m,
    );
    const contact_to_image = try geometry.invertHomography(image_to_contact);
    const expected = types.Vec2{ .x = 4.2, .y = 10.8 };
    const image = contact_to_image.project(expected).?;
    const actual = image_to_contact.project(image).?;
    try std.testing.expectApproxEqAbs(expected.x, actual.x, 1e-8);
    try std.testing.expectApproxEqAbs(expected.y, actual.y, 1e-8);
}

test "candidate count excludes ground and frame zero" {
    var contacts = try std.testing.allocator.alloc(types.ContactEvent, 3);
    contacts[0] = contactAt(0, .uncertain);
    contacts[1] = contactAt(10, .racket);
    contacts[2] = contactAt(20, .ground);
    var index = stroke_index.StrokeIndex{ .allocator = std.testing.allocator };
    defer index.deinit();
    try index.rallies.append(std.testing.allocator, .{
        .asset_index = 0,
        .contacts = contacts,
    });
    try std.testing.expectEqual(@as(usize, 1), index.candidateCount());
}

test "stroke labels are exact and independently updateable" {
    var store = stroke_labels.Store{ .allocator = std.testing.allocator };
    defer store.deinit();
    const key = stroke_labels.Key{
        .match_id = 4,
        .stem = "1_07_03",
        .contact_frame = 91,
    };
    try std.testing.expect(!store.contains(key));
    try store.put(key, 90, detector.detector_version, .correct);
    try std.testing.expect(store.contains(key));
    try std.testing.expectEqual(stroke_labels.Verdict.correct, store.entries.items[0].verdict);
    store.entries.items[0].presentation_version = 1;
    try std.testing.expect(!store.contains(key));
    try store.put(key, 90, detector.detector_version, .wrong);
    try std.testing.expect(store.contains(key));
    try std.testing.expectEqual(@as(usize, 1), store.entries.items.len);
    try std.testing.expectEqual(stroke_labels.Verdict.wrong, store.entries.items[0].verdict);

    var different_frame = key;
    different_frame.contact_frame += 1;
    try std.testing.expect(!store.contains(different_frame));
}

fn contactAt(frame: u32, classification: types.ContactClass) types.ContactEvent {
    return .{
        .frame = frame,
        .timestamp_hns = @as(i64, frame) * 333_333,
        .image = .{ .x = 640, .y = 360 },
        .side = .near,
        .impulse = 1,
        .confidence = 0.5,
        .classification = classification,
    };
}
