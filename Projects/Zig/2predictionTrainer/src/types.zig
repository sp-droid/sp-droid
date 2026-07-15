pub const Vec2 = struct {
    x: f64,
    y: f64,
};

pub const Side = enum(i8) {
    near = -1,
    unknown = 0,
    far = 1,
};

pub const ContactClass = enum {
    racket,
    ground,
    uncertain,
};

pub const Codec = enum {
    mp4v,
    h264,
    unknown,
};

pub const RallyAsset = struct {
    match_id: u8,
    stem: []const u8,
    csv_path: []const u8,
    video_path: []const u8,
    codec: Codec = .unknown,
    width: u32 = 0,
    height: u32 = 0,
    frame_rate_num: u32 = 30,
    frame_rate_den: u32 = 1,
    annotated_frame_count: u32 = 0,
    decoded_frame_count: u32 = 0,

    pub fn fps(self: RallyAsset) f64 {
        return @as(f64, @floatFromInt(self.frame_rate_num)) /
            @as(f64, @floatFromInt(self.frame_rate_den));
    }
};

pub const TrajectoryPoint = struct {
    frame: u32,
    timestamp_hns: i64,
    visibility: bool,
    image: Vec2,
    interpolated: bool = false,
};

pub const ContactEvent = struct {
    frame: u32,
    timestamp_hns: i64,
    image: Vec2,
    side: Side,
    impulse: f64,
    confidence: f64,
    classification: ContactClass,
    spans_gap: bool = false,
};

pub const Homography = struct {
    // Row-major 3x3 projective transform from image pixels to court metres.
    m: [9]f64,

    pub const identity = Homography{ .m = .{ 1, 0, 0, 0, 1, 0, 0, 0, 1 } };

    pub fn project(self: Homography, p: Vec2) ?Vec2 {
        const w = self.m[6] * p.x + self.m[7] * p.y + self.m[8];
        if (@abs(w) < 1e-9) return null;
        return .{
            .x = (self.m[0] * p.x + self.m[1] * p.y + self.m[2]) / w,
            .y = (self.m[3] * p.x + self.m[4] * p.y + self.m[5]) / w,
        };
    }
};

pub const CourtCalibration = struct {
    match_id: u8,
    rally_stem: ?[]const u8 = null,
    image_width: u32,
    image_height: u32,
    homography: Homography,
    broadcast_near_is_bottom: bool = true,
    reprojection_error_px: f64 = 0,
};

pub const Court = struct {
    pub const doubles_width_m = 6.10;
    pub const length_m = 13.40;
    pub const half_length_m = length_m / 2.0;
    pub const nominal_contact_height_m = 1.5;
};
