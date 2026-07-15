const std = @import("std");

const c = @cImport({
    @cInclude("mf_decoder.h");
});

pub const VideoInfo = struct {
    width: u32,
    height: u32,
    frame_rate_num: u32,
    frame_rate_den: u32,
    native_subtype_fourcc: u32,
};

pub const FrameInfo = struct {
    timestamp_hns: i64,
    duration_hns: i64,
    flags: u32,
};

pub const Decoder = struct {
    handle: *c.BstMfDecoder,
    info: VideoInfo,

    pub fn open(path_z: [:0]const u8) !Decoder {
        var raw_info: c.BstMfVideoInfo = undefined;
        var result: i32 = 0;
        const handle = c.bst_mf_open_utf8(path_z.ptr, &raw_info, &result) orelse {
            std.log.err("Media Foundation could not open video (HRESULT 0x{x:0>8})", .{@as(u32, @bitCast(result))});
            return error.MediaFoundationOpenFailed;
        };
        return .{
            .handle = handle,
            .info = .{
                .width = raw_info.width,
                .height = raw_info.height,
                .frame_rate_num = raw_info.frame_rate_num,
                .frame_rate_den = raw_info.frame_rate_den,
                .native_subtype_fourcc = raw_info.native_subtype_fourcc,
            },
        };
    }

    pub fn close(self: *Decoder) void {
        c.bst_mf_close(self.handle);
        self.* = undefined;
    }

    pub fn seek(self: *Decoder, timestamp_hns: i64) !void {
        const result = c.bst_mf_seek(self.handle, @max(timestamp_hns, 0));
        if (result < 0) {
            std.log.err("Media Foundation seek failed (HRESULT 0x{x:0>8})", .{@as(u32, @bitCast(result))});
            return error.MediaFoundationSeekFailed;
        }
    }

    pub fn read(self: *Decoder, y: []u8, uv: []u8) !?FrameInfo {
        var raw_frame: c.BstMfFrameInfo = undefined;
        const result = c.bst_mf_read_nv12(
            self.handle,
            y.ptr,
            @intCast(y.len),
            uv.ptr,
            @intCast(uv.len),
            &raw_frame,
        );
        if (result == 0) return null;
        if (result < 0) {
            std.log.err("Media Foundation decode failed (HRESULT 0x{x:0>8})", .{@as(u32, @bitCast(result))});
            return error.MediaFoundationDecodeFailed;
        }
        return .{
            .timestamp_hns = raw_frame.timestamp_hns,
            .duration_hns = raw_frame.duration_hns,
            .flags = raw_frame.source_reader_flags,
        };
    }
};
