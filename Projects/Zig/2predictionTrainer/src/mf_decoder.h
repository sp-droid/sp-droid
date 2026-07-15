#ifndef BADMINTON_MF_DECODER_H
#define BADMINTON_MF_DECODER_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct BstMfDecoder BstMfDecoder;

typedef struct BstMfVideoInfo {
    uint32_t width;
    uint32_t height;
    uint32_t frame_rate_num;
    uint32_t frame_rate_den;
    uint32_t native_subtype_fourcc;
} BstMfVideoInfo;

typedef struct BstMfFrameInfo {
    int64_t timestamp_hns;
    int64_t duration_hns;
    uint32_t source_reader_flags;
} BstMfFrameInfo;

// The decoder must be created, used, and destroyed on the same worker thread.
BstMfDecoder *bst_mf_open_utf8(const char *path_utf8, BstMfVideoInfo *out_info, int32_t *out_hresult);
int32_t bst_mf_seek(BstMfDecoder *decoder, int64_t timestamp_hns);
// Returns 1 for a frame, 0 for EOS, or a negative HRESULT on failure.
int32_t bst_mf_read_nv12(
    BstMfDecoder *decoder,
    uint8_t *y_plane,
    uint32_t y_capacity,
    uint8_t *uv_plane,
    uint32_t uv_capacity,
    BstMfFrameInfo *out_frame);
void bst_mf_close(BstMfDecoder *decoder);

#ifdef __cplusplus
}
#endif

#endif

