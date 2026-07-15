#define COBJMACROS
#include "mf_decoder.h"

#include <windows.h>
#include <mfapi.h>
#include <mferror.h>
#include <mfidl.h>
#include <mfreadwrite.h>
#include <propvarutil.h>
#include <stdlib.h>
#include <string.h>

struct BstMfDecoder {
    IMFSourceReader *reader;
    uint32_t width;
    uint32_t height;
    LONG stride;
    int com_initialized;
    int mf_initialized;
};

static int failed_i32(HRESULT hr) {
    return (int32_t)hr;
}

static HRESULT refresh_video_type(BstMfDecoder *decoder, BstMfVideoInfo *out_info) {
    IMFMediaType *type = NULL;
    HRESULT hr = IMFSourceReader_GetCurrentMediaType(
        decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM, &type);
    if (FAILED(hr)) return hr;

    UINT32 width = 0, height = 0, rate_num = 0, rate_den = 0;
    UINT64 packed_size = 0;
    hr = IMFMediaType_GetUINT64(type, &MF_MT_FRAME_SIZE, &packed_size);
    if (SUCCEEDED(hr)) {
        width = (UINT32)(packed_size >> 32);
        height = (UINT32)(packed_size & 0xffffffffu);
        UINT64 packed_rate = 0;
        HRESULT rate_hr = IMFMediaType_GetUINT64(type, &MF_MT_FRAME_RATE, &packed_rate);
        if (SUCCEEDED(rate_hr)) {
            rate_num = (UINT32)(packed_rate >> 32);
            rate_den = (UINT32)(packed_rate & 0xffffffffu);
        }
        if (FAILED(rate_hr) || rate_num == 0 || rate_den == 0) {
            rate_num = 30;
            rate_den = 1;
        }
    }

    LONG stride = (LONG)width;
    UINT32 stride_value = 0;
    if (SUCCEEDED(IMFMediaType_GetUINT32(type, &MF_MT_DEFAULT_STRIDE, &stride_value))) {
        stride = (LONG)stride_value;
    }

    if (SUCCEEDED(hr)) {
        decoder->width = width;
        decoder->height = height;
        decoder->stride = stride;
        if (out_info) {
            out_info->width = width;
            out_info->height = height;
            out_info->frame_rate_num = rate_num;
            out_info->frame_rate_den = rate_den;
        }
    }
    IMFMediaType_Release(type);
    return hr;
}

BstMfDecoder *bst_mf_open_utf8(const char *path_utf8, BstMfVideoInfo *out_info, int32_t *out_hresult) {
    HRESULT hr = S_OK;
    BstMfDecoder *decoder = NULL;
    IMFMediaType *native_type = NULL;
    IMFMediaType *output_type = NULL;
    wchar_t *path_wide = NULL;

    if (out_hresult) *out_hresult = S_OK;
    if (!path_utf8 || !out_info) {
        if (out_hresult) *out_hresult = E_INVALIDARG;
        return NULL;
    }
    memset(out_info, 0, sizeof(*out_info));

    decoder = (BstMfDecoder *)calloc(1, sizeof(*decoder));
    if (!decoder) {
        if (out_hresult) *out_hresult = E_OUTOFMEMORY;
        return NULL;
    }

    hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
    if (SUCCEEDED(hr)) decoder->com_initialized = 1;
    else if (hr != RPC_E_CHANGED_MODE) goto fail;

    hr = MFStartup(MF_VERSION, MFSTARTUP_FULL);
    if (FAILED(hr)) goto fail;
    decoder->mf_initialized = 1;

    int wide_len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path_utf8, -1, NULL, 0);
    if (wide_len <= 0) {
        hr = HRESULT_FROM_WIN32(GetLastError());
        goto fail;
    }
    path_wide = (wchar_t *)calloc((size_t)wide_len, sizeof(wchar_t));
    if (!path_wide) {
        hr = E_OUTOFMEMORY;
        goto fail;
    }
    if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path_utf8, -1, path_wide, wide_len)) {
        hr = HRESULT_FROM_WIN32(GetLastError());
        goto fail;
    }

    hr = MFCreateSourceReaderFromURL(path_wide, NULL, &decoder->reader);
    if (FAILED(hr)) goto fail;

    hr = IMFSourceReader_SetStreamSelection(decoder->reader, MF_SOURCE_READER_ALL_STREAMS, FALSE);
    if (FAILED(hr)) goto fail;
    hr = IMFSourceReader_SetStreamSelection(decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM, TRUE);
    if (FAILED(hr)) goto fail;

    hr = IMFSourceReader_GetNativeMediaType(
        decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0, &native_type);
    if (SUCCEEDED(hr)) {
        GUID subtype = {0};
        if (SUCCEEDED(IMFMediaType_GetGUID(native_type, &MF_MT_SUBTYPE, &subtype))) {
            out_info->native_subtype_fourcc = subtype.Data1;
        }
        IMFMediaType_Release(native_type);
        native_type = NULL;
    }

    hr = MFCreateMediaType(&output_type);
    if (FAILED(hr)) goto fail;
    hr = IMFMediaType_SetGUID(output_type, &MF_MT_MAJOR_TYPE, &MFMediaType_Video);
    if (FAILED(hr)) goto fail;
    hr = IMFMediaType_SetGUID(output_type, &MF_MT_SUBTYPE, &MFVideoFormat_NV12);
    if (FAILED(hr)) goto fail;
    hr = IMFSourceReader_SetCurrentMediaType(
        decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM, NULL, output_type);
    if (FAILED(hr)) goto fail;

    hr = refresh_video_type(decoder, out_info);
    if (FAILED(hr)) goto fail;

    IMFMediaType_Release(output_type);
    free(path_wide);
    if (out_hresult) *out_hresult = S_OK;
    return decoder;

fail:
    if (native_type) IMFMediaType_Release(native_type);
    if (output_type) IMFMediaType_Release(output_type);
    free(path_wide);
    if (out_hresult) *out_hresult = failed_i32(hr);
    bst_mf_close(decoder);
    return NULL;
}

int32_t bst_mf_seek(BstMfDecoder *decoder, int64_t timestamp_hns) {
    if (!decoder || !decoder->reader) return failed_i32(E_INVALIDARG);
    PROPVARIANT position;
    PropVariantInit(&position);
    position.vt = VT_I8;
    position.hVal.QuadPart = timestamp_hns;
    HRESULT hr = IMFSourceReader_SetCurrentPosition(decoder->reader, &GUID_NULL, &position);
    PropVariantClear(&position);
    if (SUCCEEDED(hr)) {
        hr = IMFSourceReader_Flush(decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM);
    }
    return failed_i32(hr);
}

static HRESULT copy_nv12(BstMfDecoder *decoder, IMFMediaBuffer *buffer,
                         uint8_t *y_plane, uint8_t *uv_plane) {
    IMF2DBuffer *buffer_2d = NULL;
    HRESULT hr = IMFMediaBuffer_QueryInterface(buffer, &IID_IMF2DBuffer, (void **)&buffer_2d);
    if (SUCCEEDED(hr)) {
        BYTE *scanline = NULL;
        LONG pitch = 0;
        hr = IMF2DBuffer_Lock2D(buffer_2d, &scanline, &pitch);
        if (SUCCEEDED(hr)) {
            const LONG abs_pitch = pitch < 0 ? -pitch : pitch;
            BYTE *first_y = pitch < 0
                ? scanline + ((size_t)decoder->height - 1u) * (size_t)abs_pitch
                : scanline;
            for (uint32_t row = 0; row < decoder->height; ++row) {
                BYTE *source = pitch < 0
                    ? first_y - (size_t)row * (size_t)abs_pitch
                    : first_y + (size_t)row * (size_t)abs_pitch;
                memcpy(y_plane + (size_t)row * decoder->width, source, decoder->width);
            }
            BYTE *first_uv = scanline + (size_t)abs_pitch * decoder->height;
            for (uint32_t row = 0; row < decoder->height / 2u; ++row) {
                memcpy(uv_plane + (size_t)row * decoder->width,
                       first_uv + (size_t)row * (size_t)abs_pitch,
                       decoder->width);
            }
            IMF2DBuffer_Unlock2D(buffer_2d);
        }
        IMF2DBuffer_Release(buffer_2d);
        if (SUCCEEDED(hr)) return hr;
    }

    BYTE *data = NULL;
    DWORD max_length = 0, current_length = 0;
    hr = IMFMediaBuffer_Lock(buffer, &data, &max_length, &current_length);
    if (FAILED(hr)) return hr;
    const size_t y_size = (size_t)decoder->width * decoder->height;
    const size_t uv_size = y_size / 2u;
    if ((size_t)current_length < y_size + uv_size) {
        IMFMediaBuffer_Unlock(buffer);
        return MF_E_BUFFERTOOSMALL;
    }
    memcpy(y_plane, data, y_size);
    memcpy(uv_plane, data + y_size, uv_size);
    IMFMediaBuffer_Unlock(buffer);
    return S_OK;
}

int32_t bst_mf_read_nv12(BstMfDecoder *decoder,
                         uint8_t *y_plane, uint32_t y_capacity,
                         uint8_t *uv_plane, uint32_t uv_capacity,
                         BstMfFrameInfo *out_frame) {
    if (!decoder || !decoder->reader || !y_plane || !uv_plane || !out_frame) {
        return failed_i32(E_INVALIDARG);
    }
    const uint64_t y_required = (uint64_t)decoder->width * decoder->height;
    const uint64_t uv_required = y_required / 2u;
    if (y_capacity < y_required || uv_capacity < uv_required) {
        return failed_i32(MF_E_BUFFERTOOSMALL);
    }

    for (;;) {
        DWORD stream_index = 0, flags = 0;
        LONGLONG timestamp = 0;
        IMFSample *sample = NULL;
        HRESULT hr = IMFSourceReader_ReadSample(
            decoder->reader, MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0,
            &stream_index, &flags, &timestamp, &sample);
        if (FAILED(hr)) return failed_i32(hr);

        if (flags & MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) {
            hr = refresh_video_type(decoder, NULL);
            if (FAILED(hr)) {
                if (sample) IMFSample_Release(sample);
                return failed_i32(hr);
            }
        }
        if ((flags & MF_SOURCE_READERF_ENDOFSTREAM) && !sample) return 0;
        if (!sample) continue;

        IMFMediaBuffer *buffer = NULL;
        hr = IMFSample_ConvertToContiguousBuffer(sample, &buffer);
        if (SUCCEEDED(hr)) hr = copy_nv12(decoder, buffer, y_plane, uv_plane);

        out_frame->timestamp_hns = timestamp;
        out_frame->duration_hns = 0;
        out_frame->source_reader_flags = flags;
        if (SUCCEEDED(hr)) {
            LONGLONG duration = 0;
            if (SUCCEEDED(IMFSample_GetSampleDuration(sample, &duration))) {
                out_frame->duration_hns = duration;
            }
        }

        if (buffer) IMFMediaBuffer_Release(buffer);
        IMFSample_Release(sample);
        if (FAILED(hr)) return failed_i32(hr);
        return 1;
    }
}

void bst_mf_close(BstMfDecoder *decoder) {
    if (!decoder) return;
    if (decoder->reader) IMFSourceReader_Release(decoder->reader);
    if (decoder->mf_initialized) MFShutdown();
    if (decoder->com_initialized) CoUninitialize();
    free(decoder);
}
