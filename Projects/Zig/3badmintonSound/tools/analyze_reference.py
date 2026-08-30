"""Segment the supplied racket video audio and compare it with model renders.

The exported WAV files retain the decoded source and simulator levels. Spectra in
the SVG are offset independently only for the explicitly labelled shape overlay.
"""

from __future__ import annotations

import csv
import json
import math
import shutil
import subprocess
import wave
from pathlib import Path

import numpy as np


ROOT = Path(__file__).resolve().parents[1]
REFERENCE = ROOT / "reference"
SOURCE_AUDIO = ROOT / "audio.mp3"
SOURCE_WAV = REFERENCE / "source.wav"
GENERATED = REFERENCE / "generated"
TENSIONS = (20, 24, 27, 30)
SAMPLE_RATE = 48_000
LAST_HIT_PRE_S = 0.020
LAST_HIT_POST_S = 0.160


def ensure_source_wav() -> None:
    if SOURCE_WAV.exists():
        return
    if not SOURCE_AUDIO.exists():
        raise FileNotFoundError(
            f"Expected either {SOURCE_WAV.relative_to(ROOT)} or "
            f"{SOURCE_AUDIO.relative_to(ROOT)}"
        )
    ffmpeg = shutil.which("ffmpeg")
    if ffmpeg is None:
        raise RuntimeError("ffmpeg is required to decode the supplied audio")
    SOURCE_WAV.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        [
            ffmpeg,
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-i",
            str(SOURCE_AUDIO),
            "-map",
            "0:a:0",
            "-ac",
            "1",
            "-ar",
            str(SAMPLE_RATE),
            "-c:a",
            "pcm_s16le",
            str(SOURCE_WAV),
        ],
        check=True,
    )


def read_pcm16(path: Path) -> tuple[int, np.ndarray]:
    with wave.open(str(path), "rb") as wav:
        rate = wav.getframerate()
        channels = wav.getnchannels()
        width = wav.getsampwidth()
        frames = wav.getnframes()
        if width != 2:
            raise ValueError(f"{path} is not 16-bit PCM")
        raw = np.frombuffer(wav.readframes(frames), dtype="<i2")
    if channels > 1:
        raw = raw.reshape(-1, channels).astype(np.float64).mean(axis=1)
    return rate, raw.astype(np.float64) / 32768.0


def write_pcm16(path: Path, rate: int, samples: np.ndarray) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    clipped = np.clip(samples, -1.0, 32767.0 / 32768.0)
    quantized = np.rint(clipped * 32768.0).astype("<i2")
    with wave.open(str(path), "wb") as wav:
        wav.setnchannels(1)
        wav.setsampwidth(2)
        wav.setframerate(rate)
        wav.writeframes(quantized.tobytes())


def moving_rms(samples: np.ndarray, frames: int) -> np.ndarray:
    frames = max(1, int(frames))
    kernel = np.full(frames, 1.0 / frames)
    return np.sqrt(np.convolve(samples * samples, kernel, mode="same"))


def impact_novelty(samples: np.ndarray, rate: int) -> np.ndarray:
    # A first difference discounts the music/voice bed and emphasizes the palm
    # strike's broadband leading edge.
    high_passed = np.diff(samples, prepend=samples[0])
    return moving_rms(high_passed, round(0.002 * rate))


def detect_twenty_impacts(samples: np.ndarray, rate: int) -> np.ndarray:
    novelty = impact_novelty(samples, rate)
    local_maxima = np.flatnonzero(
        (novelty[1:-1] > novelty[:-2])
        & (novelty[1:-1] >= novelty[2:])
    ) + 1
    local_maxima = local_maxima[
        novelty[local_maxima] > np.quantile(novelty, 0.90)
    ]

    minimum_spacing = round(0.170 * rate)
    selected: list[int] = []
    for candidate in local_maxima[np.argsort(novelty[local_maxima])[::-1]]:
        index = int(candidate)
        if all(abs(index - previous) > minimum_spacing for previous in selected):
            selected.append(index)
            if len(selected) == 20:
                break
    if len(selected) != 20:
        raise RuntimeError(f"Expected 20 impacts, detected {len(selected)}")

    onsets = np.array(sorted(selected), dtype=np.int64)
    gaps = np.diff(onsets)
    group_breaks = sorted((np.argsort(gaps)[-3:] + 1).tolist())
    groups = np.split(onsets, group_breaks)
    if [len(group) for group in groups] != [5, 5, 5, 5]:
        raise RuntimeError(
            "The three longest silences did not split the impacts into 4 x 5"
        )
    return onsets


def split_groups(onsets: np.ndarray) -> list[np.ndarray]:
    gaps = np.diff(onsets)
    breaks = sorted((np.argsort(gaps)[-3:] + 1).tolist())
    return [group for group in np.split(onsets, breaks)]


def detect_clip_onset(samples: np.ndarray, rate: int) -> int:
    novelty = impact_novelty(samples, rate)
    # Synthesized clips include configurable pre-impact audio for racket
    # aeroacoustics; the calibrated default impact now arrives near 63 ms.
    search_end = min(len(samples), round(0.150 * rate))
    if search_end == 0:
        return 0
    peak = int(np.argmax(novelty[:search_end]))
    threshold = novelty[peak] * 0.20
    candidates = np.flatnonzero(novelty[: peak + 1] >= threshold)
    return int(candidates[0]) if candidates.size else peak


def spectrum(
    samples: np.ndarray,
    rate: int,
    onset: int,
    start_s: float,
    end_s: float,
    fft_size: int = 32_768,
) -> tuple[np.ndarray, np.ndarray]:
    start = min(len(samples), onset + round(start_s * rate))
    end = min(len(samples), onset + round(end_s * rate))
    windowed = samples[start:end]
    if windowed.size < 32:
        return np.fft.rfftfreq(fft_size, 1.0 / rate), np.zeros(
            fft_size // 2 + 1
        )
    windowed = windowed * np.hanning(windowed.size)
    power = np.abs(np.fft.rfft(windowed, fft_size)) ** 2
    return np.fft.rfftfreq(fft_size, 1.0 / rate), power


def band_power(freq: np.ndarray, power: np.ndarray, low: float, high: float) -> float:
    selected = (freq >= low) & (freq < high)
    return float(np.sum(power[selected]))


def peak_frequency(
    freq: np.ndarray, power: np.ndarray, low: float, high: float
) -> float:
    selected = np.flatnonzero((freq >= low) & (freq <= high))
    if selected.size == 0:
        return 0.0
    return float(freq[selected[np.argmax(power[selected])]])


def db(value: float) -> float:
    return 20.0 * math.log10(max(value, 1.0e-12))


def analyze_hit(samples: np.ndarray, rate: int, known_onset: int | None = None) -> dict:
    onset = detect_clip_onset(samples, rate) if known_onset is None else known_onset
    # Start after the broadband contact edge so the note metric describes the
    # string-bed ring rather than whichever impulse-spectrum bin is largest.
    body_freq, body_power = spectrum(samples, rate, onset, 0.006, 0.120)
    attack_freq, attack_power = spectrum(samples, rate, onset, 0.000, 0.020)

    attack_band = (attack_freq >= 300.0) & (attack_freq < 12_000.0)
    attack_denominator = float(np.sum(attack_power[attack_band]))
    centroid = (
        float(
            np.sum(
                attack_freq[attack_band] * attack_power[attack_band]
            )
            / attack_denominator
        )
        if attack_denominator > 0.0
        else 0.0
    )
    low_power = band_power(attack_freq, attack_power, 300.0, 3_000.0)
    high_power = band_power(attack_freq, attack_power, 3_000.0, 12_000.0)
    lower_wide = band_power(attack_freq, attack_power, 300.0, 6_000.0)
    very_high = band_power(attack_freq, attack_power, 6_000.0, 16_000.0)

    envelope = moving_rms(samples, round(0.0015 * rate))
    peak_search_end = min(len(samples), onset + round(0.050 * rate))
    peak_index = (
        onset + int(np.argmax(envelope[onset:peak_search_end]))
        if peak_search_end > onset
        else onset
    )
    peak_envelope = float(envelope[peak_index]) if envelope.size else 0.0
    tail_start = max(0, len(envelope) - round(0.030 * rate))
    noise_floor = float(np.median(envelope[tail_start:])) if envelope.size else 0.0
    threshold = max(peak_envelope * 10.0 ** (-30.0 / 20.0), noise_floor * 2.0)
    above = np.flatnonzero(envelope[peak_index:] > threshold)
    decay_ms = (
        float(above[-1] + peak_index - onset) * 1000.0 / rate
        if above.size
        else 0.0
    )

    peak = float(np.max(np.abs(samples))) if samples.size else 0.0
    rms = float(np.sqrt(np.mean(samples * samples))) if samples.size else 0.0
    crest = peak / max(rms, 1.0e-12)
    return {
        "onset_ms": round(onset * 1000.0 / rate, 4),
        "peak_dbfs": round(db(peak), 3),
        "rms_dbfs": round(db(rms), 3),
        "crest_factor": round(crest, 3),
        # The strongest low string-bed resonance gives the perceptual note even
        # when an upper partial is fractionally louder.
        "note_peak_hz": round(
            peak_frequency(body_freq, body_power, 500.0, 1_600.0), 3
        ),
        "dominant_300_5000_hz": round(
            peak_frequency(body_freq, body_power, 300.0, 5_000.0), 3
        ),
        "attack_centroid_hz": round(centroid, 3),
        "attack_hf_ratio_db": round(
            10.0 * math.log10(max(high_power, 1.0e-24) / max(low_power, 1.0e-24)),
            3,
        ),
        "attack_very_hf_ratio_db": round(
            10.0
            * math.log10(max(very_high, 1.0e-24) / max(lower_wide, 1.0e-24)),
            3,
        ),
        "decay_30db_ms": round(decay_ms, 3),
    }


def normalized_spectrum(
    samples: np.ndarray, rate: int, onset: int
) -> tuple[np.ndarray, np.ndarray]:
    freq, power = spectrum(samples, rate, onset, 0.000, 0.140)
    magnitude_db = 10.0 * np.log10(np.maximum(power, 1.0e-24))
    useful = (freq >= 300.0) & (freq <= 16_000.0)
    magnitude_db -= float(np.max(magnitude_db[useful]))
    return freq, magnitude_db


def spectral_distance(
    reference_samples: np.ndarray,
    reference_onset: int,
    model_samples: np.ndarray,
    model_onset: int,
    rate: int,
) -> float:
    freq, reference_db = normalized_spectrum(
        reference_samples, rate, reference_onset
    )
    _, model_db = normalized_spectrum(model_samples, rate, model_onset)
    selected = (freq >= 300.0) & (freq <= 12_000.0)
    difference = np.clip(reference_db[selected], -60.0, 0.0) - np.clip(
        model_db[selected], -60.0, 0.0
    )
    return float(np.sqrt(np.mean(difference * difference)))


def svg_path(
    x_values: np.ndarray,
    y_values: np.ndarray,
    x0: float,
    y0: float,
    width: float,
    height: float,
    x_min: float,
    x_max: float,
    y_min: float,
    y_max: float,
    logarithmic_x: bool = False,
) -> str:
    if x_values.size == 0:
        return ""
    if logarithmic_x:
        x_values = np.log(np.maximum(x_values, x_min))
        x_min = math.log(x_min)
        x_max = math.log(x_max)
    x_pixels = x0 + (x_values - x_min) / (x_max - x_min) * width
    y_pixels = y0 + height - (y_values - y_min) / (y_max - y_min) * height
    return " ".join(
        ("M" if index == 0 else "L") + f"{x:.2f},{y:.2f}"
        for index, (x, y) in enumerate(zip(x_pixels, y_pixels))
    )


def make_svg(
    reference_clips: dict[int, np.ndarray],
    model_clips: dict[int, np.ndarray],
    rate: int,
    output: Path,
) -> None:
    width = 1500
    height = 1040
    row_height = 235
    lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" '
        f'height="{height}" viewBox="0 0 {width} {height}">',
        "<style>"
        "text{font-family:Segoe UI,Arial,sans-serif;fill:#d8e4ee}"
        ".small{font-size:13px;fill:#8fa5b5}.title{font-size:22px;font-weight:600}"
        ".label{font-size:16px;font-weight:600}.grid{stroke:#263b49;stroke-width:1}"
        ".ref{stroke:#61d6ff;stroke-width:1.5;fill:none}"
        ".model{stroke:#ffae57;stroke-width:1.5;fill:none}"
        "</style>",
        f'<rect width="{width}" height="{height}" fill="#07131c"/>',
        '<text x="40" y="34" class="title">Palm reference vs physics model</text>',
        '<line x1="930" y1="27" x2="970" y2="27" class="ref"/>',
        '<text x="978" y="32" class="small">reference hit 5</text>',
        '<line x1="1130" y1="27" x2="1170" y2="27" class="model"/>',
        '<text x="1178" y="32" class="small">model, 60 m/s cork</text>',
    ]

    for row, tension in enumerate(TENSIONS):
        top = 55 + row * row_height
        wave_x, wave_y, wave_w, wave_h = 55.0, top + 32.0, 620.0, 165.0
        spec_x, spec_y, spec_w, spec_h = 780.0, top + 32.0, 660.0, 165.0
        lines.append(
            f'<text x="55" y="{top + 20}" class="label">{tension} lb</text>'
        )
        for chart_x, chart_y, chart_w, chart_h in (
            (wave_x, wave_y, wave_w, wave_h),
            (spec_x, spec_y, spec_w, spec_h),
        ):
            lines.append(
                f'<rect x="{chart_x}" y="{chart_y}" width="{chart_w}" '
                f'height="{chart_h}" fill="#0c1c27" stroke="#345063"/>'
            )
        lines.append(
            f'<text x="{wave_x + 70}" y="{top + 20}" class="small">'
            "aligned waveform, raw dBFS scale (first 60 ms)</text>"
        )
        lines.append(
            f'<text x="{spec_x}" y="{top + 20}" class="small">'
            "spectral shape, independently offset to 0 dB</text>"
        )
        for value in (-0.5, 0.0, 0.5):
            y = wave_y + wave_h - (value + 0.6) / 1.2 * wave_h
            lines.append(
                f'<line x1="{wave_x}" y1="{y:.2f}" x2="{wave_x + wave_w}" '
                f'y2="{y:.2f}" class="grid"/>'
            )
            lines.append(
                f'<text x="{wave_x - 42}" y="{y + 4:.2f}" '
                f'class="small">{value:+.1f}</text>'
            )
        for value in (-60, -40, -20, 0):
            y = spec_y + spec_h - (value + 60) / 60 * spec_h
            lines.append(
                f'<line x1="{spec_x}" y1="{y:.2f}" x2="{spec_x + spec_w}" '
                f'y2="{y:.2f}" class="grid"/>'
            )
        for hz in (300, 1_000, 3_000, 10_000, 16_000):
            x = spec_x + (
                math.log(hz) - math.log(300)
            ) / (math.log(16_000) - math.log(300)) * spec_w
            lines.append(
                f'<line x1="{x:.2f}" y1="{spec_y}" x2="{x:.2f}" '
                f'y2="{spec_y + spec_h}" class="grid"/>'
            )
            label = f"{hz // 1000}k" if hz >= 1000 else str(hz)
            lines.append(
                f'<text x="{x - 10:.2f}" y="{spec_y + spec_h + 18}" '
                f'class="small">{label}</text>'
            )

        for css_class, samples in (
            ("ref", reference_clips[tension]),
            ("model", model_clips[tension]),
        ):
            onset = (
                round(LAST_HIT_PRE_S * rate)
                if css_class == "ref"
                else detect_clip_onset(samples, rate)
            )
            wave_count = min(round(0.060 * rate), len(samples) - onset)
            if wave_count > 0:
                indices = np.linspace(
                    0, wave_count - 1, min(900, wave_count), dtype=np.int64
                )
                wave_times = indices / rate
                wave_values = samples[onset + indices]
                path = svg_path(
                    wave_times,
                    wave_values,
                    wave_x,
                    wave_y,
                    wave_w,
                    wave_h,
                    0.0,
                    0.060,
                    -0.6,
                    0.6,
                )
                lines.append(f'<path d="{path}" class="{css_class}"/>')

            freq, level = normalized_spectrum(samples, rate, onset)
            # Plot a narrow smoothed envelope so individual FFT-bin leakage
            # does not obscure the comparison. Metrics still use unsmoothed
            # power spectra.
            linear_power = 10.0 ** (level / 10.0)
            linear_power = np.convolve(
                linear_power, np.full(65, 1.0 / 65.0), mode="same"
            )
            level = 10.0 * np.log10(np.maximum(linear_power, 1.0e-12))
            visible_band = (freq >= 300.0) & (freq <= 16_000.0)
            level -= float(np.max(level[visible_band]))
            selected_indices = np.flatnonzero(
                (freq >= 300.0) & (freq <= 16_000.0)
            )[::12]
            path = svg_path(
                freq[selected_indices],
                np.clip(level[selected_indices], -60.0, 0.0),
                spec_x,
                spec_y,
                spec_w,
                spec_h,
                300.0,
                16_000.0,
                -60.0,
                0.0,
                logarithmic_x=True,
            )
            lines.append(f'<path d="{path}" class="{css_class}"/>')

    lines.append(
        '<text x="55" y="1015" class="small">'
        "Audio files are not normalized. Spectral offsets are display-only; "
        "absolute palm-recording level is not comparable to a 1 m virtual microphone."
        "</text>"
    )
    lines.append("</svg>")
    output.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    ensure_source_wav()
    GENERATED.mkdir(parents=True, exist_ok=True)
    rate, source = read_pcm16(SOURCE_WAV)
    if rate != SAMPLE_RATE:
        raise ValueError(f"Expected {SAMPLE_RATE} Hz source, got {rate}")

    onsets = detect_twenty_impacts(source, rate)
    groups = split_groups(onsets)
    reference_clips: dict[int, np.ndarray] = {}
    onset_rows: list[dict] = []
    reference_metrics: dict[int, dict] = {}

    for tension, group in zip(TENSIONS, groups):
        group_start = max(0, int(group[0] - round(0.080 * rate)))
        group_end = min(len(source), int(group[-1] + round(0.250 * rate)))
        write_pcm16(
            GENERATED / f"reference_{tension}lb_all5.wav",
            rate,
            source[group_start:group_end],
        )

        last_onset = int(group[-1])
        clip_start = max(0, last_onset - round(LAST_HIT_PRE_S * rate))
        clip_end = min(len(source), last_onset + round(LAST_HIT_POST_S * rate))
        clip = source[clip_start:clip_end]
        reference_clips[tension] = clip
        write_pcm16(
            GENERATED / f"reference_{tension}lb_hit5.wav", rate, clip
        )
        reference_metrics[tension] = analyze_hit(
            clip, rate, known_onset=last_onset - clip_start
        )
        for hit_number, onset in enumerate(group, start=1):
            onset_rows.append(
                {
                    "tension_lbf": tension,
                    "hit_number": hit_number,
                    "onset_s": round(float(onset) / rate, 6),
                }
            )

    with (GENERATED / "detected_onsets.csv").open(
        "w", newline="", encoding="utf-8"
    ) as file:
        writer = csv.DictWriter(
            file, fieldnames=("tension_lbf", "hit_number", "onset_s")
        )
        writer.writeheader()
        writer.writerows(onset_rows)

    model_clips: dict[int, np.ndarray] = {}
    model_metrics: dict[int, dict] = {}
    comparisons: dict[int, dict] = {}
    missing_models: list[Path] = []
    for tension in TENSIONS:
        path = GENERATED / f"model_{tension}lb.wav"
        if not path.exists():
            missing_models.append(path)
            continue
        model_rate, clip = read_pcm16(path)
        if model_rate != rate:
            raise ValueError(f"{path.name} has unexpected sample rate {model_rate}")
        model_clips[tension] = clip
        model_onset = detect_clip_onset(clip, rate)
        model_metrics[tension] = analyze_hit(clip, rate, model_onset)
        reference_onset = round(LAST_HIT_PRE_S * rate)
        comparisons[tension] = {
            "note_error_semitones": round(
                12.0
                * math.log2(
                    model_metrics[tension]["note_peak_hz"]
                    / reference_metrics[tension]["note_peak_hz"]
                ),
                3,
            ),
            "attack_centroid_ratio": round(
                model_metrics[tension]["attack_centroid_hz"]
                / reference_metrics[tension]["attack_centroid_hz"],
                3,
            ),
            "attack_hf_ratio_error_db": round(
                model_metrics[tension]["attack_hf_ratio_db"]
                - reference_metrics[tension]["attack_hf_ratio_db"],
                3,
            ),
            "spectral_shape_rms_error_db": round(
                spectral_distance(
                    reference_clips[tension],
                    reference_onset,
                    clip,
                    model_onset,
                    rate,
                ),
                3,
            ),
        }

    report = {
        "source": {
            "path": str(SOURCE_WAV.relative_to(ROOT)).replace("\\", "/"),
            "sample_rate_hz": rate,
            "duration_s": round(len(source) / rate, 6),
            "normalization_applied": False,
            "detected_onsets_s": [round(float(value) / rate, 6) for value in onsets],
        },
        "reference_hit_5": {str(k): v for k, v in reference_metrics.items()},
        "model": {str(k): v for k, v in model_metrics.items()},
        "comparison": {str(k): v for k, v in comparisons.items()},
    }
    (GENERATED / "analysis.json").write_text(
        json.dumps(report, indent=2) + "\n", encoding="utf-8"
    )

    if comparisons:
        with (GENERATED / "comparison.csv").open(
            "w", newline="", encoding="utf-8"
        ) as file:
            fieldnames = (
                "tension_lbf",
                "reference_note_hz",
                "model_note_hz",
                "note_error_semitones",
                "reference_attack_centroid_hz",
                "model_attack_centroid_hz",
                "attack_centroid_ratio",
                "reference_attack_hf_ratio_db",
                "model_attack_hf_ratio_db",
                "attack_hf_ratio_error_db",
                "spectral_shape_rms_error_db",
            )
            writer = csv.DictWriter(file, fieldnames=fieldnames)
            writer.writeheader()
            for tension in TENSIONS:
                writer.writerow(
                    {
                        "tension_lbf": tension,
                        "reference_note_hz": reference_metrics[tension][
                            "note_peak_hz"
                        ],
                        "model_note_hz": model_metrics[tension]["note_peak_hz"],
                        "note_error_semitones": comparisons[tension][
                            "note_error_semitones"
                        ],
                        "reference_attack_centroid_hz": reference_metrics[
                            tension
                        ]["attack_centroid_hz"],
                        "model_attack_centroid_hz": model_metrics[tension][
                            "attack_centroid_hz"
                        ],
                        "attack_centroid_ratio": comparisons[tension][
                            "attack_centroid_ratio"
                        ],
                        "reference_attack_hf_ratio_db": reference_metrics[
                            tension
                        ]["attack_hf_ratio_db"],
                        "model_attack_hf_ratio_db": model_metrics[tension][
                            "attack_hf_ratio_db"
                        ],
                        "attack_hf_ratio_error_db": comparisons[tension][
                            "attack_hf_ratio_error_db"
                        ],
                        "spectral_shape_rms_error_db": comparisons[tension][
                            "spectral_shape_rms_error_db"
                        ],
                    }
                )
        make_svg(
            reference_clips,
            model_clips,
            rate,
            GENERATED / "comparison.svg",
        )

    markdown = [
        "# Palm-hit reference analysis",
        "",
        "The four groups were split at the three longest silent gaps. Hit 5 "
        "from each group is compared without changing either WAV's level.",
        "",
        "| Tension | Ref note | Ref centroid | Ref HF ratio | "
        "Model note | Model centroid | Note error | HF error |",
        "|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for tension in TENSIONS:
        reference = reference_metrics[tension]
        if tension in model_metrics:
            modeled = model_metrics[tension]
            delta = comparisons[tension]
            markdown.append(
                f"| {tension} lb | {reference['note_peak_hz']:.1f} Hz | "
                f"{reference['attack_centroid_hz']:.0f} Hz | "
                f"{reference['attack_hf_ratio_db']:+.1f} dB | "
                f"{modeled['note_peak_hz']:.1f} Hz | "
                f"{modeled['attack_centroid_hz']:.0f} Hz | "
                f"{delta['note_error_semitones']:+.2f} st | "
                f"{delta['attack_hf_ratio_error_db']:+.1f} dB |"
            )
        else:
            markdown.append(
                f"| {tension} lb | {reference['note_peak_hz']:.1f} Hz | "
                f"{reference['attack_centroid_hz']:.0f} Hz | "
                f"{reference['attack_hf_ratio_db']:+.1f} dB | — | — | — | — |"
            )
    markdown.extend(
        [
            "",
            "The reference is a palm strike recorded through an unknown phone "
            "microphone and codec. Absolute level, contact duration, and the "
            "broadband slap cannot be treated as cork-at-1-m measurements; the "
            "stable resonance progression and relative spectral balance are "
            "the useful calibration targets.",
            "",
        ]
    )
    (GENERATED / "REPORT.md").write_text(
        "\n".join(markdown), encoding="utf-8"
    )

    print("Detected impacts:")
    for tension, group in zip(TENSIONS, groups):
        print(
            f"  {tension} lb: "
            + ", ".join(f"{value / rate:.4f}s" for value in group)
        )
    if missing_models:
        print("Reference clips written; run the Zig reference renderer, then rerun.")
    else:
        print(f"Comparison written to {GENERATED}")


if __name__ == "__main__":
    main()
