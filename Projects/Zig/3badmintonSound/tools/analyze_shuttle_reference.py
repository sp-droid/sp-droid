"""Analyze the supplied 30 lb real-shuttle recording against low-speed renders."""

from __future__ import annotations

import csv
import json
import shutil
import subprocess
from pathlib import Path

import numpy as np

from analyze_reference import (
    GENERATED,
    ROOT,
    SAMPLE_RATE,
    analyze_hit,
    band_power,
    detect_clip_onset,
    impact_novelty,
    normalized_spectrum,
    read_pcm16,
    spectrum,
    svg_path,
    write_pcm16,
)


SOURCE_MP3 = ROOT / "audio_2.mp3"
SOURCE_WAV = ROOT / "reference" / "shuttle_source.wav"
MODEL_SPEEDS = (2, 5, 10, 15)
CLIP_PRE_S = 0.020
CLIP_POST_S = 0.300


def ensure_source() -> None:
    if SOURCE_WAV.exists():
        return
    if not SOURCE_MP3.exists():
        raise FileNotFoundError("audio_2.mp3 is missing")
    ffmpeg = shutil.which("ffmpeg")
    if ffmpeg is None:
        raise RuntimeError("ffmpeg is required to decode audio_2.mp3")
    SOURCE_WAV.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        [
            ffmpeg,
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-i",
            str(SOURCE_MP3),
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


def detect_impacts(samples: np.ndarray, rate: int) -> np.ndarray:
    novelty = impact_novelty(samples, rate)
    maxima = np.flatnonzero(
        (novelty[1:-1] > novelty[:-2])
        & (novelty[1:-1] >= novelty[2:])
    ) + 1
    maxima = maxima[
        (maxima > round(0.8 * rate))
        & (novelty[maxima] > np.quantile(novelty, 0.85))
    ]
    selected: list[int] = []
    minimum_spacing = round(0.6 * rate)
    for candidate in maxima[np.argsort(novelty[maxima])[::-1]]:
        index = int(candidate)
        if all(abs(index - previous) > minimum_spacing for previous in selected):
            selected.append(index)
            if len(selected) == 8:
                break
    if len(selected) != 8:
        raise RuntimeError(f"Expected eight isolated impacts, found {len(selected)}")
    return np.array(sorted(selected), dtype=np.int64)


def spectral_flatness(
    samples: np.ndarray,
    rate: int,
    onset: int,
    low_hz: float = 2_000.0,
    high_hz: float = 12_000.0,
) -> float:
    frequency, power = spectrum(samples, rate, onset, 0.0, 0.020)
    selected = power[(frequency >= low_hz) & (frequency <= high_hz)]
    selected = np.maximum(selected, 1.0e-24)
    return float(np.exp(np.mean(np.log(selected))) / np.mean(selected))


def band_shape(samples: np.ndarray, rate: int, onset: int) -> list[float]:
    bands = (
        (300.0, 1_000.0),
        (1_000.0, 2_000.0),
        (2_000.0, 4_000.0),
        (4_000.0, 8_000.0),
        (8_000.0, 12_000.0),
    )
    frequency, power = spectrum(samples, rate, onset, 0.0, 0.020)
    values = np.array(
        [band_power(frequency, power, low, high) for low, high in bands]
    )
    reference = max(float(values[0] + values[1]), 1.0e-24)
    return [
        round(float(value), 3)
        for value in (10.0 * np.log10(np.maximum(values, 1.0e-24) / reference))
    ]


def window_band_power(
    samples: np.ndarray,
    rate: int,
    onset: int,
    start_s: float,
    end_s: float,
    low_hz: float,
    high_hz: float,
) -> float:
    first = max(0, onset + round(start_s * rate))
    last = min(len(samples), onset + round(end_s * rate))
    segment = samples[first:last]
    if len(segment) < 4:
        return 1.0e-24
    window = np.hanning(len(segment))
    fft_size = max(2048, 1 << (len(segment) - 1).bit_length())
    transformed = np.fft.rfft(segment * window, fft_size)
    frequency = np.fft.rfftfreq(fft_size, 1.0 / rate)
    selected = (frequency >= low_hz) & (frequency <= high_hz)
    return float(
        np.sum(np.abs(transformed[selected]) ** 2)
        / max(float(np.sum(window * window)), 1.0e-24)
    )


def upper_mode_persistence_db(
    samples: np.ndarray, rate: int, onset: int
) -> float:
    attack = window_band_power(
        samples, rate, onset, 0.0, 0.003, 2_000.0, 8_000.0
    )
    ringing = window_band_power(
        samples, rate, onset, 0.006, 0.024, 2_000.0, 8_000.0
    )
    return float(10.0 * np.log10(max(ringing, 1.0e-24) / max(attack, 1.0e-24)))


def metrics_with_texture(
    samples: np.ndarray, rate: int, onset: int
) -> dict[str, float | list[float]]:
    metrics = analyze_hit(samples, rate, onset)
    metrics["spectral_flatness_2_12khz"] = round(
        spectral_flatness(samples, rate, onset), 6
    )
    metrics["attack_bands_db"] = band_shape(samples, rate, onset)
    metrics["upper_mode_persistence_db"] = round(
        upper_mode_persistence_db(samples, rate, onset), 6
    )
    return metrics


def median_metrics(rows: list[dict]) -> dict[str, float | list[float]]:
    scalar_keys = [
        key
        for key, value in rows[0].items()
        if isinstance(value, (float, int))
    ]
    result: dict[str, float | list[float]] = {
        key: round(float(np.median([row[key] for row in rows])), 6)
        for key in scalar_keys
    }
    result["attack_bands_db"] = [
        round(float(value), 3)
        for value in np.median(
            np.array([row["attack_bands_db"] for row in rows]), axis=0
        )
    ]
    return result


def median_normalized_spectrum(
    clips: list[np.ndarray], rate: int, onset: int
) -> tuple[np.ndarray, np.ndarray]:
    spectra = []
    frequency: np.ndarray | None = None
    for clip in clips:
        frequency, level = normalized_spectrum(clip, rate, onset)
        spectra.append(level)
    assert frequency is not None
    return frequency, np.median(np.array(spectra), axis=0)


def smoothed_level(frequency: np.ndarray, level: np.ndarray) -> np.ndarray:
    power = 10.0 ** (level / 10.0)
    power = np.convolve(power, np.full(65, 1.0 / 65.0), mode="same")
    result = 10.0 * np.log10(np.maximum(power, 1.0e-12))
    visible = (frequency >= 300.0) & (frequency <= 16_000.0)
    return result - float(np.max(result[visible]))


def make_svg(
    reference_clips: list[np.ndarray],
    model_clip: np.ndarray,
    rate: int,
    output: Path,
) -> None:
    width, height = 1450, 760
    reference_onset = round(CLIP_PRE_S * rate)
    model_onset = detect_clip_onset(model_clip, rate)
    ref_frequency, ref_level = median_normalized_spectrum(
        reference_clips, rate, reference_onset
    )
    model_frequency, model_level = normalized_spectrum(
        model_clip, rate, model_onset
    )
    ref_level = smoothed_level(ref_frequency, ref_level)
    model_level = smoothed_level(model_frequency, model_level)

    lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" '
        f'height="{height}" viewBox="0 0 {width} {height}">',
        "<style>"
        "text{font-family:Segoe UI,Arial,sans-serif;fill:#d8e4ee}"
        ".small{font-size:14px;fill:#8fa5b5}.title{font-size:24px;font-weight:600}"
        ".grid{stroke:#263b49;stroke-width:1}"
        ".ref{stroke:#61d6ff;stroke-width:2;fill:none}"
        ".model{stroke:#ffae57;stroke-width:2;fill:none}"
        "</style>",
        f'<rect width="{width}" height="{height}" fill="#07131c"/>',
        '<text x="45" y="38" class="title">30 lb real shuttle vs simplified model</text>',
        '<line x1="940" y1="30" x2="980" y2="30" class="ref"/>',
        '<text x="990" y="35" class="small">median of 8 real impacts</text>',
        '<line x1="1200" y1="30" x2="1240" y2="30" class="model"/>',
        '<text x="1250" y="35" class="small">5 m/s model</text>',
    ]

    wave_x, wave_y, wave_w, wave_h = 60.0, 100.0, 1330.0, 230.0
    spec_x, spec_y, spec_w, spec_h = 60.0, 425.0, 1330.0, 250.0
    for x, y, w, h in (
        (wave_x, wave_y, wave_w, wave_h),
        (spec_x, spec_y, spec_w, spec_h),
    ):
        lines.append(
            f'<rect x="{x}" y="{y}" width="{w}" height="{h}" '
            'fill="#0c1c27" stroke="#345063"/>'
        )
    lines.append(
        '<text x="60" y="88" class="small">'
        "attack waveform, aligned and peak-scaled for timbre display only</text>"
    )
    lines.append(
        '<text x="60" y="413" class="small">'
        "20 ms attack spectrum, smoothed and independently offset to 0 dB</text>"
    )

    for value in (-1.0, 0.0, 1.0):
        y = wave_y + wave_h - (value + 1.0) / 2.0 * wave_h
        lines.append(
            f'<line x1="{wave_x}" y1="{y:.2f}" x2="{wave_x + wave_w}" '
            f'y2="{y:.2f}" class="grid"/>'
        )
    for value in (-60, -40, -20, 0):
        y = spec_y + spec_h - (value + 60.0) / 60.0 * spec_h
        lines.append(
            f'<line x1="{spec_x}" y1="{y:.2f}" x2="{spec_x + spec_w}" '
            f'y2="{y:.2f}" class="grid"/>'
        )
    for hz in (300, 1_000, 3_000, 10_000, 16_000):
        x = spec_x + (
            np.log(hz) - np.log(300)
        ) / (np.log(16_000) - np.log(300)) * spec_w
        lines.append(
            f'<line x1="{x:.2f}" y1="{spec_y}" x2="{x:.2f}" '
            f'y2="{spec_y + spec_h}" class="grid"/>'
        )
        label = f"{hz // 1000}k" if hz >= 1_000 else str(hz)
        lines.append(
            f'<text x="{x - 10:.2f}" y="{spec_y + spec_h + 22}" '
            f'class="small">{label}</text>'
        )

    display_reference = reference_clips[-2]
    for css_class, samples, onset in (
        ("ref", display_reference, reference_onset),
        ("model", model_clip, model_onset),
    ):
        frame_count = min(round(0.060 * rate), len(samples) - onset)
        indexes = np.linspace(
            0, frame_count - 1, min(1600, frame_count), dtype=np.int64
        )
        values = samples[onset + indexes]
        values = values / max(float(np.max(np.abs(values))), 1.0e-12)
        path = svg_path(
            indexes / rate,
            values,
            wave_x,
            wave_y,
            wave_w,
            wave_h,
            0.0,
            0.060,
            -1.0,
            1.0,
        )
        lines.append(f'<path d="{path}" class="{css_class}"/>')

    for css_class, frequency, level in (
        ("ref", ref_frequency, ref_level),
        ("model", model_frequency, model_level),
    ):
        indexes = np.flatnonzero(
            (frequency >= 300.0) & (frequency <= 16_000.0)
        )[::12]
        path = svg_path(
            frequency[indexes],
            np.clip(level[indexes], -60.0, 0.0),
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
        '<text x="60" y="735" class="small">'
        "Exported WAV levels are untouched. Display scaling is used only because "
        "the recording distance and microphone gain are unknown.</text>"
    )
    lines.append("</svg>")
    output.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    ensure_source()
    GENERATED.mkdir(parents=True, exist_ok=True)
    rate, source = read_pcm16(SOURCE_WAV)
    if rate != SAMPLE_RATE:
        raise ValueError(f"Expected {SAMPLE_RATE} Hz, got {rate}")

    onsets = detect_impacts(source, rate)
    clips: list[np.ndarray] = []
    hit_metrics: list[dict] = []
    for hit_number, onset in enumerate(onsets, start=1):
        start = max(0, int(onset - round(CLIP_PRE_S * rate)))
        end = min(len(source), int(onset + round(CLIP_POST_S * rate)))
        clip = source[start:end]
        clips.append(clip)
        write_pcm16(
            GENERATED / f"shuttle_reference_hit{hit_number}.wav",
            rate,
            clip,
        )
        metrics = metrics_with_texture(clip, rate, int(onset) - start)
        metrics["hit_number"] = hit_number
        metrics["source_onset_s"] = round(float(onset) / rate, 6)
        hit_metrics.append(metrics)

    reference_median = median_metrics(hit_metrics)
    model_metrics: dict[str, dict] = {}
    model_clips: dict[int, np.ndarray] = {}
    for speed in MODEL_SPEEDS:
        path = GENERATED / f"shuttle_model_30lb_{speed}mps.wav"
        if not path.exists():
            continue
        model_rate, model_clip = read_pcm16(path)
        if model_rate != rate:
            raise ValueError(f"{path.name} has unexpected rate {model_rate}")
        model_clips[speed] = model_clip
        model_onset = detect_clip_onset(model_clip, rate)
        model_metrics[str(speed)] = metrics_with_texture(
            model_clip, rate, model_onset
        )

    report = {
        "source": {
            "path": str(SOURCE_WAV.relative_to(ROOT)).replace("\\", "/"),
            "sample_rate_hz": rate,
            "duration_s": round(len(source) / rate, 6),
            "normalization_applied": False,
            "detected_onsets_s": [round(float(value) / rate, 6) for value in onsets],
        },
        "reference_hits": hit_metrics,
        "reference_median": reference_median,
        "models_by_speed_mps": model_metrics,
    }
    (GENERATED / "shuttle_analysis.json").write_text(
        json.dumps(report, indent=2) + "\n", encoding="utf-8"
    )

    with (GENERATED / "shuttle_comparison.csv").open(
        "w", newline="", encoding="utf-8"
    ) as file:
        fields = (
            "source",
            "speed_mps",
            "note_peak_hz",
            "attack_centroid_hz",
            "attack_hf_ratio_db",
            "attack_very_hf_ratio_db",
            "spectral_flatness_2_12khz",
            "upper_mode_persistence_db",
            "crest_factor",
            "decay_30db_ms",
        )
        writer = csv.DictWriter(file, fieldnames=fields)
        writer.writeheader()
        writer.writerow(
            {
                "source": "real shuttle median",
                "speed_mps": "",
                **{key: reference_median[key] for key in fields[2:]},
            }
        )
        for speed in MODEL_SPEEDS:
            if str(speed) not in model_metrics:
                continue
            writer.writerow(
                {
                    "source": "model",
                    "speed_mps": speed,
                    **{key: model_metrics[str(speed)][key] for key in fields[2:]},
                }
            )

    markdown = [
        "# Real-shuttle 30 lb calibration",
        "",
        "Eight isolated impacts were detected. Source and model WAV levels are "
        "unchanged; only display spectra are offset for shape comparison.",
        "",
        "| Source | Note | Attack centroid | HF ratio | VHF ratio | "
        "2–12 kHz flatness | Upper persistence | Crest |",
        "|---|---:|---:|---:|---:|---:|---:|---:|",
        f"| Real median | {reference_median['note_peak_hz']:.1f} Hz | "
        f"{reference_median['attack_centroid_hz']:.0f} Hz | "
        f"{reference_median['attack_hf_ratio_db']:+.1f} dB | "
        f"{reference_median['attack_very_hf_ratio_db']:+.1f} dB | "
        f"{reference_median['spectral_flatness_2_12khz']:.3f} | "
        f"{reference_median['upper_mode_persistence_db']:+.1f} dB | "
        f"{reference_median['crest_factor']:.1f} |",
    ]
    for speed in MODEL_SPEEDS:
        metrics = model_metrics.get(str(speed))
        if metrics is None:
            continue
        markdown.append(
            f"| Model {speed} m/s | {metrics['note_peak_hz']:.1f} Hz | "
            f"{metrics['attack_centroid_hz']:.0f} Hz | "
            f"{metrics['attack_hf_ratio_db']:+.1f} dB | "
            f"{metrics['attack_very_hf_ratio_db']:+.1f} dB | "
            f"{metrics['spectral_flatness_2_12khz']:.3f} | "
            f"{metrics['upper_mode_persistence_db']:+.1f} dB | "
            f"{metrics['crest_factor']:.1f} |"
        )
    markdown.extend(
        [
            "",
            "At 5 m/s the model note differs from the real median by "
            f"{model_metrics.get('5', {}).get('note_peak_hz', 0.0) - reference_median['note_peak_hz']:+.1f} Hz, "
            "and its 6–16 kHz balance differs by "
            f"{model_metrics.get('5', {}).get('attack_very_hf_ratio_db', 0.0) - reference_median['attack_very_hf_ratio_db']:+.1f} dB. "
            "Deterministic micro-contact roughness excites the reduced upper "
            "string modes, while a dense residual plus sparse micro-collisions "
            "fills the gaps between those modes.",
            "",
            "The recording contains room/microphone decay, so its long decay "
            "time is not used as a dry-model target. Stable note and attack "
            "spectral shape are the calibration signals.",
            "",
        ]
    )
    (GENERATED / "SHUTTLE_REPORT.md").write_text(
        "\n".join(markdown), encoding="utf-8"
    )

    if 5 in model_clips:
        make_svg(
            clips,
            model_clips[5],
            rate,
            GENERATED / "shuttle_comparison.svg",
        )

    print("Detected shuttle impacts:")
    print(", ".join(f"{value / rate:.4f}s" for value in onsets))
    print(f"Reference median note: {reference_median['note_peak_hz']:.1f} Hz")
    print(
        "Reference median 2-12 kHz flatness: "
        f"{reference_median['spectral_flatness_2_12khz']:.3f}"
    )


if __name__ == "__main__":
    main()
