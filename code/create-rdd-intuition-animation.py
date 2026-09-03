"""Create a six-second animated explanation of a sharp RD design."""

from io import BytesIO
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from PIL import Image


OUTPUT = Path("images/rdd-intuition-animation.gif")
FRAME_COUNT = 60
FRAME_DURATION_MS = 100
WIDTH_PX = 1200
HEIGHT_PX = 675
DPI = 120

BACKGROUND = "#FBFAF7"
INK = "#20252B"
MUTED = "#AAB2B9"
GRID = "#E1E4E6"
CONTROL = "#2C78B8"
TREATED = "#E05A3F"
EFFECT = "#25834D"
CUTOFF = "#343A40"


def smoothstep(value):
    value = np.clip(value, 0.0, 1.0)
    return value * value * (3.0 - 2.0 * value)


def phase_progress(frame, start, end):
    return smoothstep((frame - start) / max(end - start, 1))


def build_data():
    rng = np.random.default_rng(123)
    running = np.sort(rng.uniform(-1.25, 1.25, 112))
    treatment = running >= 0
    untreated_outcome = 50 + 8 * running + 1.8 * running**2
    observed_outcome = (
        untreated_outcome
        + 10 * treatment.astype(float)
        + rng.normal(0, 1.65, running.size)
    )

    bandwidth = 0.58
    left = (running < 0) & (running >= -bandwidth)
    right = (running >= 0) & (running <= bandwidth)
    left_coef = np.polyfit(running[left], observed_outcome[left], 1)
    right_coef = np.polyfit(running[right], observed_outcome[right], 1)

    return {
        "running": running,
        "treatment": treatment,
        "outcome": observed_outcome,
        "bandwidth": bandwidth,
        "left_coef": left_coef,
        "right_coef": right_coef,
    }


def render_frame(frame, data):
    running = data["running"]
    treatment = data["treatment"]
    outcome = data["outcome"]
    bandwidth = data["bandwidth"]

    points_in = phase_progress(frame, 0, 9)
    cutoff_in = phase_progress(frame, 10, 20)
    local_in = phase_progress(frame, 22, 32)
    fits_in = phase_progress(frame, 34, 45)
    effect_in = phase_progress(frame, 47, 54)

    fig, ax = plt.subplots(
        figsize=(WIDTH_PX / DPI, HEIGHT_PX / DPI),
        dpi=DPI,
        facecolor=BACKGROUND,
    )
    ax.set_facecolor(BACKGROUND)

    if cutoff_in > 0:
        ax.axvspan(
            -1.35,
            0,
            color=CONTROL,
            alpha=0.045 * cutoff_in,
            zorder=0,
        )
        ax.axvspan(
            0,
            1.35,
            color=TREATED,
            alpha=0.05 * cutoff_in,
            zorder=0,
        )

    if local_in > 0:
        ax.axvspan(
            -bandwidth,
            bandwidth,
            color="#F2C94C",
            alpha=0.12 * local_in,
            zorder=0,
        )

    far_from_cutoff = np.abs(running) > bandwidth
    local_points = ~far_from_cutoff

    base_colors = np.where(treatment, TREATED, CONTROL)
    initial_color = np.array([MUTED] * running.size, dtype=object)
    point_colors = initial_color.copy()
    if cutoff_in > 0.05:
        point_colors = base_colors

    alphas = np.full(running.size, 0.92 * points_in)
    sizes = np.full(running.size, 42.0)
    if local_in > 0:
        alphas[far_from_cutoff] *= 1.0 - 0.78 * local_in
        sizes[local_points] = 42 + 34 * local_in

    ax.scatter(
        running,
        outcome,
        s=sizes,
        c=point_colors,
        alpha=alphas,
        edgecolors="white",
        linewidths=0.65,
        zorder=3,
    )

    if cutoff_in > 0:
        ax.axvline(
            0,
            color=CUTOFF,
            linewidth=2.0,
            linestyle=(0, (5, 5)),
            alpha=cutoff_in,
            zorder=2,
        )
        ax.text(
            0,
            68.7,
            "Cutoff",
            ha="center",
            va="bottom",
            fontsize=13,
            fontweight="bold",
            color=CUTOFF,
            alpha=cutoff_in,
        )
        ax.text(
            -0.76,
            67.2,
            "No scholarship",
            ha="center",
            va="center",
            fontsize=12.5,
            fontweight="bold",
            color=CONTROL,
            alpha=cutoff_in,
        )
        ax.text(
            0.76,
            67.2,
            "Scholarship",
            ha="center",
            va="center",
            fontsize=12.5,
            fontweight="bold",
            color=TREATED,
            alpha=cutoff_in,
        )

    left_intercept = np.polyval(data["left_coef"], 0)
    right_intercept = np.polyval(data["right_coef"], 0)

    if fits_in > 0:
        left_end = -bandwidth + bandwidth * fits_in
        right_end = bandwidth * fits_in
        left_grid = np.linspace(-bandwidth, left_end, 80)
        right_grid = np.linspace(0, right_end, 80)
        ax.plot(
            left_grid,
            np.polyval(data["left_coef"], left_grid),
            color=CONTROL,
            linewidth=3.2,
            zorder=4,
        )
        ax.plot(
            right_grid,
            np.polyval(data["right_coef"], right_grid),
            color=TREATED,
            linewidth=3.2,
            zorder=4,
        )

    if effect_in > 0:
        ax.scatter(
            [0, 0],
            [left_intercept, right_intercept],
            s=72,
            color=[CONTROL, TREATED],
            edgecolor="white",
            linewidth=0.9,
            alpha=effect_in,
            zorder=6,
        )
        ax.annotate(
            "",
            xy=(0.045, right_intercept),
            xytext=(0.045, left_intercept),
            arrowprops=dict(
                arrowstyle="<->",
                color=EFFECT,
                lw=3.0,
                shrinkA=0,
                shrinkB=0,
                alpha=effect_in,
            ),
            zorder=5,
        )
        ax.text(
            0.10,
            (left_intercept + right_intercept) / 2,
            "RD effect\nat the cutoff",
            ha="left",
            va="center",
            fontsize=13.5,
            fontweight="bold",
            color=EFFECT,
            alpha=effect_in,
            zorder=6,
        )

    if frame < 10:
        title = "Units have a running variable and an outcome"
    elif frame < 22:
        title = "Crossing the cutoff determines treatment"
    elif frame < 34:
        title = "Compare units just below and just above the cutoff"
    elif frame < 47:
        title = "Estimate the outcome relationship on both sides"
    else:
        title = "The discontinuity identifies a local causal effect"

    fig.suptitle(
        title,
        x=0.5,
        y=0.965,
        fontsize=21,
        fontweight="bold",
        color=INK,
    )

    ax.set_xlim(-1.34, 1.34)
    ax.set_ylim(37.5, 70.5)
    ax.set_xlabel("Running variable", fontsize=15, fontweight="bold", color=INK)
    ax.set_ylabel("Outcome", fontsize=15, fontweight="bold", color=INK)
    ax.set_xticks([-1.0, -0.5, 0, 0.5, 1.0])
    ax.set_xticklabels(["-1.0", "-0.5", "0", "0.5", "1.0"])
    ax.set_yticks([40, 50, 60, 70])
    ax.tick_params(axis="both", labelsize=12, colors="#4E555B", length=0)
    ax.grid(True, color=GRID, linewidth=1.0, alpha=0.9)
    ax.set_axisbelow(True)
    for spine in ax.spines.values():
        spine.set_visible(False)

    fig.subplots_adjust(left=0.09, right=0.975, bottom=0.14, top=0.86)

    buffer = BytesIO()
    fig.savefig(
        buffer,
        format="png",
        dpi=DPI,
        facecolor=BACKGROUND,
        bbox_inches=None,
    )
    plt.close(fig)
    buffer.seek(0)
    return Image.open(buffer).convert("RGB")


def main():
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    data = build_data()
    frames = [render_frame(frame, data) for frame in range(FRAME_COUNT)]

    # Use a shared adaptive palette to limit file size while preserving the
    # book's restrained color palette.
    palette_source = frames[-1].convert("P", palette=Image.Palette.ADAPTIVE, colors=128)
    palette = palette_source.getpalette()
    gif_frames = []
    for frame in frames:
        paletted = frame.quantize(palette=palette_source, dither=Image.Dither.FLOYDSTEINBERG)
        paletted.putpalette(palette)
        gif_frames.append(paletted)

    gif_frames[0].save(
        OUTPUT,
        save_all=True,
        append_images=gif_frames[1:],
        duration=FRAME_DURATION_MS,
        loop=0,
        optimize=True,
        disposal=2,
    )


if __name__ == "__main__":
    main()
