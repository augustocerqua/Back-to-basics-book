"""Create a twelve-second worked example explaining an honest causal tree."""

from io import BytesIO
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
import numpy as np
from PIL import Image


OUTPUT = Path("images/causal-tree-energy-worked-example.gif")
FRAME_COUNT = 90
FRAME_DURATIONS_MS = [130, 130, 140] * (FRAME_COUNT // 3)
WIDTH_PX = 1200
HEIGHT_PX = 675
DPI = 120

BACKGROUND = "#FBFAF7"
INK = "#20252B"
MUTED = "#AAB2B9"
GRID = "#E1E4E6"
TREATED = "#D4513F"
CONTROL = "#275D8C"
SPLIT = "#D9A420"
EFFECT = "#25834D"
NODE_FILL = "#F4F0E5"


def smoothstep(value):
    value = np.clip(value, 0.0, 1.0)
    return value * value * (3.0 - 2.0 * value)


def progress(frame, start, end):
    return smoothstep((frame - start) / max(end - start, 1))


def build_data():
    rng = np.random.default_rng(123)
    n = 1600
    baseline_use = rng.uniform(2000, 6500, n)
    dwelling_age = rng.uniform(1, 50, n)
    treatment = rng.binomial(1, 0.5, n)

    true_effect = np.where(
        baseline_use <= 4000,
        80.0,
        np.where(dwelling_age <= 20, 210.0, 420.0),
    )
    untreated_savings = (
        25
        + 0.006 * (baseline_use - 4000)
        + 0.35 * dwelling_age
        + rng.normal(0, 105, n)
    )
    observed_savings = untreated_savings + treatment * true_effect

    discovery = rng.random(n) < 0.5
    estimation = ~discovery
    leaf = np.select(
        [
            baseline_use <= 4000,
            (baseline_use > 4000) & (dwelling_age <= 20),
            (baseline_use > 4000) & (dwelling_age > 20),
        ],
        [0, 1, 2],
    )

    estimates = []
    intervals = []
    leaf_counts = []
    for value in range(3):
        mask = estimation & (leaf == value)
        treated_outcomes = observed_savings[mask & (treatment == 1)]
        control_outcomes = observed_savings[mask & (treatment == 0)]
        estimate = treated_outcomes.mean() - control_outcomes.mean()
        se = np.sqrt(
            treated_outcomes.var(ddof=1) / treated_outcomes.size
            + control_outcomes.var(ddof=1) / control_outcomes.size
        )
        estimates.append(estimate)
        intervals.append((estimate - 1.96 * se, estimate + 1.96 * se))
        leaf_counts.append(mask.sum())

    display_rng = np.random.default_rng(321)
    display_indices = display_rng.choice(np.flatnonzero(discovery), size=150, replace=False)

    return {
        "baseline_use": baseline_use,
        "dwelling_age": dwelling_age,
        "treatment": treatment,
        "display_indices": display_indices,
        "estimates": np.array(estimates),
        "intervals": np.array(intervals),
        "leaf_counts": np.array(leaf_counts),
    }


def rounded_box(ax, center, width, height, text, edge, fill, alpha=1.0, fontsize=9.5, linewidth=1.8):
    x, y = center
    patch = FancyBboxPatch(
        (x - width / 2, y - height / 2),
        width,
        height,
        boxstyle="round,pad=0.015,rounding_size=0.025",
        linewidth=linewidth,
        edgecolor=edge,
        facecolor=fill,
        alpha=alpha,
        transform=ax.transAxes,
        zorder=4,
    )
    ax.add_patch(patch)
    ax.text(
        x,
        y,
        text,
        transform=ax.transAxes,
        ha="center",
        va="center",
        fontsize=fontsize,
        weight="bold",
        color=INK,
        alpha=alpha,
        zorder=5,
    )


def branch(ax, start, end, alpha=1.0, label=None, label_offset=(0, 0)):
    ax.plot(
        [start[0], end[0]],
        [start[1], end[1]],
        transform=ax.transAxes,
        color="#697178",
        linewidth=2.0,
        alpha=alpha,
        zorder=2,
    )
    if label:
        midpoint = ((start[0] + end[0]) / 2 + label_offset[0], (start[1] + end[1]) / 2 + label_offset[1])
        ax.text(
            midpoint[0],
            midpoint[1],
            label,
            transform=ax.transAxes,
            fontsize=8.5,
            weight="bold",
            color="#697178",
            ha="center",
            va="center",
            alpha=alpha,
            bbox=dict(facecolor=BACKGROUND, edgecolor="none", pad=1.2),
            zorder=3,
        )


def draw_sample_split(ax, alpha):
    ax.axis("off")
    rounded_box(
        ax,
        (0.50, 0.76),
        0.72,
        0.17,
        "Randomized sample",
        edge=INK,
        fill=NODE_FILL,
        alpha=alpha,
        fontsize=11,
    )
    branch(ax, (0.50, 0.67), (0.28, 0.48), alpha=alpha)
    branch(ax, (0.50, 0.67), (0.72, 0.48), alpha=alpha)
    rounded_box(
        ax,
        (0.28, 0.37),
        0.38,
        0.20,
        "DISCOVERY SAMPLE\nchoose the splits",
        edge=SPLIT,
        fill="#FBF3D9",
        alpha=alpha,
        fontsize=10,
    )
    rounded_box(
        ax,
        (0.72, 0.37),
        0.38,
        0.20,
        "ESTIMATION SAMPLE\nestimate leaf effects",
        edge=EFFECT,
        fill="#E7F3EB",
        alpha=alpha,
        fontsize=10,
    )
    ax.text(
        0.5,
        0.12,
        "Discovery and estimation use different households",
        transform=ax.transAxes,
        ha="center",
        va="center",
        fontsize=9.5,
        color="#697178",
        weight="bold",
        alpha=alpha,
    )


def leaf_text(label, estimate=None, interval=None):
    if estimate is None:
        return label
    return f"{label}\nEstimated effect\n+{estimate:.0f} kWh"


def draw_tree(ax, frame, data):
    ax.axis("off")
    first_split = progress(frame, 30, 43)
    second_split = progress(frame, 45, 58)
    estimates_in = progress(frame, 60, 75)
    highlight = progress(frame, 76, 87)

    rounded_box(
        ax,
        (0.50, 0.86),
        0.50,
        0.15,
        "Baseline use\n> 4,000 kWh?",
        edge=SPLIT,
        fill="#FBF3D9",
        alpha=first_split,
        fontsize=10.5,
    )
    branch(ax, (0.50, 0.785), (0.23, 0.59), alpha=first_split, label="No", label_offset=(-0.025, 0.015))
    branch(ax, (0.50, 0.785), (0.70, 0.59), alpha=first_split, label="Yes", label_offset=(0.025, 0.015))

    low_text = leaf_text(
        "LOW USE",
        data["estimates"][0] if estimates_in > 0.4 else None,
        data["intervals"][0] if estimates_in > 0.4 else None,
    )
    rounded_box(
        ax,
        (0.23, 0.48),
        0.37,
        0.19 if estimates_in > 0.4 else 0.14,
        low_text,
        edge=EFFECT if estimates_in > 0.4 else CONTROL,
        fill="#E7F3EB" if estimates_in > 0.4 else "#EAF0F6",
        alpha=first_split,
        fontsize=9.0 if estimates_in > 0.4 else 10,
    )

    rounded_box(
        ax,
        (0.70, 0.53),
        0.46,
        0.15,
        "Dwelling age\n> 20 years?",
        edge=SPLIT,
        fill="#FBF3D9",
        alpha=first_split,
        fontsize=10,
    )

    branch(ax, (0.70, 0.45), (0.53, 0.26), alpha=second_split, label="No", label_offset=(-0.025, 0.005))
    branch(ax, (0.70, 0.45), (0.85, 0.26), alpha=second_split, label="Yes", label_offset=(0.025, 0.005))

    newer_text = leaf_text(
        "HIGH USE, NEWER",
        data["estimates"][1] if estimates_in > 0.4 else None,
        data["intervals"][1] if estimates_in > 0.4 else None,
    )
    older_text = leaf_text(
        "HIGH USE, OLDER",
        data["estimates"][2] if estimates_in > 0.4 else None,
        data["intervals"][2] if estimates_in > 0.4 else None,
    )
    rounded_box(
        ax,
        (0.53, 0.14),
        0.29,
        0.21 if estimates_in > 0.4 else 0.17,
        newer_text,
        edge=EFFECT if estimates_in > 0.4 else CONTROL,
        fill="#E7F3EB" if estimates_in > 0.4 else "#EAF0F6",
        alpha=second_split,
        fontsize=9.0 if estimates_in > 0.4 else 9,
    )
    rounded_box(
        ax,
        (0.85, 0.14),
        0.29,
        0.21 if estimates_in > 0.4 else 0.17,
        older_text,
        edge=EFFECT,
        fill="#D6EDDE" if highlight > 0 else ("#E7F3EB" if estimates_in > 0.4 else "#EAF0F6"),
        alpha=second_split,
        fontsize=9.0 if estimates_in > 0.4 else 9,
        linewidth=1.8 + 2.0 * highlight,
    )


def draw_covariate_panel(ax, frame, data):
    indices = data["display_indices"]
    use = data["baseline_use"][indices] / 1000
    age = data["dwelling_age"][indices]
    treatment = data["treatment"][indices]

    points_in = 0.20 + 0.80 * progress(frame, 0, 12)
    first_split = progress(frame, 30, 43)
    second_split = progress(frame, 45, 58)
    highlight = progress(frame, 76, 87)

    if highlight > 0:
        ax.axvspan(4.0, 6.5, ymin=20 / 55, ymax=1.0, color=EFFECT, alpha=0.10 * highlight, zorder=0)

    for status, color, label in [(0, CONTROL, "Control"), (1, TREATED, "Energy advice")]:
        mask = treatment == status
        ax.scatter(
            use[mask],
            age[mask],
            s=34,
            color=color,
            alpha=0.72 * points_in,
            edgecolor="white",
            linewidth=0.35,
            label=label,
            zorder=2,
        )

    if first_split > 0:
        ax.plot([4, 4], [0, 55], color=SPLIT, linewidth=2.5, linestyle=(0, (6, 4)), alpha=first_split, zorder=3)
        ax.text(3.92, 52, "First split", ha="right", va="top", color=SPLIT, fontsize=9.5, weight="bold", alpha=first_split)
    if second_split > 0:
        ax.plot([4, 6.5], [20, 20], color=SPLIT, linewidth=2.5, linestyle=(0, (6, 4)), alpha=second_split, zorder=3)
        ax.text(6.42, 21.4, "Second split", ha="right", va="bottom", color=SPLIT, fontsize=9.5, weight="bold", alpha=second_split)
    if highlight > 0:
        ax.text(5.25, 46, "Largest estimated effect", ha="center", va="center", color=EFFECT, fontsize=10.5, weight="bold", alpha=highlight)

    ax.set_xlim(2.0, 6.5)
    ax.set_ylim(0, 55)
    ax.set_xticks([2, 3, 4, 5, 6])
    ax.set_yticks([0, 10, 20, 30, 40, 50])
    ax.set_xlabel("Baseline electricity use (thousand kWh)", fontsize=10.5, weight="bold", color=INK)
    ax.set_ylabel("Dwelling age (years)", fontsize=10.5, weight="bold", color=INK)
    ax.grid(True, color=GRID, linewidth=0.8)
    ax.tick_params(colors="#5C646B", labelsize=9)
    ax.legend(loc="lower left", frameon=False, fontsize=9, ncol=2, handletextpad=0.3, columnspacing=0.8)
    for spine in ax.spines.values():
        spine.set_visible(False)


def render_frame(frame, data):
    fig = plt.figure(figsize=(WIDTH_PX / DPI, HEIGHT_PX / DPI), dpi=DPI, facecolor=BACKGROUND)
    ax = fig.add_axes([0.065, 0.17, 0.44, 0.68], facecolor=BACKGROUND)
    tax = fig.add_axes([0.54, 0.17, 0.42, 0.68], facecolor=BACKGROUND)

    draw_covariate_panel(ax, frame, data)

    if 15 <= frame < 30:
        draw_sample_split(tax, progress(frame, 15, 26))
    elif frame >= 30:
        draw_tree(tax, frame, data)
    else:
        tax.axis("off")
        tax.text(0.5, 0.58, "Random assignment", ha="center", va="center", transform=tax.transAxes, fontsize=13, weight="bold", color=INK)
        tax.text(0.5, 0.45, "makes treatment and control\ncomparable on average", ha="center", va="center", transform=tax.transAxes, fontsize=11, color="#697178", linespacing=1.4)

    if frame < 15:
        title = "Randomize households to energy advice or control"
    elif frame < 30:
        title = "Separate discovery from effect estimation"
    elif frame < 45:
        title = "First split: baseline electricity use"
    elif frame < 60:
        title = "Second split: dwelling age among high-use homes"
    elif frame < 76:
        title = "Estimate each leaf effect in the untouched sample"
    else:
        title = "Compare effects across the final leaves"

    fig.suptitle(title, x=0.5, y=0.958, fontsize=18, weight="bold", color=INK)

    if frame < 15:
        footer = "Outcome: annual electricity savings. Blue = control; red = energy advice."
        color = "#697178"
    elif frame < 60:
        footer = "The tree searches for subgroups with different treatment-control contrasts."
        color = "#697178"
    elif frame < 76:
        footer = "Leaf effect = mean savings under advice - mean savings under control."
        color = EFFECT
    else:
        footer = "The tree discovers heterogeneity; randomization identifies the causal effects."
        color = EFFECT
    fig.text(0.5, 0.055, footer, ha="center", va="center", fontsize=10.3, color=color, weight="bold")

    buffer = BytesIO()
    fig.savefig(buffer, format="png", dpi=DPI, facecolor=BACKGROUND)
    plt.close(fig)
    buffer.seek(0)
    return Image.open(buffer).convert("RGB")


def main():
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    data = build_data()
    frames = [render_frame(frame, data) for frame in range(FRAME_COUNT)]

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
        duration=FRAME_DURATIONS_MS,
        loop=0,
        optimize=True,
        disposal=2,
    )


if __name__ == "__main__":
    main()
