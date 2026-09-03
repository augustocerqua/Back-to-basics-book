"""Create a nine-second animation of a forecast-based counterfactual method."""

from io import BytesIO
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from PIL import Image


OUTPUT = Path("images/forecast-based-workplace-safety-animation.gif")
FRAME_COUNT = 90
FRAME_DURATIONS_MS = [130, 130, 140] * (FRAME_COUNT // 3)
WIDTH_PX = 1200
HEIGHT_PX = 675
DPI = 120

BACKGROUND = "#FBFAF7"
INK = "#20252B"
MUTED = "#AAB2B9"
GRID = "#E1E4E6"
OBSERVED = "#D4513F"
FORECAST = "#275D8C"
EFFECT = "#25834D"
ACCENT = "#D9A420"
CUTOFF = "#3E454B"


def smoothstep(value):
    value = np.clip(value, 0.0, 1.0)
    return value * value * (3.0 - 2.0 * value)


def progress(frame, start, end):
    return smoothstep((frame - start) / max(end - start, 1))


def build_data():
    rng = np.random.default_rng(123)
    months = np.arange(-36, 25)
    n_firms = 9
    sizes = np.repeat(np.array(["Small", "Medium", "Large"]), 3)
    firm_names = np.array([f"Firm {i}" for i in range(1, n_firms + 1)])

    intercepts = np.array([8.0, 9.1, 10.0, 7.5, 8.8, 9.7, 7.1, 8.2, 9.2])
    slopes = np.array([-0.008, 0.004, -0.003, 0.007, -0.004, 0.003, 0.002, -0.006, 0.005])
    amplitudes = np.array([0.55, 0.70, 0.60, 0.48, 0.62, 0.52, 0.44, 0.58, 0.50])
    phases = np.linspace(0.0, np.pi, n_firms)

    true_no_reform = np.empty((months.size, n_firms))
    observed = np.empty_like(true_no_reform)
    for i in range(n_firms):
        seasonal = amplitudes[i] * np.sin(2 * np.pi * months / 12 + phases[i])
        common_cycle = 0.18 * np.cos(2 * np.pi * months / 24)
        true_no_reform[:, i] = intercepts[i] + slopes[i] * months + seasonal + common_cycle
        observed[:, i] = true_no_reform[:, i] + rng.normal(0, 0.16, months.size)

    post = months >= 0
    size_effect = {"Small": -1.45, "Medium": -2.05, "Large": -2.65}
    for i, size in enumerate(sizes):
        ramp = 1 - np.exp(-(months[post] + 1) / 4.0)
        observed[post, i] += size_effect[size] * ramp

    pre = months < 0
    design = np.column_stack(
        [
            np.ones(months.size),
            months,
            np.sin(2 * np.pi * months / 12),
            np.cos(2 * np.pi * months / 12),
            np.sin(2 * np.pi * months / 24),
            np.cos(2 * np.pi * months / 24),
        ]
    )
    forecasts = np.empty_like(observed)
    for i in range(n_firms):
        coefficients = np.linalg.lstsq(design[pre], observed[pre, i], rcond=None)[0]
        forecasts[:, i] = design @ coefficients

    firm_effects = (observed[post] - forecasts[post]).mean(axis=0)
    overall_effect = firm_effects.mean()
    overall_se = firm_effects.std(ddof=1) / np.sqrt(n_firms)
    overall_ci = overall_effect + np.array([-1, 1]) * 1.96 * overall_se
    group_effects = np.array([firm_effects[sizes == size].mean() for size in ["Small", "Medium", "Large"]])

    return {
        "months": months,
        "observed": observed,
        "forecasts": forecasts,
        "firm_names": firm_names,
        "sizes": sizes,
        "firm_effects": firm_effects,
        "overall_effect": overall_effect,
        "overall_ci": overall_ci,
        "group_effects": group_effects,
    }


def partial_line(ax, x, y, start, end, fraction, **kwargs):
    target = start + (end - start) * fraction
    mask = (x >= start) & (x <= target)
    if mask.sum() >= 2:
        ax.plot(x[mask], y[mask], **kwargs)


def draw_main_panel(ax, frame, data):
    months = data["months"]
    observed = data["observed"]
    forecasts = data["forecasts"]
    pre = months < 0
    post = months >= 0
    focus = 4

    pre_in = progress(frame, 0, 11)
    focus_in = progress(frame, 13, 24)
    forecast_in = progress(frame, 27, 42)
    observed_in = progress(frame, 44, 58)
    gaps_in = progress(frame, 58, 70)

    for i in range(observed.shape[1]):
        alpha = 0.36 * pre_in
        width = 1.15
        color = MUTED
        zorder = 1
        if focus_in > 0 and i == focus:
            alpha = 0.36 * (1 - focus_in) + 1.0 * focus_in
            width = 1.15 * (1 - focus_in) + 3.0 * focus_in
            color = OBSERVED
            zorder = 5
        elif focus_in > 0:
            alpha *= 1 - 0.65 * focus_in
        ax.plot(
            months[pre],
            observed[pre, i],
            color=color,
            linewidth=width,
            alpha=alpha,
            zorder=zorder,
        )

    if forecast_in > 0:
        for i in range(observed.shape[1]):
            alpha = 0.16 + 0.55 * (i == focus)
            width = 1.1 + 1.9 * (i == focus)
            partial_line(
                ax,
                months,
                forecasts[:, i],
                0,
                24,
                forecast_in,
                color=FORECAST,
                linewidth=width,
                linestyle=(0, (7, 4)),
                alpha=alpha,
                zorder=3 + (i == focus),
            )

    if observed_in > 0:
        target = int(np.floor(24 * observed_in))
        shown = post & (months <= target)
        for i in range(observed.shape[1]):
            alpha = 0.22 + 0.76 * (i == focus)
            width = 1.2 + 1.8 * (i == focus)
            if shown.sum() >= 2:
                ax.plot(
                    months[shown],
                    observed[shown, i],
                    color=OBSERVED,
                    linewidth=width,
                    alpha=alpha,
                    zorder=5 + (i == focus),
                )

    if gaps_in > 0:
        target = int(np.floor(24 * gaps_in))
        shown = post & (months <= target)
        if shown.sum() >= 2:
            ax.fill_between(
                months[shown],
                observed[shown, focus],
                forecasts[shown, focus],
                color=EFFECT,
                alpha=0.18 * gaps_in,
                zorder=2,
            )

    ax.axvline(0, color=CUTOFF, linewidth=2.0, linestyle=(0, (5, 5)), zorder=2)
    ax.text(0, 11.18, "National reform", ha="center", va="bottom", fontsize=10, weight="bold", color=CUTOFF)

    if frame >= 13:
        ax.text(-34, 10.92, "Observed accident rate", color=OBSERVED, fontsize=10, weight="bold")
    if frame >= 27:
        ax.text(8, 10.92, "Forecast: no reform", color=FORECAST, fontsize=10, weight="bold")
    if frame >= 58:
        ax.annotate(
            "Firm-level gap",
            xy=(17, (observed[months == 17, focus][0] + forecasts[months == 17, focus][0]) / 2),
            xytext=(8, 6.2),
            color=EFFECT,
            fontsize=10,
            weight="bold",
            arrowprops=dict(arrowstyle="->", color=EFFECT, linewidth=1.6),
        )

    ax.set_xlim(-36, 24)
    ax.set_ylim(4.9, 11.7)
    ax.set_xticks([-36, -24, -12, 0, 12, 24])
    ax.set_xticklabels(["-36", "-24", "-12", "0", "+12", "+24"])
    ax.set_yticks([5, 7, 9, 11])
    ax.set_xlabel("Months relative to reform", fontsize=11, weight="bold", color=INK)
    ax.set_ylabel("Accidents per 1,000 workers", fontsize=11, weight="bold", color=INK)
    ax.grid(True, color=GRID, linewidth=0.8)
    ax.tick_params(colors="#5C646B", labelsize=9)
    for spine in ax.spines.values():
        spine.set_visible(False)


def draw_summary_panel(ax, frame, data):
    if frame < 59:
        x = np.tile(np.arange(3), 3)
        y = np.repeat(np.arange(3), 3)
        ax.scatter(
            x,
            y,
            s=170,
            color=ACCENT,
            edgecolor="white",
            linewidth=1.2,
            alpha=0.88,
        )
        ax.set_xlim(-0.7, 2.7)
        ax.set_ylim(-1.25, 3.15)
        ax.text(1, 2.85, "All firms treated", ha="center", va="bottom", fontsize=11, weight="bold", color=INK)
        ax.text(1, -0.72, "No untreated group", ha="center", va="top", fontsize=10, weight="bold", color=OBSERVED)
        ax.axis("off")
        return

    firm_in = progress(frame, 59, 73)
    group_in = progress(frame, 75, 86)
    effects = data["firm_effects"]

    ax.axvline(0, color=MUTED, linewidth=1.5, linestyle=(0, (4, 4)), zorder=1)

    if group_in < 0.55:
        y = np.arange(effects.size, 0, -1)
        for i, (value, ypos) in enumerate(zip(effects, y)):
            reveal = progress(firm_in, i / effects.size, min(1.0, (i + 2) / effects.size))
            if reveal > 0:
                ax.plot([0, value * reveal], [ypos, ypos], color=GRID, linewidth=1.5, zorder=1)
                ax.scatter(value * reveal, ypos, s=44, color=ACCENT, edgecolor="white", linewidth=0.7, zorder=3)
        ax.set_yticks(y)
        ax.set_yticklabels(data["firm_names"], fontsize=8)
        ax.set_title("Firm-level effects", fontsize=11, weight="bold", color=INK, pad=8)
    else:
        labels = np.array(["All firms", "Small", "Medium", "Large"])
        values = np.r_[data["overall_effect"], data["group_effects"]]
        y = np.arange(4, 0, -1)
        for i, (value, ypos) in enumerate(zip(values, y)):
            reveal = progress(group_in, 0.50 + i * 0.10, 0.68 + i * 0.10)
            if reveal > 0:
                color = EFFECT if i == 0 else ACCENT
                ax.plot([0, value * reveal], [ypos, ypos], color=GRID, linewidth=1.5, zorder=1)
                ax.scatter(value * reveal, ypos, s=70 if i == 0 else 52, color=color, edgecolor="white", linewidth=0.8, zorder=3)
        if group_in > 0.82:
            lo, hi = data["overall_ci"]
            ax.plot([lo, hi], [4, 4], color=EFFECT, linewidth=2.2, zorder=2)
            ax.plot([lo, lo], [3.88, 4.12], color=EFFECT, linewidth=1.6)
            ax.plot([hi, hi], [3.88, 4.12], color=EFFECT, linewidth=1.6)
        ax.set_yticks(y)
        ax.set_yticklabels(labels, fontsize=9)
        ax.set_ylim(0.4, 4.6)
        ax.set_title("Overall and by firm size", fontsize=11, weight="bold", color=INK, pad=8)

    ax.set_xlim(-3.5, 0.35)
    ax.set_xlabel("Change in accident rate", fontsize=10, weight="bold", color=INK)
    ax.grid(axis="x", color=GRID, linewidth=0.8)
    ax.tick_params(colors="#5C646B", labelsize=8)
    for spine in ax.spines.values():
        spine.set_visible(False)


def render_frame(frame, data):
    fig = plt.figure(figsize=(WIDTH_PX / DPI, HEIGHT_PX / DPI), dpi=DPI, facecolor=BACKGROUND)
    ax = fig.add_axes([0.075, 0.17, 0.66, 0.68], facecolor=BACKGROUND)
    sax = fig.add_axes([0.79, 0.21, 0.18, 0.58], facecolor=BACKGROUND)

    draw_main_panel(ax, frame, data)
    draw_summary_panel(sax, frame, data)

    if frame < 13:
        title = "One national reform, no contemporaneous control group"
    elif frame < 27:
        title = "Pre-reform outcomes reveal each firm's untreated dynamics"
    elif frame < 44:
        title = "A model forecasts each firm's no-reform path"
    elif frame < 59:
        title = "Observed outcomes are compared with the forecasts"
    elif frame < 75:
        title = "Firm-level gaps are averaged across firms"
    else:
        title = "Average effects and heterogeneity summarize the evidence"

    fig.suptitle(title, x=0.5, y=0.958, fontsize=18, weight="bold", color=INK)

    if frame < 75:
        footer = "FBCM learns the counterfactual from outcomes observed before the reform."
        color = "#687179"
    else:
        footer = "Interpretation requires that no concurrent shock explains the post-reform gap."
        color = OBSERVED
    fig.text(0.5, 0.055, footer, ha="center", va="center", fontsize=10.5, color=color, weight="bold")

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
