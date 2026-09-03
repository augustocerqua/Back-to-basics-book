"""Create a nine-second animated explanation of synthetic control."""

from io import BytesIO
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from PIL import Image


OUTPUT = Path("images/scm-germany-reunification-animation.gif")
FRAME_COUNT = 90
FRAME_DURATION_MS = 100
WIDTH_PX = 1200
HEIGHT_PX = 675
DPI = 120

BACKGROUND = "#FBFAF7"
INK = "#20252B"
MUTED = "#AAB2B9"
GRID = "#E1E4E6"
TREATED = "#D4513F"
SYNTHETIC = "#275D8C"
WEIGHT = "#D9A420"
EFFECT = "#25834D"
CUTOFF = "#3E454B"


def smoothstep(value):
    value = np.clip(value, 0.0, 1.0)
    return value * value * (3.0 - 2.0 * value)


def progress(frame, start, end):
    return smoothstep((frame - start) / max(end - start, 1))


def build_data():
    years = np.arange(1960, 2004)
    time = years - years[0]
    donor_names = np.array(
        [
            "Austria",
            "United States",
            "Japan",
            "Switzerland",
            "Netherlands",
            "Canada",
            "France",
            "Australia",
        ]
    )
    weights = np.array([0.36, 0.22, 0.16, 0.14, 0.12, 0.0, 0.0, 0.0])

    rng = np.random.default_rng(123)
    intercepts = np.array([-2.0, 5.0, -7.0, 8.0, 0.0, 3.0, -1.0, 4.0])
    slopes = np.array([0.05, 0.18, 0.28, -0.03, 0.10, 0.14, 0.07, 0.12])
    phases = np.linspace(0, np.pi, donor_names.size)

    donors = []
    for index in range(donor_names.size):
        path = (
            62
            + intercepts[index]
            + (0.92 + slopes[index]) * time
            + 1.2 * np.sin(time / 5 + phases[index])
            + 0.35 * np.sin(time / 2.7 + phases[index] / 2)
            + rng.normal(0, 0.12, years.size)
        )
        donors.append(path)
    donors = np.column_stack(donors)

    synthetic = donors @ weights
    west_counterfactual = synthetic + rng.normal(0, 0.16, years.size)

    reunification_effect = np.zeros(years.size)
    post = years >= 1990
    reunification_effect[post] = -np.linspace(0.4, 12.0, post.sum())
    observed_west = west_counterfactual + reunification_effect

    return {
        "years": years,
        "donor_names": donor_names,
        "donors": donors,
        "weights": weights,
        "synthetic": synthetic,
        "observed_west": observed_west,
        "counterfactual": west_counterfactual,
        "effect": reunification_effect,
    }


def partial_line(ax, x, y, start_year, end_year, fraction, **kwargs):
    target_year = start_year + (end_year - start_year) * fraction
    mask = (x >= start_year) & (x <= target_year)
    if mask.sum() >= 2:
        ax.plot(x[mask], y[mask], **kwargs)


def render_frame(frame, data):
    donors_in = progress(frame, 0, 11)
    weights_in = progress(frame, 13, 27)
    synthetic_in = progress(frame, 29, 42)
    event_in = progress(frame, 44, 53)
    post_in = progress(frame, 54, 69)
    effect_in = progress(frame, 71, 80)

    years = data["years"]
    donors = data["donors"]
    weights = data["weights"]
    pre_mask = years < 1990
    post_mask = years >= 1990

    fig = plt.figure(
        figsize=(WIDTH_PX / DPI, HEIGHT_PX / DPI),
        dpi=DPI,
        facecolor=BACKGROUND,
    )
    ax = fig.add_axes([0.075, 0.15, 0.67, 0.70], facecolor=BACKGROUND)
    wax = fig.add_axes([0.785, 0.18, 0.19, 0.64], facecolor=BACKGROUND)

    donor_alpha = 0.48 * donors_in
    for index, name in enumerate(data["donor_names"]):
        if weights_in > 0:
            target_alpha = 0.10 + 0.55 * (weights[index] / weights.max())
            alpha = donor_alpha * (1 - weights_in) + target_alpha * weights_in
            linewidth = 1.0 + 1.2 * (weights[index] / weights.max()) * weights_in
        else:
            alpha = donor_alpha
            linewidth = 1.15

        ax.plot(
            years[pre_mask],
            donors[pre_mask, index],
            color=MUTED,
            linewidth=linewidth,
            alpha=alpha,
            zorder=1,
        )

    ax.plot(
        years[pre_mask],
        data["observed_west"][pre_mask],
        color=TREATED,
        linewidth=3.0,
        alpha=donors_in,
        label="West Germany",
        zorder=4,
    )

    if synthetic_in > 0:
        partial_line(
            ax,
            years,
            data["synthetic"],
            1960,
            1989,
            synthetic_in,
            color=SYNTHETIC,
            linewidth=3.0,
            linestyle=(0, (7, 4)),
            label="Synthetic West Germany",
            zorder=5,
        )

    if event_in > 0:
        ax.axvline(
            1990,
            color=CUTOFF,
            linewidth=2.0,
            linestyle=(0, (5, 5)),
            alpha=event_in,
            zorder=2,
        )
        ax.text(
            1990,
            109.0,
            "1990\nReunification",
            ha="center",
            va="bottom",
            fontsize=12.5,
            fontweight="bold",
            color=CUTOFF,
            alpha=event_in,
        )

    if post_in > 0:
        partial_line(
            ax,
            years,
            data["synthetic"],
            1990,
            2003,
            post_in,
            color=SYNTHETIC,
            linewidth=3.0,
            linestyle=(0, (7, 4)),
            zorder=5,
        )
        partial_line(
            ax,
            years,
            data["observed_west"],
            1990,
            2003,
            post_in,
            color=TREATED,
            linewidth=3.0,
            zorder=5,
        )

    if effect_in > 0:
        completed_year = int(round(1990 + 13 * post_in))
        shade_mask = (years >= 1990) & (years <= completed_year)
        ax.fill_between(
            years[shade_mask],
            data["observed_west"][shade_mask],
            data["synthetic"][shade_mask],
            color=EFFECT,
            alpha=0.13 * effect_in,
            zorder=2,
        )

        final_x = 2003
        actual_final = data["observed_west"][-1]
        synthetic_final = data["synthetic"][-1]
        ax.annotate(
            "",
            xy=(final_x - 0.25, synthetic_final),
            xytext=(final_x - 0.25, actual_final),
            arrowprops=dict(
                arrowstyle="<->",
                color=EFFECT,
                lw=2.8,
                alpha=effect_in,
            ),
            zorder=7,
        )
        ax.text(
            2001.8,
            (actual_final + synthetic_final) / 2,
            "Estimated\neffect",
            ha="right",
            va="center",
            fontsize=12.5,
            fontweight="bold",
            color=EFFECT,
            alpha=effect_in,
            zorder=7,
        )

    ax.text(
        1961,
        106.5,
        "Treated unit",
        fontsize=11.5,
        fontweight="bold",
        color=TREATED,
        alpha=donors_in,
    )
    if synthetic_in > 0.2:
        ax.text(
            1961,
            102.7,
            "Weighted donor average",
            fontsize=11.5,
            fontweight="bold",
            color=SYNTHETIC,
            alpha=synthetic_in,
        )

    ax.set_xlim(1959, 2005)
    ax.set_ylim(55, 112)
    ax.set_xlabel("Year", fontsize=14, fontweight="bold", color=INK)
    ax.set_ylabel("GDP per capita index", fontsize=14, fontweight="bold", color=INK)
    ax.set_xticks([1960, 1970, 1980, 1990, 2000])
    ax.set_yticks([60, 70, 80, 90, 100, 110])
    ax.tick_params(axis="both", labelsize=11.5, colors="#4E555B", length=0)
    ax.grid(True, color=GRID, linewidth=1.0)
    ax.set_axisbelow(True)
    for spine in ax.spines.values():
        spine.set_visible(False)

    # Donor-weight panel.
    order = np.arange(data["donor_names"].size)[::-1]
    displayed_weights = weights * weights_in
    wax.barh(
        order,
        displayed_weights,
        color=WEIGHT,
        alpha=0.90 * max(weights_in, 0.08 * donors_in),
        height=0.62,
    )
    wax.set_yticks(order)
    wax.set_yticklabels(data["donor_names"], fontsize=10.5, color=INK)
    wax.set_xlim(0, 0.40)
    wax.set_xticks([0, 0.2, 0.4])
    wax.set_xticklabels(["0%", "20%", "40%"], fontsize=10, color="#5A6268")
    wax.tick_params(axis="both", length=0)
    wax.grid(axis="x", color=GRID, linewidth=0.9)
    wax.set_axisbelow(True)
    wax.set_title(
        "Donor weights",
        fontsize=14,
        fontweight="bold",
        color=INK,
        pad=12,
        alpha=donors_in,
    )
    for spine in wax.spines.values():
        spine.set_visible(False)

    if frame < 13:
        title = "One treated country, many possible controls"
    elif frame < 29:
        title = "SCM chooses donor weights using pre-1990 outcomes"
    elif frame < 44:
        title = "Weighted donors reproduce West Germany before 1990"
    elif frame < 54:
        title = "German reunification occurs in 1990"
    elif frame < 71:
        title = "Synthetic West Germany provides the counterfactual"
    else:
        title = "The post-1990 gap is the estimated treatment effect"

    fig.suptitle(
        title,
        x=0.5,
        y=0.965,
        fontsize=20.5,
        fontweight="bold",
        color=INK,
    )

    if effect_in > 0:
        fig.text(
            0.5,
            0.045,
            r"Estimated effect$_t$ = West Germany$_t$ - Synthetic West Germany$_t$",
            ha="center",
            va="center",
            fontsize=13.0,
            fontweight="bold",
            color=EFFECT,
            alpha=effect_in,
        )
    else:
        fig.text(
            0.5,
            0.045,
            "Stylized illustration based on the German reunification application",
            ha="center",
            va="center",
            fontsize=10.8,
            color="#687078",
            alpha=donors_in,
        )

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

    palette_source = frames[-1].convert(
        "P", palette=Image.Palette.ADAPTIVE, colors=128
    )
    palette = palette_source.getpalette()
    gif_frames = []
    for frame in frames:
        paletted = frame.quantize(
            palette=palette_source,
            dither=Image.Dither.FLOYDSTEINBERG,
        )
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
