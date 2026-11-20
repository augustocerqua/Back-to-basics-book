"""Create a 27-second binary-instrument IV animation with illustrative data."""

from io import BytesIO
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, FancyBboxPatch
import numpy as np
from PIL import Image


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "images" / "iv-draft-lottery-animation.gif"
FRAME_COUNT = 60
FRAME_DURATION_MS = 450
WIDTH_PX = 1920
HEIGHT_PX = 1080
DPI = 192
plt.rcParams["text.parse_math"] = False

GROUP_SIZE = 24
ALWAYS_TAKERS = 3
COMPLIERS = 9
SERVICE_RATE_0 = ALWAYS_TAKERS / GROUP_SIZE
SERVICE_RATE_1 = (ALWAYS_TAKERS + COMPLIERS) / GROUP_SIZE
FIRST_STAGE = SERVICE_RATE_1 - SERVICE_RATE_0
LATE = -5000
EARNINGS_0 = 44000
REDUCED_FORM = LATE * FIRST_STAGE
EARNINGS_1 = EARNINGS_0 + REDUCED_FORM
assert FIRST_STAGE == 0.375
assert REDUCED_FORM == -1875
assert EARNINGS_1 == 42125
assert REDUCED_FORM / FIRST_STAGE == LATE

BACKGROUND = "#FBFAF7"
INK = "#20252B"
MUTED = "#9DA5AC"
GRID = "#DDE1E4"
NO_SERVICE = "#4C8BCB"
SERVICE = "#E05A3F"
COMPLIER = "#25834D"
GOLD = "#D9A420"
PANEL = "#FFFFFF"


def dollars(value):
    return ("-$" if value < 0 else "$") + f"{abs(value):,.0f}"


def smoothstep(value):
    value = np.clip(value, 0.0, 1.0)
    return value * value * (3.0 - 2.0 * value)


def progress(frame, start, end):
    return smoothstep((frame - start) / max(end - start, 1))


def draw_card(ax, xy, width, height, edgecolor, alpha=1.0, linewidth=1.8):
    patch = FancyBboxPatch(
        xy,
        width,
        height,
        boxstyle="round,pad=0.012,rounding_size=0.018",
        facecolor=PANEL,
        edgecolor=edgecolor,
        linewidth=linewidth,
        alpha=alpha,
        transform=ax.transAxes,
        zorder=1,
    )
    ax.add_patch(patch)
    return patch


def group_positions(x0):
    positions = []
    for row in range(4):
        for col in range(6):
            positions.append((x0 + 0.038 + col * 0.049, 0.665 - row * 0.072))
    return positions


def draw_person_dot(ax, position, facecolor, alpha, outline=None, outline_alpha=0):
    x, y = position
    if outline is not None and outline_alpha > 0:
        ax.add_patch(
            Circle(
                (x, y),
                0.0185,
                transform=ax.transAxes,
                facecolor="none",
                edgecolor=outline,
                linewidth=2.6,
                alpha=outline_alpha,
                zorder=4,
            )
        )
    ax.add_patch(
        Circle(
            (x, y),
            0.0128,
            transform=ax.transAxes,
            facecolor=facecolor,
            edgecolor="white",
            linewidth=0.7,
            alpha=alpha,
            zorder=5,
        )
    )


def render_frame(frame):
    groups_in = progress(frame, 0, 8)
    service_in = progress(frame, 9, 19)
    earnings_in = progress(frame, 21, 31)
    ratio_in = progress(frame, 33, 43)
    compliers_in = progress(frame, 45, 53)

    fig, ax = plt.subplots(
        figsize=(WIDTH_PX / DPI, HEIGHT_PX / DPI),
        dpi=DPI,
        facecolor=BACKGROUND,
    )
    ax.set_facecolor(BACKGROUND)
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis("off")

    left_x = 0.075
    right_x = 0.575
    card_y = 0.315
    card_w = 0.35
    card_h = 0.48

    draw_card(ax, (left_x, card_y), card_w, card_h, NO_SERVICE, groups_in)
    draw_card(ax, (right_x, card_y), card_w, card_h, SERVICE, groups_in)

    ax.text(
        left_x + card_w / 2,
        0.755,
        "Not draft eligible  (Z = 0)",
        transform=ax.transAxes,
        ha="center",
        va="center",
        fontsize=14,
        fontweight="bold",
        color=NO_SERVICE,
        alpha=groups_in,
        zorder=6,
    )
    ax.text(
        right_x + card_w / 2,
        0.755,
        "Draft eligible  (Z = 1)",
        transform=ax.transAxes,
        ha="center",
        va="center",
        fontsize=14,
        fontweight="bold",
        color=SERVICE,
        alpha=groups_in,
        zorder=6,
    )

    left_positions = group_positions(left_x + 0.008)
    right_positions = group_positions(right_x + 0.008)

    # Both eligibility groups contain the same principal-strata composition:
    # 3 always-takers, 9 compliers, and 12 never-takers. Eligibility changes
    # service only for the 9 compliers.
    always = set(range(ALWAYS_TAKERS))
    complier = set(range(ALWAYS_TAKERS, ALWAYS_TAKERS + COMPLIERS))

    for index, position in enumerate(left_positions):
        served = index in always
        if service_in < 0.05:
            color = MUTED
        else:
            color = SERVICE if served else NO_SERVICE
        alpha = 0.92 * groups_in
        if compliers_in > 0 and index not in complier:
            alpha *= 1 - 0.60 * compliers_in
        draw_person_dot(
            ax,
            position,
            color,
            alpha,
            outline=COMPLIER if index in complier else None,
            outline_alpha=compliers_in if index in complier else 0,
        )

    for index, position in enumerate(right_positions):
        served = index in always or index in complier
        if service_in < 0.05:
            color = MUTED
        else:
            color = SERVICE if served else NO_SERVICE
        alpha = 0.92 * groups_in
        if compliers_in > 0 and index not in complier:
            alpha *= 1 - 0.60 * compliers_in
        draw_person_dot(
            ax,
            position,
            color,
            alpha,
            outline=COMPLIER if index in complier else None,
            outline_alpha=compliers_in if index in complier else 0,
        )

    if service_in > 0:
        if compliers_in < 0.05:
            ax.add_patch(
                Circle(
                    (0.385, 0.825),
                    0.008,
                    transform=ax.transAxes,
                    facecolor=SERVICE,
                    edgecolor="white",
                    linewidth=0.5,
                    alpha=service_in,
                    zorder=8,
                )
            )
            ax.text(
                0.400,
                0.825,
                "Served",
                transform=ax.transAxes,
                ha="left",
                va="center",
                fontsize=11.5,
                color=INK,
                alpha=service_in,
            )
            ax.add_patch(
                Circle(
                    (0.525, 0.825),
                    0.008,
                    transform=ax.transAxes,
                    facecolor=NO_SERVICE,
                    edgecolor="white",
                    linewidth=0.5,
                    alpha=service_in,
                    zorder=8,
                )
            )
            ax.text(
                0.540,
                0.825,
                "Did not serve",
                transform=ax.transAxes,
                ha="left",
                va="center",
                fontsize=11.5,
                color=INK,
                alpha=service_in,
            )
        ax.text(
            left_x + card_w / 2,
            0.385,
            f"Military service: {SERVICE_RATE_0:.1%}",
            transform=ax.transAxes,
            ha="center",
            fontsize=13.2,
            fontweight="bold",
            color=INK,
            alpha=service_in,
        )
        ax.text(
            right_x + card_w / 2,
            0.385,
            f"Military service: {SERVICE_RATE_1:.1%}",
            transform=ax.transAxes,
            ha="center",
            fontsize=13.2,
            fontweight="bold",
            color=INK,
            alpha=service_in,
        )
        ax.annotate(
            "",
            xy=(0.566, 0.535),
            xytext=(0.434, 0.535),
            xycoords=ax.transAxes,
            textcoords=ax.transAxes,
            arrowprops=dict(
                arrowstyle="->",
                color=GOLD,
                lw=2.3,
                alpha=service_in,
            ),
            zorder=7,
        )
        ax.text(
            0.5,
            0.575,
            f"First stage\n+{100 * FIRST_STAGE:.1f} pp",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=11.5,
            linespacing=1.05,
            fontweight="bold",
            color=GOLD,
            alpha=service_in,
            bbox=dict(
                boxstyle="round,pad=0.28",
                facecolor=BACKGROUND,
                edgecolor="none",
                alpha=0.94,
            ),
            zorder=8,
        )

    if earnings_in > 0:
        ax.text(
            left_x + card_w / 2,
            0.335,
            f"Average earnings: {dollars(EARNINGS_0)}",
            transform=ax.transAxes,
            ha="center",
            fontsize=12.8,
            color=INK,
            alpha=earnings_in,
        )
        ax.text(
            right_x + card_w / 2,
            0.335,
            f"Average earnings: {dollars(EARNINGS_1)}",
            transform=ax.transAxes,
            ha="center",
            fontsize=12.8,
            color=INK,
            alpha=earnings_in,
        )
        ax.text(
            0.5,
            0.270,
            f"Reduced form: {dollars(REDUCED_FORM)}",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=14,
            fontweight="bold",
            color=SERVICE,
            alpha=earnings_in,
        )

    if ratio_in > 0:
        draw_card(
            ax,
            (0.245, 0.085),
            0.51,
            0.105,
            COMPLIER,
            ratio_in,
            linewidth=2.0,
        )
        ax.text(
            0.5,
            0.145,
            f"Wald ratio:  {dollars(REDUCED_FORM)} / {FIRST_STAGE:.3f} = {dollars(LATE)}",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=15.0,
            fontweight="bold",
            color=COMPLIER,
            alpha=ratio_in,
            zorder=8,
        )
        ax.text(
            0.5,
            0.102,
            "Local average treatment effect",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=11.5,
            color=INK,
            alpha=ratio_in,
            zorder=8,
        )

    if compliers_in > 0:
        ax.text(
            0.5,
            0.825,
            "Compliers: military service changes because of eligibility",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=12.8,
            fontweight="bold",
            color=COMPLIER,
            alpha=compliers_in,
        )

    if frame < 9:
        title = "The draft lottery creates as-if random eligibility"
    elif frame < 21:
        title = "Eligibility changes the probability of military service"
    elif frame < 33:
        title = "Eligibility also changes average earnings"
    elif frame < 45:
        title = "Scale the earnings change by the service change"
    else:
        title = "IV identifies the average treatment effect for compliers"

    fig.suptitle(
        title,
        x=0.5,
        y=0.965,
        fontsize=20.5,
        fontweight="bold",
        color=INK,
    )
    fig.text(
        0.5, 0.895,
        "Stylized example: numbers are illustrative, not Angrist's estimates.",
        ha="center", va="center", fontsize=11.2, color="#5B636A",
    )

    if frame >= 45:
        ax.text(
            0.5,
            0.051,
            "Green rings illustrate latent complier types; individual types are not observed.",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=10.5,
            color="#5B636A",
            alpha=compliers_in,
        )
        ax.text(
            0.5,
            0.018,
            "Requires relevance, as-if random assignment, exclusion, and monotonicity",
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=10.8,
            color="#5B636A",
            alpha=compliers_in,
        )

    fig.subplots_adjust(left=0.015, right=0.985, bottom=0.02, top=0.91)
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
    frames = [render_frame(frame) for frame in range(FRAME_COUNT)]

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
    qa = ROOT / "private" / "iv-draft-animation-qa"
    qa.mkdir(parents=True, exist_ok=True)
    for index in (8, 20, 32, 44, 59):
        frames[index].save(qa / f"after-frame-{index:02d}.png", dpi=(300, 300))
    with Image.open(OUTPUT) as check:
        durations = []
        for index in range(check.n_frames):
            check.seek(index)
            check.load()
            durations.append(check.info["duration"])
        assert sum(durations) == FRAME_COUNT * FRAME_DURATION_MS
        assert check.size == (WIDTH_PX, HEIGHT_PX)
        print(f"Saved {OUTPUT}")
        print(f"Verified {check.n_frames} GIF frames; duration: {sum(durations) / 1000:.1f}s")


if __name__ == "__main__":
    main()
