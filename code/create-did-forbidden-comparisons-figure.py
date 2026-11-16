"""Show how a tutoring program can also change outcomes in the control schools."""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle


OUTPUT = Path(__file__).resolve().parents[1] / "images" / "did-forbidden-comparisons.png"
BG, INK, MUTED = "#FCFBF8", "#202A31", "#475861"
RULE, BASE, TUTORING = "#DADFD9", "#DCE7E9", "#15637A"
GOOD, WARNING = "#326D48", "#AA3D2D"
GAIN_WITHOUT_TUTORING = 1
FIRST_YEAR_EFFECT, SECOND_YEAR_EFFECT = 3, 5
POINT_WIDTH = 0.063


def block(fig, left, bottom, width, height, color):
    fig.add_artist(Rectangle((left, bottom), width, height, transform=fig.transFigure,
                             facecolor=color, edgecolor="none"))


def draw_row(fig, left, label_y, schools, status, before, after):
    extra_gain = after - before
    gain = GAIN_WITHOUT_TUTORING + extra_gain
    fig.text(left, label_y, schools, fontsize=14.5, weight="bold", va="center")
    fig.text(left + 0.153, label_y, status, fontsize=12, color=MUTED, va="center")

    bar_y, height = label_y - 0.117, 0.067
    x = left
    for points, color, text_color in (
        (GAIN_WITHOUT_TUTORING, BASE, INK),
        (extra_gain, TUTORING, "white"),
    ):
        if not points:
            continue
        width = points * POINT_WIDTH
        block(fig, x, bar_y, width, height, color)
        fig.text(x + width / 2, bar_y + height / 2, f"+{points}", fontsize=15,
                 weight="bold", color=text_color, ha="center", va="center")
        x += width
    fig.text(x + 0.014, bar_y + height / 2, f"+{gain} total", fontsize=14, va="center")
    return gain


def draw_panel(fig, left, title, rows, accent):
    fig.text(left, 0.683, title, fontsize=18, weight="bold", va="center")
    target, control = [draw_row(fig, left, y, *row) for y, row in zip((0.592, 0.402), rows)]
    effect = target - control
    result = f"{effect:+d}" if effect else "0"
    unit = "point" if abs(effect) == 1 else "points"
    block(fig, left, 0.237, 0.397, 0.002, RULE)
    fig.text(left, 0.185, f"DiD: {target} \u2212 {control} = {result} {unit}",
             fontsize=18, weight="bold", color=accent, va="center")


def main():
    plt.rcParams.update({
        "font.family": ["Segoe UI", "DejaVu Sans"],
        "text.color": INK,
    })
    fig = plt.figure(figsize=(11, 5.2), facecolor=BG)
    fig.text(0.05, 0.945, "What if the control schools also benefit?", fontsize=25,
             weight="bold", va="top")
    fig.text(0.05, 0.834, "Year-to-year gains in average test scores (points)",
             fontsize=14, color=MUTED, va="center")
    for left, color, label in (
        (0.05, BASE, "Gain without tutoring"),
        (0.325, TUTORING, "Extra gain from tutoring"),
    ):
        block(fig, left, 0.747, 0.026, 0.034, color)
        fig.text(left + 0.035, 0.764, label, fontsize=12, color=MUTED, va="center")

    block(fig, 0.496, 0.146, 0.001, 0.553, RULE)
    draw_panel(fig, 0.05, "A   2023 to 2024", (
        ("Early schools", "Start tutoring", 0, FIRST_YEAR_EFFECT),
        ("Late schools", "No tutoring yet (control)", 0, 0),
    ), GOOD)
    draw_panel(fig, 0.55, "B   2024 to 2025", (
        ("Late schools", "Start tutoring", 0, FIRST_YEAR_EFFECT),
        ("Early schools", "Tutoring continues (control)", FIRST_YEAR_EFFECT, SECOND_YEAR_EFFECT),
    ), WARNING)

    fig.text(0.05, 0.075, f"True effect in the first year of tutoring: +{FIRST_YEAR_EFFECT} points",
             fontsize=14, weight="semibold", va="center")
    fig.savefig(OUTPUT, dpi=330, facecolor=BG)
    plt.close(fig)


if __name__ == "__main__":
    main()
