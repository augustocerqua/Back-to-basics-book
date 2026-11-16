"""Compare parallel wage changes in levels and logs using two stylized examples."""

from math import isclose, log
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D


OUTPUT = Path(__file__).resolve().parents[1] / "images" / "did-parallel-trends-levels-vs-logs.png"
BG, INK, MUTED = "#FCFBF8", "#202A31", "#475861"
GRID, AXIS = "#E2E3DE", "#A8B0B1"
TREATED, CONTROL, GREEN = "#AA3D2D", "#15637A", "#326D48"
GROUPS = (
    ("Treated group", TREATED, "-", "o"),
    ("Control group", CONTROL, (0, (5, 2.5)), "s"),
)


def panel(ax, wages, use_logs):
    values = [tuple(map(log, pair)) if use_logs else pair for pair in wages]
    changes = [end - start for start, end in values]
    parallel = isclose(*changes)
    ax.text(0, 1.06, "Natural logs" if use_logs else "Levels",
            transform=ax.transAxes, fontsize=16, weight="bold", color=INK)
    ax.text(1, 1.06, "Parallel" if parallel else "Not parallel",
            transform=ax.transAxes, fontsize=14, weight="bold", ha="right",
            color=GREEN if parallel else MUTED)

    ax.set_xlim(-0.21, 1.21)
    ax.set_ylim((2.12, 3.64) if use_logs else (7, 34))
    ax.set_yticks([2.5, 3.0, 3.5] if use_logs else [10, 20, 30])
    ax.set_xticks([0, 1], [r"$t-2$", r"$t-1$"])
    ax.set_ylabel("Log hourly wage" if use_logs else "Hourly wage (\u20ac)", labelpad=8)
    ax.tick_params(axis="both", length=0, pad=7, colors=MUTED)
    ax.set_axisbelow(True)
    ax.grid(axis="y", color=GRID, linewidth=0.9)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("bottom", "left"):
        ax.spines[side].set_color(AXIS)

    label_background = {"facecolor": BG, "edgecolor": "none", "pad": 1.5}
    for (_, color, style, marker), pair, y, change in zip(GROUPS, wages, values, changes):
        ax.plot([0, 1], y, color=color, linestyle=style, linewidth=2.6, marker=marker,
                markersize=6.5, markeredgecolor=BG, markeredgewidth=0.8)
        if use_logs:
            rate = (pair[1] / pair[0] - 1) * 100
            label = f"{change:+.3f}  ({rate:+.0f}%)"
        else:
            label = f"+\u20ac{change:g}"
            for x, value, offset, align in ((0, y[0], -8, "right"), (1, y[1], 8, "left")):
                ax.annotate(f"\u20ac{value:g}", (x, value), xytext=(offset, 0),
                            textcoords="offset points", ha=align, va="center",
                            color=color, fontsize=11.5, bbox=label_background)
        ax.annotate(label, (0.5, sum(y) / 2), xytext=(0, 9), textcoords="offset points",
                    ha="center", va="bottom", color=color, fontsize=13, weight="semibold",
                    bbox=label_background)


def main():
    plt.rcParams.update({
        "font.family": ["Segoe UI", "DejaVu Sans"],
        "font.size": 12,
        "text.color": INK,
        "axes.labelcolor": MUTED,
        "axes.facecolor": BG,
    })
    fig = plt.figure(figsize=(11, 6.55), facecolor=BG)
    fig.text(0.05, 0.952, "Parallel trends depend on the outcome scale",
             fontsize=25, weight="bold", va="top")
    fig.text(0.051, 0.875, "Same wages on two scales. Treatment begins at $t$.",
             fontsize=13, color=MUTED, va="center")
    fig.legend(
        handles=[Line2D([0], [0], color=c, linestyle=s, marker=m, linewidth=2.6,
                        label=name) for name, c, s, m in GROUPS],
        loc="center right", bbox_to_anchor=(0.954, 0.875), ncols=2,
        frameon=False, fontsize=12, columnspacing=1.3,
    )

    # Match the axes within each column so visual differences reflect the data.
    examples = (
        (0.828, 0.782, 0.482, "A", "Same increase in euros", ((20, 25), (10, 15))),
        (0.419, 0.373, 0.073, "B", "Same percentage increase", ((20, 30), (10, 15))),
    )
    for rule_y, heading_y, bottom, letter, title, wages in examples:
        fig.add_artist(Line2D([0.05, 0.95], [rule_y, rule_y],
                             transform=fig.transFigure, color=GRID, linewidth=1))
        fig.text(0.051, heading_y, letter, color=MUTED, fontsize=16, weight="bold", va="center")
        fig.text(0.082, heading_y, title, fontsize=17, weight="bold", va="center")
        for left, use_logs in ((0.11, False), (0.584, True)):
            panel(fig.add_axes([left, bottom, 0.366, 0.21]), wages, use_logs)

    fig.savefig(OUTPUT, dpi=330, facecolor=BG)
    plt.close(fig)


if __name__ == "__main__":
    main()
