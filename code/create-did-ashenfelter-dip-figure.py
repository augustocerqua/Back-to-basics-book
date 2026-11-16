"""Illustrate how a temporary pre-training dip biases a two-period DiD."""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D


OUTPUT = Path(__file__).resolve().parents[1] / "images" / "did-ashenfelter-dip.png"
BG, INK, MUTED = "#FCFBF8", "#202A31", "#475861"
GRID, AXIS = "#E2E3DE", "#A8B0B1"
TREATED, CONTROL, RECOVERY, DID = "#AA3D2D", "#15637A", "#326D48", "#656E74"
PRE_TREATED = (24, 24, 18)
CONTROL_EARNINGS = (20, 20, 20, 20)
OBSERVED_POST, UNTREATED_POST = 24, 24
DID_PREDICTION = PRE_TREATED[-1] + CONTROL_EARNINGS[-1] - CONTROL_EARNINGS[-2]


def money(thousands):
    return f"\u20ac{thousands * 1000:,}"


def main():
    plt.rcParams.update({
        "font.family": ["Segoe UI", "DejaVu Sans"],
        "font.size": 12,
        "text.color": INK,
        "axes.labelcolor": MUTED,
        "axes.facecolor": BG,
    })
    fig = plt.figure(figsize=(11, 5.65), facecolor=BG)
    fig.text(0.05, 0.952, "Ashenfelter's dip", fontsize=26, weight="bold", va="top")
    fig.text(0.051, 0.855, "How a natural rebound can look like a training effect",
             fontsize=14, color=MUTED, va="center")

    ax = fig.add_axes([0.108, 0.218, 0.537, 0.54])
    ax.set_xlim(-0.2, 3.52)
    ax.set_ylim(15.7, 26.7)
    ax.set_xticks(range(4), [r"$t-3$", r"$t-2$", r"$t-1$", r"$t$"])
    ax.set_yticks(range(16, 27, 2))
    ax.set_ylabel("Mean earnings (\u20ac thousands)", labelpad=8)
    ax.tick_params(axis="both", length=0, pad=7, colors=MUTED)
    ax.set_axisbelow(True)
    ax.grid(axis="y", color=GRID, linewidth=0.9)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(AXIS)

    ax.axvline(2.5, color=AXIS, linewidth=1.2)
    label_background = {"facecolor": BG, "edgecolor": "none", "pad": 2}
    ax.text(2.5, 26.16, "Training starts", ha="center", va="center",
            fontsize=11, color=MUTED, bbox=label_background)

    control, = ax.plot(range(4), CONTROL_EARNINGS, color=CONTROL, linewidth=2.1,
                       marker="s", markersize=5, label="Non-participants (observed)")
    treated, = ax.plot(range(3), PRE_TREATED, color=TREATED, linewidth=2.7,
                       marker="o", markersize=6.5, label="Participants (observed)", zorder=4)
    recovery, = ax.plot([2, 3], [PRE_TREATED[-1], UNTREATED_POST], color=RECOVERY,
                        linestyle=(0, (1.3, 1.9)), linewidth=2.7,
                        label="Participants without training: true path")
    implied, = ax.plot([2, 3], [PRE_TREATED[-1], DID_PREDICTION], color=DID,
                       linestyle=(0, (5, 2.5)), linewidth=2.1,
                       label="Participants without training: DiD prediction")
    ax.plot(3, DID_PREDICTION, marker="o", markersize=8, markerfacecolor=BG,
            markeredgecolor=DID, markeredgewidth=1.6, zorder=4)
    ax.plot(3, UNTREATED_POST, marker="o", markersize=11, markerfacecolor=BG,
            markeredgecolor=RECOVERY, markeredgewidth=1.5, zorder=4)
    ax.plot(3, OBSERVED_POST, marker="o", markersize=6.5, color=TREATED, zorder=5)
    ax.annotate("Temporary dip", (2, PRE_TREATED[-1]), xytext=(0, -10),
                textcoords="offset points", ha="center", va="top",
                color=TREATED, fontsize=11, weight="semibold")

    # The bracket compares the observed outcome with the counterfactual used by DiD.
    x = 3.18
    ax.plot([x, x], [DID_PREDICTION, OBSERVED_POST], color=DID, linewidth=1.4)
    ax.hlines([DID_PREDICTION, OBSERVED_POST], x - 0.05, x + 0.05, color=DID, linewidth=1.4)
    ax.text(x + 0.08, (OBSERVED_POST + DID_PREDICTION) / 2,
            f"+\u20ac{OBSERVED_POST - DID_PREDICTION}k",
            va="center", color=TREATED, fontsize=12, weight="bold")

    fig.add_artist(Line2D([0.674, 0.674], [0.22, 0.755], transform=fig.transFigure,
                         color=GRID, linewidth=1))
    fig.text(0.702, 0.753, r"DiD compares $t-1$ with $t$", fontsize=15, weight="bold", va="top")
    fig.text(0.702, 0.673, "Participants gain \u20ac6,000.\nNon-participants' earnings\nstay flat.",
             fontsize=13, linespacing=1.25, va="top")
    fig.text(0.702, 0.493, "Naive DiD estimate", fontsize=12, color=MUTED, va="top")
    fig.text(0.702, 0.448, "+" + money(OBSERVED_POST - DID_PREDICTION),
             fontsize=22, color=TREATED, weight="bold", va="top")
    fig.text(0.702, 0.354, "True effect", fontsize=12, color=MUTED, va="top")
    fig.text(0.702, 0.309, money(OBSERVED_POST - UNTREATED_POST),
             fontsize=22, color=RECOVERY, weight="bold", va="top")
    fig.text(0.782, 0.290, "The same rebound would\noccur without training.",
             fontsize=11, color=MUTED, va="top")

    fig.legend(handles=[treated, control, recovery, implied], loc="lower left",
               bbox_to_anchor=(0.103, 0.018), ncols=2, frameon=False, fontsize=11.5,
               columnspacing=2.3, handlelength=2.8)
    fig.savefig(OUTPUT, dpi=330, facecolor=BG)
    plt.close(fig)


if __name__ == "__main__":
    main()
