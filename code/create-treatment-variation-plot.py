"""Render a binary treatment-status matrix without interpolated cell colors."""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, to_rgb
from matplotlib.patches import Patch
import numpy as np


OUTPUT = Path(__file__).resolve().parents[1] / "images" / "treatment_variation_plot.png"
BG, INK, MUTED = "#FCFBF8", "#202A31", "#475861"
UNTREATED, TREATED = "#E5E9ED", "#2468B4"
PERIODS = np.arange(7)
FIRST_TREATED = (1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, None, None, None)
UNITS = [f"Unit {chr(65 + i)}" for i in range(len(FIRST_TREATED))]


def main():
    status = np.array([
        PERIODS >= start if start is not None else np.zeros(len(PERIODS), dtype=bool)
        for start in FIRST_TREATED
    ], dtype=np.uint8)
    assert set(np.unique(status)) == {0, 1}
    assert np.all(np.diff(status.astype(int), axis=1) >= 0)

    plt.rcParams.update({
        "font.family": ["Segoe UI", "DejaVu Sans"],
        "font.size": 13,
        "text.color": INK,
        "axes.labelcolor": MUTED,
    })
    fig = plt.figure(figsize=(10, 6.6), dpi=330, facecolor=BG)
    fig.text(0.06, 0.951, "Treatment variation plot", fontsize=26, weight="bold", va="top")
    fig.text(0.061, 0.862, "Once treated, units remain treated. Each cell is one unit in one period.",
             fontsize=13.5, color=MUTED, va="center")

    rows = len(UNITS)
    ax = fig.add_axes([0.15, 0.183, 0.67, 0.605])
    ax.pcolormesh(
        np.arange(len(PERIODS) + 1) - 0.5,
        np.arange(rows + 1) - 0.5,
        status,
        cmap=ListedColormap([UNTREATED, TREATED]),
        vmin=0, vmax=1,
        shading="flat",
        edgecolors=BG,
        linewidth=1.5,
        antialiased=False,
    )
    ax.set_xlim(-0.5, len(PERIODS) - 0.5)
    ax.set_ylim(rows - 0.5, -0.5)
    ax.set_xticks(PERIODS, [rf"$t_{{{t}}}$" for t in PERIODS])
    ax.set_yticks(range(rows), UNITS)
    ax.tick_params(axis="both", length=0, pad=9, labelsize=12, colors=INK)
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.set_xlabel("Time", fontsize=13, labelpad=8)

    # The final three rows are untreated throughout the observation window.
    x = len(PERIODS) - 0.36
    top, bottom = 12.6, rows - 0.6
    ax.plot([x, x + 0.08, x + 0.08, x], [top, top, bottom, bottom],
            color=MUTED, linewidth=1.1, clip_on=False)
    ax.text(x + 0.17, (top + bottom) / 2, "Never\ntreated", va="center",
            fontsize=12, color=MUTED, linespacing=1.2, clip_on=False)

    fig.legend(
        handles=[Patch(facecolor=UNTREATED, label="Untreated"),
                 Patch(facecolor=TREATED, label="Treated")],
        loc="center", bbox_to_anchor=(0.485, 0.062), ncols=2,
        frameon=False, fontsize=13, handlelength=1.6, handleheight=1.1, columnspacing=2.5,
    )

    # Check multiple interior locations, not just cell centers, for partial fills.
    fig.canvas.draw()
    pixels = np.asarray(fig.canvas.buffer_rgba())
    for row in range(rows):
        for column in PERIODS:
            expected = np.rint(
                np.array(to_rgb(TREATED if status[row, column] else UNTREATED)) * 255
            ).astype(int)
            for dx in (-0.35, 0, 0.35):
                for dy in (-0.25, 0.25):
                    px, py = ax.transData.transform((column + dx, row + dy))
                    actual = pixels[pixels.shape[0] - 1 - int(py), int(px), :3]
                    assert np.max(np.abs(actual.astype(int) - expected)) <= 1, (
                        f"Incorrect fill in row {row}, period {column}"
                    )
    fig.savefig(OUTPUT, dpi=330, facecolor=BG)
    plt.close(fig)
    print(f"Saved {OUTPUT}")
    print(f"Verified {status.size} binary cells, including their left and right interiors.")


if __name__ == "__main__":
    main()
