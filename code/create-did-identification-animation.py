"""Render the two-group, two-period DiD identification example."""

from pathlib import Path
from math import hypot

from PIL import Image, ImageDraw, ImageFont


OUTPUT = Path(__file__).resolve().parents[1] / "images"
FONTS = Path("C:/Windows/Fonts")
BG, INK, MUTED = "#FCFBF8", "#202A31", "#475861"
GRID, AXIS = "#E6E6E1", "#A8B0B1"
TREATED, CONTROL, EFFECT = "#AA3D2D", "#15637A", "#326D48"
X0, X1 = 157, 745
LOW, HIGH = 598, 215
C0, C1, T0, T1 = 5, 8, 10, 17
CHANGE = C1 - C0
CF = T0 + CHANGE
DID = T1 - CF


def y(value):
    return LOW - (value - 4) * (LOW - HIGH) / 14


class Canvas:
    def __init__(self, scale, size=(1440, 810)):
        self.s = scale
        self.image = Image.new("RGB", tuple(round(v * scale) for v in size), BG)
        self.draw = ImageDraw.Draw(self.image)

    def point(self, xy):
        return tuple(round(v * self.s) for v in xy)

    def line(self, points, color, width=2):
        self.draw.line([self.point(p) for p in points], fill=color, width=max(1, round(width * self.s)))

    def text(self, xy, label, size=24, color=INK, bold=False, anchor="lt"):
        font = ImageFont.truetype(str(FONTS / ("segoeuib.ttf" if bold else "segoeui.ttf")), round(size * self.s))
        self.draw.text(self.point(xy), label, fill=color, font=font, anchor=anchor)

    def dot(self, x, cy, color, hollow=False):
        self.draw.ellipse((*self.point((x - 8, cy - 8)), *self.point((x + 8, cy + 8))),
                          fill=BG if hollow else color, outline=color, width=round(3 * self.s))

    def dash(self, a, b, color, width=3, dash=12, gap=8):
        length = hypot(b[0] - a[0], b[1] - a[1])
        if length == 0:
            return
        for off in range(0, int(length) + 1, dash + gap):
            pts = [tuple(a[i] + (b[i] - a[i]) * min(d, length) / length for i in (0, 1))
                   for d in (off, off + dash)]
            self.line(pts, color, width)

    def bracket(self, x, bottom, top, color, label, align_left=False):
        yb, yt = y(bottom), y(top)
        self.line([(x, yt), (x, yb)], color, 3)
        for cy in (yt, yb):
            self.line([(x - 7, cy), (x + 7, cy)], color, 3)
        self.text((x - 14 if align_left else x + 14, (yb + yt) / 2), label, 21, color,
                  bold=True, anchor="rm" if align_left else "lm")


def draw_frame(stage, fraction=1.0, scale=1):
    c = Canvas(scale)
    c.text((54, 41), "How difference-in-differences identifies the ATT", 43, bold=True)
    c.text((56, 106), "Two groups, two periods", 25, MUTED)
    stages = ["1  Observe", "2  Measure change", "3  Build counterfactual", "4  Measure effect", "5  Compare calculations"]
    for i, title in enumerate(stages):
        x = 56 + 268 * i
        c.line([(x, 155), (x + 245, 155)], CONTROL if i == stage else GRID, 4)
        c.text((x, 169), title, 19, CONTROL if i == stage else MUTED, bold=i == stage)

    for v in range(4, 19, 2):
        c.line([(125, y(v)), (790, y(v))], GRID, 1)
        c.text((112, y(v)), str(v), 18, MUTED, anchor="rm")
    c.line([(125, HIGH), (125, LOW), (790, LOW)], AXIS, 2)
    c.text((140, 222), "Group mean", 17, MUTED)
    c.text((X0, 618), "Pre", 22, bold=True, anchor="mt")
    c.text((X1, 618), "Post", 22, bold=True, anchor="mt")
    event = (X0 + X1) / 2
    c.dash((event, HIGH + 32), (event, LOW), AXIS, 2, 6, 7)
    c.text((event, HIGH + 4), "Treatment begins", 17, MUTED, anchor="mt")
    c.line([(860, 212), (860, 688)], GRID, 2)

    for v0, v1, color in ((C0, C1, CONTROL), (T0, T1, TREATED)):
        c.line([(X0, y(v0)), (X1, y(v1))], color, 4)
        c.dot(X0, y(v0), color)
        c.dot(X1, y(v1), color)
        c.text((X0 + 16, y(v0) - 16), str(v0), 23, color, True, anchor="lb")
    c.text((X1 + 15, y(T1)), str(T1), 23, TREATED, True, anchor="lm")
    c.text((X1 + 15, y(C1) + 12), str(C1), 23, CONTROL, True, anchor="lm")

    if stage == 2 and fraction < 1:
        offset = (T0 - C0) * fraction
        c.dash((X0, y(C0 + offset)), (X1, y(C1 + offset)), CONTROL, 4)
    if stage >= 3 or (stage == 2 and fraction == 1):
        c.dash((X0, y(T0)), (X1, y(CF)), CONTROL, 4)
        c.dot(X0, y(T0), TREATED)
        c.dot(X1, y(CF), CONTROL, hollow=True)
        c.text((X1 + 15, y(CF) - 2), str(CF), 23, CONTROL, True, anchor="lm")
    if stage == 1:
        c.bracket(806, C0, C1, CONTROL, "+3", align_left=True)
    if stage >= 3:
        c.bracket(794, CF, T1, EFFECT, "+4")

    c.line([(125, 683), (155, 683)], TREATED, 4)
    c.text((167, 683), "Treated", 19, anchor="lm")
    c.line([(295, 683), (325, 683)], CONTROL, 4)
    c.text((337, 683), "Control", 19, anchor="lm")
    if stage >= 2:
        c.dash((460, 683), (492, 683), CONTROL, 3, 8, 5)
        c.text((505, 683), "Treated without treatment (inferred)", 19, anchor="lm")

    def heading(label):
        c.text((890, 232), label, 31, bold=True)

    def rows(first, second, detail):
        c.text((891, 299), first, 22, MUTED)
        c.text((891, 339), second, 33, bold=True)
        for i, row in enumerate(detail):
            c.text((891, 411 + 32 * i), row, 22, MUTED)

    if stage == 0:
        heading("Start with the observed means")
        rows("Treated group", "10 → 17", ["The groups start at different levels.", "A post-treatment comparison alone", "would include that initial difference."])
        c.text((891, 548), "Control group", 22, MUTED)
        c.text((891, 585), "5 → 8", 33, CONTROL, bold=True)
    elif stage == 1:
        heading("Measure the control change")
        rows("Change without treatment in the control group", "8 − 5 = +3", ["The control mean rose by 3 units.", "DiD uses this change to construct", "the treated group’s missing outcome."])
    elif stage == 2:
        heading("Apply parallel trends")
        rows("Treated pre-treatment mean + control change", "10 + 3 = 13", ["Without treatment, the treated group", "would have had the same average", "change as the control group.", "13 is inferred, not observed."])
    elif stage == 3:
        heading("Recover the treatment effect")
        rows("Observed treated post − counterfactual post", "17 − 13 = 4", ["Under parallel trends, this gap", "identifies the average treatment", "effect on the treated (ATT)."])
    else:
        heading("The DiD and the counterfactual")
        for cy, label, equation in [
            (296, "Changes over time within each group", "(17 − 10) − (8 − 5) = 4"),
            (416, "Change in the treated–control gap", "(17 − 8) − (10 − 5) = 4"),
            (536, "Observed treated post − counterfactual", "17 − (10 + 3) = 4"),
        ]:
            c.text((891, cy), label, 22, MUTED)
            c.text((891, cy + 41), equation, 29, EFFECT, bold=True)

    c.line([(56, 726), (1384, 726)], GRID, 2)
    c.text((56, 751), "Identification assumes parallel untreated trends, no anticipation, and an unaffected control group.", 20, MUTED)
    return c.image


def draw_print_figure():
    c = Canvas(2, (1440, 860))
    panels = [
        ("A", "Observe the two groups", "Treated: 10 → 17     Control: 5 → 8"),
        ("B", "Measure the control change", "Control change: 8 − 5 = +3"),
        ("C", "Apply parallel trends", "Treated without treatment: 10 + 3 = 13"),
        ("D", "Estimate the ATT", "DiD: 17 − 13 = (17 − 10) − (8 − 5) = 4"),
    ]
    c.text((40, 22), "From observed outcomes to the DiD estimate", 34, bold=True)
    c.line([(720, 86), (720, 775)], GRID)
    c.line([(40, 427), (1400, 427)], GRID)

    for i, (letter, title, equation) in enumerate(panels):
        left, top = 40 + (i % 2) * 705, 85 + (i // 2) * 354
        start, end = left + 128, left + 565
        base = top + 252

        def py(v):
            return base - (v - 4) * 13.3

        c.text((left, top), letter, 24, CONTROL, bold=True)
        c.text((left + 39, top - 1), title, 26, bold=True)
        c.text((left + 9, top + 50), "Group", 16, MUTED)
        c.text((left + 9, top + 70), "mean", 16, MUTED)
        for v in (4, 8, 12, 16):
            c.line([(left + 73, py(v)), (end + 64, py(v))], GRID, 1)
            c.text((left + 63, py(v)), str(v), 17, MUTED, anchor="rm")
        c.line([(left + 73, py(18)), (left + 73, base), (end + 64, base)], AXIS)
        c.dash(((start + end) / 2, py(18)), ((start + end) / 2, base), AXIS, 2, 5, 6)
        for pre, post, color in ((C0, C1, CONTROL), (T0, T1, TREATED)):
            c.line([(start, py(pre)), (end, py(post))], color, 4)
            c.dot(start, py(pre), color)
            c.dot(end, py(post), color)
            c.text((start + 13, py(pre) - 8), str(pre), 20, color, True, anchor="lb")
            c.text((end + 12, py(post)), str(post), 20, color, True, anchor="lm")
        if i >= 2:
            c.dash((start, py(T0)), (end, py(CF)), CONTROL, 4)
            c.dot(start, py(T0), TREATED)
            c.dot(end, py(CF), CONTROL, hollow=True)
            c.text((end + 12, py(CF)), str(CF), 20, CONTROL, True, anchor="lm")
        if i in (1, 3):
            bottom, upper, gain, color = (C0, C1, CHANGE, CONTROL) if i == 1 else (CF, T1, DID, EFFECT)
            bx = end + 45
            c.line([(bx, py(bottom)), (bx, py(upper))], color, 3)
            for v in (bottom, upper):
                c.line([(bx - 5, py(v)), (bx + 5, py(v))], color, 3)
            c.text((bx + 10, (py(bottom) + py(upper)) / 2), f"+{gain}", 20, color, True, anchor="lm")
        c.text((start, base + 10), "Pre", 19, MUTED, anchor="mt")
        c.text((end, base + 10), "Post", 19, MUTED, anchor="mt")
        c.text((left + 73, base + 49), equation, 22, EFFECT if i == 3 else INK, bold=True)

    for x, label, color, dashed in [
        (42, "Treated", TREATED, False),
        (216, "Control", CONTROL, False),
        (390, "Treated without treatment (counterfactual)", CONTROL, True),
    ]:
        if dashed:
            c.dash((x, 817), (x + 31, 817), color, 3, 7, 5)
        else:
            c.line([(x, 817), (x + 31, 817)], color, 4)
        c.text((x + 42, 817), label, 20, MUTED, anchor="lm")
    return c.image


def main():
    assert DID == (T1 - T0) - (C1 - C0) == (T1 - C1) - (T0 - C0)
    # Keep a shared palette so that only moving content changes between frames.
    final = draw_frame(4)
    palette = final.quantize(colors=192)
    sequence = [(0, 1, 2800), (1, 1, 2800)]
    sequence += [(2, i / 12, 90) for i in range(1, 12)]
    sequence += [(2, 1, 3500), (3, 1, 3300), (4, 1, 7900)]
    frames = [draw_frame(stage, t).quantize(palette=palette, dither=Image.Dither.NONE)
              for stage, t, _ in sequence]
    name = OUTPUT / "did-parallel-trends-identification"
    frames[0].save(name.with_suffix(".gif"), save_all=True, append_images=frames[1:],
                   duration=[ms for _, _, ms in sequence], loop=0, disposal=1, optimize=True)
    draw_print_figure().save(name.with_suffix(".png"), dpi=(300, 300))


if __name__ == "__main__":
    main()
