"""Create a slow Italian teaching animation about tree ensembles.

The numerical predictions are illustrative, not fitted estimates. The boosting
example uses squared-error gradient boosting with a learning rate of 0.5.
Sources:
https://scikit-learn.org/stable/modules/ensemble.html
https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html
https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingRegressor.html
"""

from functools import lru_cache
from math import atan2, cos, sin
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "images"
QA = ROOT / "private" / "ensemble-animation-it-qa"
FONTS = Path("C:/Windows/Fonts")
W, H, SCALE = 1600, 960, 1.25
BG, INK, MUTED = "#F8FAFC", "#17313F", "#506674"
RULE, FAINT, WHITE = "#DCE4E9", "#E9EFF3", "#FFFFFF"
BAG, RF, BOOST = "#137D92", "#37734B", "#AF473D"
EURO = "\u20ac"
ARROW = "\u2192"
FEATURES = ("Superficie", "Stanze", "Zona", "Et\u00e0")
SAMPLES = ("A  C  C  D  E  F", "B  B  C  D  F  F", "A  A  B  D  E  E")
SCENES = (
    ("problem", 0, 7000),
    ("bag", 0, 8000), ("bag", 1, 7000), ("bag", 2, 8000),
    ("features", 0, 9000), ("features", 1, 9000), ("forest", 0, 7000),
    ("boost", 0, 7000), ("boost", 1, 9000), ("boost", 2, 9000), ("boost", 3, 9000),
    ("summary", 0, 13000),
)
assert sum(scene[2] for scene in SCENES) == 102_000


@lru_cache(maxsize=None)
def font(size, bold=False):
    return ImageFont.truetype(str(FONTS / ("segoeuib.ttf" if bold else "segoeui.ttf")),
                              round(size * SCALE))


class Canvas:
    def __init__(self):
        self.im = Image.new("RGB", (round(W * SCALE), round(H * SCALE)), BG)
        self.d = ImageDraw.Draw(self.im)

    def xy(self, p):
        return tuple(round(v * SCALE) for v in p)

    def text(self, p, label, size=26, color=INK, bold=False, anchor="lt", max_width=None):
        if max_width:
            assert self.d.textlength(label, font=font(size, bold)) <= max_width * SCALE, label
        self.d.text(self.xy(p), label, font=font(size, bold), fill=color, anchor=anchor)

    def lines(self, p, labels, size=26, color=INK, bold=False, leading=40, max_width=None):
        for n, line in enumerate(labels.split("\n")):
            self.text((p[0], p[1] + n * leading), line, size, color, bold, max_width=max_width)

    def line(self, points, color=RULE, width=2):
        self.d.line([self.xy(p) for p in points], fill=color, width=round(width * SCALE))

    def rect(self, bounds, fill, outline=None, width=1):
        self.d.rectangle(self.xy(bounds), fill=fill, outline=outline, width=round(width * SCALE))

    def dot(self, p, r=10, fill=WHITE, outline=RULE, width=2):
        self.d.ellipse(self.xy((p[0] - r, p[1] - r, p[0] + r, p[1] + r)),
                       fill=fill, outline=outline, width=round(width * SCALE))

    def arrow(self, a, b, color=RULE, width=3):
        self.line([a, b], color, width)
        angle = atan2(b[1] - a[1], b[0] - a[0])
        tips = [(b[0] - 13 * cos(angle + d), b[1] - 13 * sin(angle + d)) for d in (-0.5, 0.5)]
        self.d.polygon([self.xy(b)] + [self.xy(t) for t in tips], fill=color)

    def tree(self, x, y, color, small=False):
        spread, depth, radius = (48, 33, 7) if small else (92, 49, 10)
        for parent, children in (
            ((x, y), [(x - spread, y + depth), (x + spread, y + depth)]),
            ((x - spread, y + depth), [(x - 1.5 * spread, y + 2 * depth),
                                      (x - 0.5 * spread, y + 2 * depth)]),
            ((x + spread, y + depth), [(x + 0.5 * spread, y + 2 * depth),
                                      (x + 1.5 * spread, y + 2 * depth)]),
        ):
            for child in children:
                self.line([parent, child], color, 3)
                self.dot(child, radius, WHITE, color)
            self.dot(parent, radius, WHITE, color)


def header(c, section, progress):
    c.text((56, 34), "Bagging, random forest e boosting", 40, bold=True)
    labels = ("Il problema", "Bagging", "Random forest", "Boosting", "Il confronto")
    for n, label in enumerate(labels):
        x = 57 + n * 308
        color = (INK, BAG, RF, BOOST, INK)[n] if n == section else MUTED
        c.line([(x, 116), (x + 281, 116)], color if n == section else RULE, 4)
        c.text((x, 134), f"{n + 1:02d}  {label}", 23, color, n == section)
    c.line([(56, 939), (1544, 939)], RULE, 3)
    if progress:
        c.line([(56, 939), (56 + progress * 1488, 939)], MUTED, 3)


def footer(c, main, secondary, color=INK):
    c.line([(56, 824), (1544, 824)], RULE, 2)
    c.text((58, 847), main, 28, color, True, max_width=1480)
    c.text((58, 895), secondary, 23, MUTED, max_width=1480)


def house(c, x, y, color=BAG):
    c.line([(x - 45, y + 10), (x, y - 29), (x + 45, y + 10)], color, 4)
    c.rect((x - 34, y + 8, x + 34, y + 66), WHITE, color, 3)
    c.rect((x - 7, y + 31, x + 10, y + 66), FAINT, color, 2)
    c.rect((x - 24, y + 24, x - 13, y + 38), FAINT, color, 2)


def original_rows(c):
    c.text((800, 338), "Dati originali: 6 case", 25, bold=True, anchor="mt")
    for j, letter in enumerate("ABCDEF"):
        x = 610 + j * 76
        c.dot((x, 404), 25, WHITE, BAG)
        c.text((x, 404), letter, 23, BAG, True, "mm")


def problem(c):
    c.text((57, 213), "Prevedere il prezzo di una casa", 36, bold=True)
    c.text((58, 274), "Tutti e tre combinano alberi decisionali, ma li costruiscono in modo diverso.", 26,
           MUTED, max_width=1480)
    c.text((92, 365), "Case gi\u00e0 vendute: dati di training", 28, bold=True)
    columns = [(92, "Casa"), (240, "Superficie"), (460, "Stanze"), (625, "Zona"), (900, "Et\u00e0"),
               (1090, f"Prezzo (mila {EURO})")]
    for x, label in columns:
        c.text((x, 424), label, 25, MUTED, True)
    rows = (
        ("A", "50 m\u00b2", "2", "Periferia", "30", "120"),
        ("B", "65 m\u00b2", "3", "Periferia", "20", "140"),
        ("C", "75 m\u00b2", "3", "Periferia", "10", "160"),
        ("D", "90 m\u00b2", "4", "Centro", "20", "220"),
        ("E", "100 m\u00b2", "4", "Centro", "10", "260"),
        ("F", "120 m\u00b2", "5", "Centro", "5", "300"),
    )
    for i, row in enumerate(rows):
        y = 475 + 47 * i
        c.line([(85, y - 10), (1465, y - 10)])
        for (x, _), value in zip(columns, row):
            c.text((x, y), value, 25)
    footer(c, "Bagging: media. Random forest: pi\u00f9 diversit\u00e0. Boosting: correzioni successive.",
           "Esempio di regressione. Numeri e pochi alberi sono scelti solo per illustrare la logica.")


def bagging(c, phase, is_forest=False):
    color = RF if is_forest else BAG
    title = "Random forest: alberi diversi, poi la media" if is_forest else (
        "Bagging: creo campioni diversi", "Bagging: costruisco alberi in parallelo",
        "Bagging: faccio la media delle previsioni"
    )[phase]
    c.text((57, 213), title, 36, bold=True, color=color)
    subtitles = (
        "Bootstrap: estraggo 6 case con reinserimento. Una casa pu\u00f2 comparire pi\u00f9 volte.",
        "Ogni albero impara dal proprio campione: non deve aspettare gli altri.",
        "Per una nuova casa, combino le previsioni dei singoli alberi.",
    )
    c.text((58, 274), subtitles[2] if is_forest else subtitles[phase], 26, MUTED, max_width=1480)
    original_rows(c)
    for i, x in enumerate((300, 800, 1300)):
        c.arrow((650 + 150 * i, 438), (x, 488), color)
        c.text((x, 506), f"Campione {i + 1}:  {SAMPLES[i]}", 24, color, anchor="mt")
        if phase >= 1 or is_forest:
            c.tree(x, 580, color)
            c.text((x, 709), f"Albero {i + 1}", 24, color, True, "mt")
        if phase == 2 or is_forest:
            predictions = (230, 250, 270) if is_forest else (220, 250, 280)
            c.text((x, 752), f"{predictions[i]} mila {EURO}", 29, color, True, "mt")
    if is_forest:
        footer(c, f"Previsione = (230 + 250 + 270) / 3 = 250 mila {EURO}",
               "Bootstrap + variabili casuali a ogni nodo: alberi meno simili, poi una media.", color)
    elif phase == 0:
        footer(c, "Ogni campione ha 6 righe, ma alcune case si ripetono e altre non compaiono.",
               "Il ricampionamento crea versioni diverse dello stesso insieme di dati.", color)
    elif phase == 1:
        footer(c, "A ogni divisione, ciascun albero pu\u00f2 valutare tutte e 4 le variabili.",
               "La media aiuta a ridurre l'instabilit\u00e0 dei singoli alberi.", color)
    else:
        footer(c, f"Previsione = (220 + 250 + 280) / 3 = 250 mila {EURO}",
               "Si mediano previsioni complete del prezzo, non correzioni di altri alberi.", color)


def features(c, phase):
    c.text((57, 213), "Random forest: cambia anche le variabili candidate", 36, RF, True)
    c.text((58, 274), "Come nel bagging, ogni albero usa un campione bootstrap.", 26, MUTED)
    c.text((80, 358), "Zoom su un albero", 29, RF, True)
    c.text((80, 414), "A questo nodo: 2 variabili su 4", 26, bold=True)
    selected = (0, 1) if phase == 0 else (2, 3)
    for i, label in enumerate(FEATURES):
        x, y = 80 + 265 * (i % 2), 480 + 79 * (i // 2)
        c.rect((x, y, x + 238, y + 57), RF if i in selected else FAINT)
        c.text((x + 119, y + 28), label, 26, WHITE if i in selected else MUTED,
               i in selected, "mm")
    c.lines((80, 685), "Il sottoinsieme cambia\nquando passo a un altro nodo.", 25, MUTED, leading=37)
    root, left, right = (1110, 445), (910, 612), (1310, 612)
    for a, b in ((root, left), (root, right)):
        c.line([a, b], RF if phase else RULE, 4)
    for x in (910, 1310):
        for dx in (-70, 70):
            c.line([(x, 612), (x + dx, 727)], RF if phase else RULE, 3)
            c.dot((x + dx, 727), 10, WHITE, RF if phase else RULE)
    c.dot(root, 19, RF if phase == 0 else WHITE, RF, 3)
    c.text((1110, 384), "Superficie < 90 m\u00b2?", 26, RF, True, "mt")
    c.dot(left, 19, RF if phase else WHITE, RF if phase else RULE, 3)
    c.dot(right, 19, WHITE, RF if phase else RULE, 3)
    if phase:
        c.text((910, 551), "Et\u00e0 > 20 anni?", 24, RF, True, "mt")
        c.text((1310, 551), "Zona = centro?", 24, RF, True, "mt")
    else:
        c.text((1110, 780), "Scelgo la divisione migliore fra le candidate.", 24, MUTED, anchor="mt")
    footer(c, "L'estrazione delle variabili si ripete a OGNI divisione, non una volta per albero.",
           "Questa fonte aggiuntiva di casualit\u00e0 aiuta a rendere gli alberi meno correlati.", RF)


def boosting(c, phase):
    c.text((57, 213), "Boosting: aggiungo alberi che correggono il modello", 36, BOOST, True)
    c.text((58, 274), "Qui: gradient boosting con errore quadratico. Gli alberi si imparano in sequenza.",
           26, MUTED, max_width=1480)
    c.text((80, 352), f"Casa F nel training set: prezzo osservato = 300 mila {EURO}", 28, bold=True)
    c.text((80, 401), "Parto da 200, il prezzo medio delle 6 case. Uso un learning rate di 0,5.", 25, MUTED)

    for i, x in enumerate((260, 620, 980, 1340)):
        active = i <= phase
        color = BOOST if active else MUTED
        c.text((x, 479), "Partenza" if i == 0 else f"Passo {i}", 25, color, True, "mt")
        if i == 0:
            c.text((x, 582), "200", 48, BOOST, True, "mm")
            c.text((x, 672), "Previsione iniziale", 23, MUTED, anchor="mt")
        else:
            c.tree(x, 552, BOOST if active else RULE, small=True)
            correction = (100, 50, 25)[i - 1]
            c.text((x, 651), f"Correzione: +{correction}" if active else "In attesa", 24,
                   color, active, "mt")
            if active:
                increment = correction * 0.5
                c.text((x, 706), f"Aggiungo +{increment:g}".replace(".", ","), 26, BOOST, True, "mt")
        if i < 3:
            c.arrow((x + 100, 600), (x + 255, 600), BOOST if i < phase else RULE, 4)

    prediction = (200, 250, 275, 287.5)[phase]
    residual = 300 - prediction
    if phase == 0:
        main = "Errore iniziale: 300 - 200 = 100. Il prossimo albero impara a prevedere gli errori."
        secondary = "Con errore quadratico, gli errori residui sono prezzo osservato meno previsione corrente."
    else:
        increments = ("50", "50 + 25", "50 + 25 + 12,5")[phase - 1]
        amount = f"{prediction:g}".replace(".", ",")
        gap = f"{residual:g}".replace(".", ",")
        main = f"Previsione aggiornata: 200 + {increments} = {amount}. Errore residuo: {gap}."
        secondary = "Sommo piccole correzioni: il learning rate di 0,5 dimezza il contributo di ogni nuovo albero."
    footer(c, main, secondary, BOOST)


def summary(c):
    c.text((57, 213), "Tre modi diversi di far lavorare gli alberi", 36, bold=True)
    c.text((58, 274), "La differenza non \u00e8 solo quanti alberi uso, ma come imparano e si combinano.",
           26, MUTED, max_width=1480)
    details = (
        ("Bagging", BAG, "Campioni bootstrap", "Alberi indipendenti,\naddestrabili in parallelo.",
         "Media di previsioni\ncomplete."),
        ("Random forest", RF, "Bootstrap + variabili casuali",
         "Nuove candidate\na ogni divisione.", "Media di previsioni\ndi alberi meno simili."),
        ("Boosting", BOOST, "Correzioni in sequenza", "Ogni albero dipende\ndal modello precedente.",
         "Previsione iniziale\n+ correzioni pesate."),
    )
    for i, (title, color, key, learning, combination) in enumerate(details):
        x = 65 + i * 520
        if i:
            c.line([(x - 28, 358), (x - 28, 784)], RULE, 2)
        c.text((x, 364), title, 32, color, True)
        c.text((x, 433), key, 24, color, True, max_width=480)
        c.tree(x + 190, 513, color, small=True)
        c.lines((x, 622), learning, 26, leading=37, max_width=470)
        c.lines((x, 717), combination, 26, bold=True, leading=37, max_width=470)
    footer(c, "Non c'\u00e8 un vincitore universale: confronta i metodi su dati non usati nel training.",
           "Per il boosting abbiamo illustrato la variante gradient boosting per regressione.")


def render(name, phase, progress):
    c = Canvas()
    section = {"problem": 0, "bag": 1, "features": 2, "forest": 2, "boost": 3, "summary": 4}[name]
    header(c, section, progress)
    if name == "problem":
        problem(c)
    elif name == "bag":
        bagging(c, phase)
    elif name == "features":
        features(c, phase)
    elif name == "forest":
        bagging(c, 2, is_forest=True)
    elif name == "boost":
        boosting(c, phase)
    else:
        summary(c)
    return c.im


def main():
    OUT.mkdir(exist_ok=True)
    QA.mkdir(parents=True, exist_ok=True)
    frames, durations, elapsed = [], [], 0
    total = sum(ms for _, _, ms in SCENES)
    for i, (name, phase, ms) in enumerate(SCENES):
        # Static reading holds dominate; only the timeline moves during each hold.
        for part in range(4):
            duration = ms // 4
            im = render(name, phase, (elapsed + duration * (part + 1)) / total)
            frames.append(im)
            durations.append(duration)
        frames[-1].save(QA / f"{i:02d}-{name}-{phase}.png", dpi=(300, 300))
        elapsed += ms

    # Use one palette for every frame to avoid color flicker.
    strip = Image.new("RGB", (400, 240 * len(SCENES)), BG)
    for i in range(len(SCENES)):
        strip.paste(frames[i * 4 + 3].resize((400, 240)), (0, i * 240))
    palette = strip.quantize(colors=192)
    indexed = [frame.quantize(palette=palette, dither=Image.Dither.NONE) for frame in frames]
    gif = OUT / "bagging-random-forest-boosting-it.gif"
    indexed[0].save(gif, save_all=True, append_images=indexed[1:], duration=durations,
                    loop=0, optimize=True, disposal=1)
    frames[-1].save(OUT / "bagging-random-forest-boosting-it.png", dpi=(300, 300))
    with Image.open(gif) as check:
        timings = []
        for i in range(check.n_frames):
            check.seek(i)
            check.load()
            timings.append(check.info["duration"])
        assert check.size == (2000, 1200)
        assert abs(sum(timings) - total) <= 100
        print(f"GIF: {gif}\nDuration: {sum(timings) / 1000:.2f}s; frames: {check.n_frames}")
    print(f"Storyboard: {QA}")


if __name__ == "__main__":
    main()
