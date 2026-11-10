"""Update the supplied teaching animation to Chapter 3 notation.

Run with the original GIF path as the first argument. The original unit cards,
moving dots, frame durations, and loop are retained; notation is redrawn at 2x.
"""
import io
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'private' / 'animation-dependencies'))
import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams['savefig.transparent'] = True
from matplotlib.mathtext import math_to_image
from matplotlib.font_manager import FontProperties
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[1]
SOURCE = Path(sys.argv[1])
DEST = ROOT / 'images'
SCALE = 2
BG = (248, 250, 252)
INK = (24, 48, 64)
MUTED = (80, 102, 120)
TEAL = (0, 126, 128)
FONT = Path('C:/Windows/Fonts')
MATH_CACHE = {}


def box(draw, bounds, color):
    draw.rectangle(tuple(round(v*SCALE) for v in bounds), fill=color)


def text(draw, xy, value, size=21, color=INK, bold=False):
    font = ImageFont.truetype(str(FONT / ('segoeuib.ttf' if bold else 'segoeui.ttf')), size*SCALE)
    draw.text((xy[0]*SCALE, xy[1]*SCALE), value, font=font, fill=color)


def math(image, xy, formula, size=27, color=INK):
    key = (formula, size, color)
    if key not in MATH_CACHE:
        stream = io.BytesIO()
        math_to_image('$'+formula+'$', stream,
                      prop=FontProperties(size=size, family='DejaVu Serif'),
                      dpi=72*SCALE, format='png', color=tuple(c/255 for c in color))
        stream.seek(0)
        MATH_CACHE[key] = Image.open(stream).convert('RGBA')
    layer = MATH_CACHE[key]
    image.paste(layer, (round(xy[0]*SCALE), round(xy[1]*SCALE)), layer)


def redraw(frame, stage):
    image = frame.resize((2880, 1620), Image.Resampling.LANCZOS)
    draw = ImageDraw.Draw(image)
    box(draw, (50, 209, 791, 244), BG)
    text(draw, (54, 211), 'Individual treatment effects', size=22, bold=True)
    math(image, (366, 218), r'\Delta_i', size=21)

    # Preserve each original badge's color, including dimmed non-selected units.
    for n in range(12):
        x = 54+(n % 4)*182
        y = 249+(n // 4)*146
        for bx, value in [(x+13, 'Dᵢ = '+str(TREATED[n])), (x+91, 'Xᵢ = '+str(CHARACTERISTIC[n]))]:
            fill = frame.getpixel((bx+5, y+96))
            box(draw, (bx, y+94, bx+62, y+117), fill)
            if stage in (0, 1, 4) or (stage == 2 and TREATED[n]) or (stage == 3 and CHARACTERISTIC[n]):
                foreground = (255,255,255) if bx==x+13 and TREATED[n] else ((119,82,170) if bx==x+91 and CHARACTERISTIC[n] else MUTED)
            else:
                foreground = (190, 201, 208)
            text(draw, (bx+8, y+94), value, size=15, color=foreground, bold=True)

    box(draw, (51, 695, 800, 735), BG)
    text(draw, (54, 701), 'Dᵢ = 1: treated     ·     Xᵢ = 1: has the characteristic', size=16, color=MUTED)
    box(draw, (52, 752, 1390, 800), BG)
    text(draw, (54, 763), 'Hypothetical effects, assumed known for illustration. Equal weights within each target group.', size=16, color=MUTED)

    if stage == 0:
        box(draw, (865, 237, 1400, 685), BG)
        text(draw, (876, 242), 'Start with individual effects', size=29, bold=True)
        math(image, (876, 309), r'\Delta_i=Y_i(1)-Y_i(0)', size=28)
        text(draw, (876, 378), '12 units. 12 individual treatment effects.', size=20)
        text(draw, (876, 439), 'Then choose whom to average over:', size=21)
        for y,label in [(491,'All units → ATE'),(539,'Treated units → ATT'),(587,'Units with Xᵢ = x → CATE(x)')]:
            text(draw, (876, y), label, size=22)
    elif stage in (1,2,3):
        # Retain the source's moving-dot plot and give it an explicit estimand.
        box(draw, (865, 237, 1400, 338), BG)
        titles = {1:'Average treatment effect',2:'Average effect on the treated',3:'Conditional average effect'}
        counts = {1:'Entire population · 12 units',2:'Dᵢ = 1 · 6 treated units',3:'Xᵢ = 1 · 4 units'}
        text(draw, (876, 244), titles[stage], size=27, bold=True)
        text(draw, (876, 299), counts[stage], size=21, color=MUTED)
        formulas = {1:r'ATE=E[\Delta_i]',2:r'ATT=E[\Delta_i\mid D_i=1]',3:r'CATE(x)=E[\Delta_i\mid X_i=x]'}
        box(draw, (863, 551, 1405, 686), BG)
        math(image, (876, 566), formulas[stage], size=25)
        if stage == 3:
            text(draw, (876, 620), 'Here, x = 1.', size=20, color=MUTED)
    else:
        box(draw, (865, 237, 1405, 734), BG)
        text(draw, (876, 244), 'Same effects, different averages', size=27, bold=True)
        for y,label,formula in [
            (307,'Entire population · 12 units',r'ATE=E[\Delta_i]=2.00'),
            (414,'Treated units · 6 units',r'ATT=E[\Delta_i\mid D_i=1]=3.50'),
            (521,'Characteristic Xᵢ = 1 · 4 units',r'CATE(1)=E[\Delta_i\mid X_i=1]=0.50')]:
            text(draw, (876,y), label, size=18, color=MUTED)
            math(image, (876,y+38), formula, size=22, color=TEAL)
        text(draw, (876, 655), 'The estimand changes with the target group.', size=18, color=MUTED)
    return image


EFFECTS = [-2,-1,0,1,1,2,2,3,3,4,5,6]
TREATED = [0,0,0,1,0,1,0,1,0,1,1,1]
CHARACTERISTIC = [1,0,1,1,0,0,0,1,0,0,0,0]
assert sum(EFFECTS)/12 == 2
assert sum(v for v,d in zip(EFFECTS,TREATED) if d)/sum(TREATED) == 3.5
assert sum(v for v,x in zip(EFFECTS,CHARACTERISTIC) if x)/sum(CHARACTERISTIC) == .5

source = Image.open(SOURCE)
palette_sample = Image.new('RGB', (1440, 810))
for slot, index in enumerate((0, 28)):
    source.seek(index)
    palette_sample.paste(source.convert('RGB').resize((720,810)), (slot*720,0))
palette = palette_sample.quantize(colors=192)
frames, durations, samples = [], [], {}
for i in range(source.n_frames):
    source.seek(i)
    frame = source.convert('RGB')
    active = [frame.getpixel((x,156))[0]<20 for x in (60,410,760,1110)]
    stage = 4 if all(active) else active.index(True)
    # Use the true source background for seamless replacement areas.
    BG = frame.getpixel((1400,700))
    updated = redraw(frame, stage)
    if stage not in samples or i==source.n_frames-1:
        samples[stage]=updated.copy()
    frames.append(updated.quantize(palette=palette, dither=Image.Dither.NONE))
    durations.append(source.info.get('duration',100))

DEST.mkdir(exist_ok=True)
gif=DEST/'causal-parameters-ite-ate-att-cate.gif'
frames[0].save(gif,save_all=True,append_images=frames[1:],duration=durations,loop=0,disposal=1,optimize=True)
samples[4].save(DEST/'causal-parameters-ite-ate-att-cate.png',dpi=(300,300))
qa=ROOT/'private'/'causal-parameters-gif-qa'
qa.mkdir(parents=True,exist_ok=True)
for stage,im in samples.items():
    im.save(qa/f'stage-{stage}.png')
print(f'{gif}\nFrames: {len(frames)}; duration: {sum(durations)/1000:.1f}s')
