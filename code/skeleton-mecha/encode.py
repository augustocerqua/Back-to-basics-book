"""Encode the rendered Three.js frames as an exactly 20-second looping GIF."""
from pathlib import Path
import tempfile
from PIL import Image

frames_dir = Path(tempfile.gettempdir()) / 'backbone-mecha-render' / 'frames'
output = Path(__file__).resolve().parents[2] / 'images' / 'backbone-skeleton-mecha-3d.gif'
paths = sorted(frames_dir.glob('[0-9][0-9][0-9][0-9].png'))
assert len(paths) == 250, f'Expected 250 frames, found {len(paths)}'
# One shared palette avoids color flicker during assembly.
samples = Image.new('RGB', (480 * 5, 360))
for col, index in enumerate([20, 75, 125, 170, 225]):
    with Image.open(paths[index]) as im:
        samples.paste(im.convert('RGB').resize((480, 360)), (480 * col, 0))
palette = samples.quantize(colors=256, method=Image.Quantize.MEDIANCUT)
frames = []
for i, p in enumerate(paths):
    with Image.open(p) as im:
        frames.append(im.convert('RGB').quantize(palette=palette, dither=Image.Dither.NONE))
    if i % 50 == 0:
        print(f'Encoding {i}/250', flush=True)
frames[0].save(output, save_all=True, append_images=frames[1:], duration=80, loop=0, optimize=False, disposal=1)
with Image.open(output) as gif:
    duration = 0
    for i in range(gif.n_frames):
        gif.seek(i)
        duration += gif.info.get('duration', 0)
    assert duration == 20000, duration
    print(f'{output}\n{gif.size}, {gif.n_frames} frames, {duration / 1000:.1f} seconds, {output.stat().st_size / 1e6:.1f} MB')
