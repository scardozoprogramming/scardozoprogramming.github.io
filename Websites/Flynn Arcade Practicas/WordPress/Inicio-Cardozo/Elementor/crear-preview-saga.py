"""Create a short animated WebP from the user's own Saga gameplay video."""
from pathlib import Path
import argparse
import json
import subprocess
from PIL import Image

parser = argparse.ArgumentParser()
parser.add_argument('--ffmpeg', required=True)
parser.add_argument('--input', required=True)
parser.add_argument('--work', required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[3]
work = Path(args.work)
work.mkdir(exist_ok=True, parents=True)
source = Path(args.input)
command = [args.ffmpeg, '-y', '-hide_banner', '-ss', '2', '-i', str(source), '-t', '4', '-an', '-vf', 'fps=10,scale=480:-2:flags=lanczos', str(work / 'frame-%03d.png')]
conversion = subprocess.run(command, capture_output=True, check=True)
(work / 'video-info.txt').write_bytes(conversion.stderr)
frames = [Image.open(file).convert('RGB') for file in sorted(work.glob('frame-*.png'))]
assert len(frames) > 1
output = root / 'WordPress' / 'Inicio-Cardozo' / 'Materiales-optimizados' / 'Saga' / 'fc-home-saga-hover.webp'
output.parent.mkdir(exist_ok=True)
frames[0].save(output, format='WEBP', save_all=True, append_images=frames[1:], duration=100, loop=0, quality=68, method=4, minimize_size=True)
if output.stat().st_size > 450_000:
    smaller = [frame.resize((420, 236), Image.Resampling.LANCZOS) for frame in frames]
    smaller[0].save(output, format='WEBP', save_all=True, append_images=smaller[1:], duration=100, loop=0, quality=60, method=4, minimize_size=True)
with Image.open(output) as check:
    report = {'source': source.name, 'sourceBytes': source.stat().st_size, 'output': output.name, 'bytes': output.stat().st_size, 'frames': check.n_frames, 'width': check.width, 'height': check.height, 'durationMs': len(frames) * 100, 'startSeconds': 2}
(output.parent.parent / 'saga-manifest.json').write_text(json.dumps(report, indent=2), encoding='utf-8')
print(json.dumps(report))
