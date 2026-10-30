"""Build lightweight Home media without changing the original files."""
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
from PIL import Image, ImageSequence
import json

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'WordPress' / 'Inicio-Cardozo' / 'Materiales-optimizados' / 'Para-subir'

def optimize(entry):
    slug = entry['slug']
    folder = ROOT / 'Games' / 'Juegos' / slug / 'Images'
    src = folder / 'preview.webp'
    target = OUT / f'fc-home-{slug}-hover.webp'
    static = OUT / f'fc-home-{slug}-cover.webp'
    if target.exists() and static.exists() and target.stat().st_size <= 650_000:
        with Image.open(target) as check:
            details = {'width': check.width, 'height': check.height, 'frames': check.n_frames}
        return {'slug': slug, 'title': entry['title'], 'animation': target.name, 'cover': static.name, 'sourceBytes': src.stat().st_size, 'animationBytes': target.stat().st_size, 'coverBytes': static.stat().st_size, **details}
    frames, durations, elapsed = [], [], 0
    with Image.open(src) as image:
        for frame in ImageSequence.Iterator(image):
            frame.load()
            duration = max(20, int(image.info.get('duration', 100)))
            frames.append(frame.convert('RGB').copy())
            durations.append(duration)
            elapsed += duration
            if elapsed >= 6000:
                break
    start = min(700, max(0, elapsed - 4000))
    end = min(elapsed, start + 4000)
    chosen = []
    next_sample, elapsed = start, 0
    for frame, duration in zip(frames, durations):
        if elapsed + duration > next_sample and next_sample < end:
            while next_sample < min(elapsed + duration, end):
                chosen.append(frame)
                next_sample += 100
        elapsed += duration
    if len(chosen) < 2:
        raise ValueError(f'Animation missing for {slug}')
    for width, quality in [(480, 68), (420, 58), (400, 50)]:
        sized = [f.resize((min(width, f.width), round(f.height * min(width, f.width) / f.width)), Image.Resampling.LANCZOS) for f in chosen]
        sized[0].save(target, format='WEBP', save_all=True, append_images=sized[1:], duration=100, loop=0, quality=quality, method=4, minimize_size=True)
        if target.stat().st_size <= 650_000:
            break
    cover = next((folder / name for name in ['cover.jpg', 'cover.jpeg', 'cover.webp', 'cover.png'] if (folder / name).exists()), None)
    if cover is None:
        raise FileNotFoundError(f'Cover missing for {slug}')
    static = OUT / f'fc-home-{slug}-cover.webp'
    with Image.open(cover) as image:
        image = image.convert('RGB')
        image.thumbnail((768, 768), Image.Resampling.LANCZOS)
        image.save(static, format='WEBP', quality=78, method=5)
    with Image.open(target) as check:
        if check.n_frames < 2:
            raise ValueError(f'Output is not animated: {slug}')
        details = {'width': check.width, 'height': check.height, 'frames': check.n_frames}
    return {'slug': slug, 'title': entry['title'], 'animation': target.name, 'cover': static.name, 'sourceBytes': src.stat().st_size, 'animationBytes': target.stat().st_size, 'coverBytes': static.stat().st_size, **details}

if __name__ == '__main__':
    OUT.mkdir(exist_ok=True, parents=True)
    entries = json.loads((ROOT / 'Games' / 'JSON' / 'catalog.json').read_text(encoding='utf-8'))
    with ProcessPoolExecutor(max_workers=3) as pool:
        results = []
        for item in pool.map(optimize, entries):
            results.append(item)
            print(json.dumps(item), flush=True)
    report = {'games': len(results), 'sourceBytes': sum(x['sourceBytes'] for x in results), 'animationBytes': sum(x['animationBytes'] for x in results), 'coverBytes': sum(x['coverBytes'] for x in results), 'files': results}
    (OUT.parent / 'manifest.json').write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding='utf-8')
    print(json.dumps({k: v for k, v in report.items() if k != 'files'}), flush=True)
