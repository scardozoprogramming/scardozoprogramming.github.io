"""Losslessly wrap the existing TTF tables in WOFF 1.0 (W3C format)."""
from pathlib import Path
import json
import struct
import zlib

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'WordPress' / 'Inicio-Cardozo' / 'Materiales-optimizados' / 'Fuentes'
OUT.mkdir(exist_ok=True)
align = lambda size: (size + 3) & ~3
report = []
for name in ['barlow-regular', 'barlow-bold', 'press-start-2p']:
    original = (ROOT / 'Compartido' / 'Fonts' / f'{name}.ttf').read_bytes()
    flavor, count = struct.unpack_from('>IH', original)
    records = [struct.unpack_from('>4sIII', original, 12 + 16 * i) for i in range(count)]
    offset, directory, payload = 44 + count * 20, bytearray(), bytearray()
    positions = {}
    for tag, checksum, start, length in sorted(records, key=lambda r: r[2]):
        table = original[start:start + length]
        packed = zlib.compress(table, 9)
        packed = packed if len(packed) < length else table
        assert (zlib.decompress(packed) if len(packed) < length else packed) == table
        positions[tag] = (offset, len(packed))
        payload.extend(packed)
        payload.extend(b'\0' * (align(len(packed)) - len(packed)))
        offset += align(len(packed))
    for tag, checksum, start, length in records:
        position, compressed_length = positions[tag]
        directory.extend(struct.pack('>4sIIII', tag, position, compressed_length, length, checksum))
    total_sfnt = 12 + count * 16 + sum(align(r[3]) for r in records)
    header = struct.pack('>IIIHHIHHIIIII', 0x774F4646, flavor, offset, count, 0, total_sfnt, 1, 0, 0, 0, 0, 0, 0)
    result = header + directory + payload
    assert len(result) == offset
    (OUT / f'{name}.woff').write_bytes(result)
    report.append({'font': name, 'ttfBytes': len(original), 'woffBytes': len(result)})
(OUT / 'manifest.json').write_text(json.dumps(report, indent=2), encoding='utf-8')
print(json.dumps(report))
