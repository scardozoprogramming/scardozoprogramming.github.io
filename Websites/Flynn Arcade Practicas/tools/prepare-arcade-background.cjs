const fs = require('node:fs');
const path = require('node:path');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');

(async () => {
  const source = process.argv[2];
  if (!source) throw new Error('Provide the generated PNG path.');
  const assets = path.resolve(__dirname, '../flynns-arcade-neon/assets/images/backgrounds');
  const originals = path.resolve(__dirname, '../docs/assets');
  fs.mkdirSync(assets, { recursive: true });
  fs.mkdirSync(originals, { recursive: true });
  fs.copyFileSync(source, path.join(originals, 'arcade-city-original.png'));
  await sharp(source).resize({ width: 1920, withoutEnlargement: true }).webp({ quality: 86 }).toFile(path.join(assets, 'arcade-city.webp'));
  await sharp(source).resize(720, 1280, { fit: 'cover', position: 'right' }).webp({ quality: 84 }).toFile(path.join(assets, 'arcade-city-mobile.webp'));
  for (const file of ['arcade-city.webp', 'arcade-city-mobile.webp']) {
    const metadata = await sharp(path.join(assets, file)).metadata();
    console.log(JSON.stringify({ file, width: metadata.width, height: metadata.height, bytes: fs.statSync(path.join(assets, file)).size }));
  }
})().catch((error) => { console.error(error); process.exitCode = 1; });
