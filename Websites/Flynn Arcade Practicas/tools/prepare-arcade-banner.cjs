const fs = require('node:fs');
const path = require('node:path');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');

(async () => {
  const source = process.argv[2];
  if (!source) throw new Error('Provide the generated panoramic PNG path.');
  const original = path.resolve(__dirname, '../docs/assets/hero-arcade-banner-original.png');
  const output = path.resolve(__dirname, '../flynns-arcade-neon/assets/images/hero-arcade-banner.webp');
  const metadata = await sharp(source).metadata();
  if (metadata.width !== 2172 || metadata.height !== 724) throw new Error('Review banner dimensions before updating its template.');
  fs.mkdirSync(path.dirname(original), { recursive: true });
  fs.copyFileSync(source, original);
  await sharp(source).webp({ quality: 90, effort: 5 }).toFile(output);
  console.log(JSON.stringify({ output, original, width: metadata.width, height: metadata.height, bytes: fs.statSync(output).size }));
})().catch((error) => { console.error(error); process.exitCode = 1; });
