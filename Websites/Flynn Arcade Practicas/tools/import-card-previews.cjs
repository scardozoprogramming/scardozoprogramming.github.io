const { chromium, request } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
const theme = path.join(root, 'flynns-arcade-neon');
const references = path.join(root, 'checks/game-reference');
const originals = path.join(root, 'checks/card-preview-originals');
const manifest = JSON.parse(fs.readFileSync(path.join(references, 'manifest.json'), 'utf8'));
const available = {};
const report = [];
let browser;
let client;

(async () => {
  fs.mkdirSync(originals, { recursive: true });
  fs.mkdirSync(path.join(theme, 'assets/images/previews'), { recursive: true });
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  client = await request.newContext({ timeout: 60000 });
  for (const game of manifest) {
    const html = fs.readFileSync(path.join(references, `${game.slug}.html`), 'utf8');
    const source = await page.evaluate((html) => {
      const doc = new DOMParser().parseFromString(html, 'text/html');
      const main = doc.querySelector('[data-elementor-type="wp-page"]') || doc.querySelector('main');
      const body = Array.from(main.children).find((section) => section.querySelector('[data-widget_type="text-editor.default"]'));
      const candidates = [];
      for (const element of main.querySelectorAll('[src],[href],[style]')) {
        if (element.closest('.eael-image-accordion-item')) continue;
        for (const value of [element.getAttribute('src'), element.getAttribute('href'), element.style.backgroundImage?.match(/url\(["']?(.*?)["']?\)/)?.[1]]) {
          try {
            const url = new URL(value);
            if (url.hostname !== 'www.flynnsarcades.com' || !url.pathname.startsWith('/wp-content/uploads/') || !/\.gif$/i.test(url.pathname)) continue;
            const score = body?.contains(element) && element.closest('[data-widget_type="image.default"]') ? 3 : body?.contains(element) ? 2 : 1;
            candidates.push({ url: url.href, score });
          } catch {}
        }
      }
      return candidates.sort((a, b) => b.score - a.score)[0]?.url || null;
    }, html);
    if (!source) {
      if (game.slug !== 'eternum-ex') throw new Error(`Missing original GIF: ${game.title}`);
      report.push({ game: game.slug, animated: false, reason: 'The user confirmed this game has no GIF yet; retain its static cover.' });
      continue;
    }
    const original = path.join(originals, `${game.slug}.gif`);
    if (!fs.existsSync(original)) {
      const response = await client.get(source);
      if (!response.ok()) throw new Error(`GIF HTTP ${response.status()}: ${source}`);
      fs.writeFileSync(original, await response.body());
    }
    const input = await sharp(original, { animated: true }).metadata();
    if (input.format !== 'gif' || input.pages < 2) throw new Error(`Not an animated GIF: ${game.title}`);
    const image = `previews/${game.slug}.webp`;
    const target = path.join(theme, 'assets/images', image);
    if (!fs.existsSync(target)) {
      await sharp(original, { animated: true }).resize({ width: 640, withoutEnlargement: true }).webp({ quality: 72, effort: 3, loop: 0 }).toFile(target);
    }
    const output = await sharp(target, { animated: true }).metadata();
    if (output.pages < 2 || output.delay.reduce((sum, delay) => sum + delay, 0) !== input.delay.reduce((sum, delay) => sum + delay, 0)) throw new Error(`Animation duration changed: ${game.title}`);
    available[game.slug] = { image, width: output.width, height: output.pageHeight };
    report.push({ game: game.slug, animated: true, source, image, sourceFrames: input.pages, frames: output.pages, width: output.width, height: output.pageHeight, duration: output.delay.reduce((sum, delay) => sum + delay, 0), originalBytes: fs.statSync(original).size, bytes: fs.statSync(target).size });
    console.log(`Prepared ${game.title}: ${output.pages} frames, ${fs.statSync(target).size} bytes.`);
  }
  fs.writeFileSync(path.join(theme, 'inc/card-previews.json'), JSON.stringify(available, null, 2) + '\n');
  fs.writeFileSync(path.join(root, 'checks/card-preview-import.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify({ animated: Object.keys(available).length, static: manifest.length - Object.keys(available).length, bytes: report.reduce((sum, item) => sum + (item.bytes || 0), 0) }));
  await client.dispose();
  await browser.close();
})().catch(async (error) => { console.error(error); await client?.dispose(); await browser?.close(); process.exitCode = 1; });
