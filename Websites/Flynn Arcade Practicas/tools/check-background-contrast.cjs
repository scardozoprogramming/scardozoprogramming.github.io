const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const theme = path.resolve(__dirname, '../flynns-arcade-neon');
const luminance = (rgb) => rgb.map((n) => n / 255).map((n) => n <= .04045 ? n / 12.92 : ((n + .055) / 1.055) ** 2.4).reduce((sum, n, i) => sum + n * [.2126, .7152, .0722][i], 0);
let browser;

(async () => {
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  await page.route(/^https?:\/\//, (route) => route.abort());
  const results = [];
  const targets = [
    ['preview.html', '.featured-game-description'],
    ['about.html', '.about-story p'],
    ['about.html', '.about-story h2'],
    ['indies.html', '.studio-description p'],
    ['indies.html', '.studio-games'],
    ['press.html', '.press-copy > p:not(.eyebrow)'],
    ['press.html', '.presskit-download'],
    ['press.html', '.social-band p'],
    ['game-eternum-ex.html', '.game-detail-copy > p:not(.eyebrow)'],
  ];
  for (const width of [1440, 390]) {
    await page.setViewportSize({ width, height: 900 });
    for (const [file, selector] of targets) {
      await page.goto(pathToFileURL(path.join(theme, file)).href);
      await page.evaluate(async () => {
        await document.fonts.ready;
        const background = new Image();
        background.src = getComputedStyle(document.body, '::before').backgroundImage.match(/url\("?([^"\)]+)"?\)/)[1];
        await background.decode();
      });
      const text = await page.locator(selector).first().evaluate((el) => {
        el.scrollIntoView({ block: 'center', behavior: 'instant' });
        const range = document.createRange();
        range.selectNodeContents(el);
        return { color: getComputedStyle(el).color, rects: Array.from(range.getClientRects()).map((rect) => ({ x: rect.x, y: rect.y, right: rect.right, bottom: rect.bottom })) };
      });
      await page.addStyleTag({ content: 'body * { color: transparent !important; text-shadow: none !important; }' });
      const { data, info } = await sharp(await page.screenshot()).removeAlpha().raw().toBuffer({ resolveWithObject: true });
      let brightest = 0;
      for (const rect of text.rects) {
        for (let y = Math.max(0, Math.ceil(rect.y)); y < Math.min(info.height, Math.floor(rect.bottom)); y++) {
          for (let x = Math.max(0, Math.ceil(rect.x)); x < Math.min(info.width, Math.floor(rect.right)); x++) {
            const offset = (y * info.width + x) * info.channels;
            brightest = Math.max(brightest, luminance([data[offset], data[offset + 1], data[offset + 2]]));
          }
        }
      }
      const foreground = luminance(text.color.match(/[\d.]+/g).slice(0, 3).map(Number));
      const ratio = (foreground + .05) / (brightest + .05);
      results.push({ width, file, selector, ratio: Number(ratio.toFixed(2)), pass: ratio >= 4.5 });
    }
  }
  const output = path.resolve(__dirname, `../checks/background-contrast-${Date.now()}.json`);
  fs.writeFileSync(output, JSON.stringify(results, null, 2));
  console.log(JSON.stringify({ output, samples: results.length, minimumRatio: Math.min(...results.map((result) => result.ratio)), passed: results.every((result) => result.pass) }, null, 2));
  if (results.some((result) => !result.pass)) throw new Error('Some text does not meet 4.5:1 contrast against the rendered background.');
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
