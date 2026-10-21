const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const directory = path.resolve(__dirname, '../checks/game-reference');
const catalog = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../flynns-arcade-neon/inc/catalog.json'), 'utf8'));
const slug = (text) => text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
let browser;

(async () => {
  fs.mkdirSync(directory, { recursive: true });
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  let home;
  try {
    if (process.argv.includes('--reuse-home')) {
      home = fs.readFileSync(path.join(directory, 'catalog-home.html'), 'utf8');
    } else {
      const response = await page.request.get('https://www.flynnsarcades.com/', { timeout: 60000 });
      if (!response.ok()) throw new Error(`Homepage returned ${response.status()}`);
      home = await response.text();
      fs.writeFileSync(path.join(directory, 'catalog-home.html'), home);
    }
  } catch (error) {
    home = fs.readFileSync('C:/Users/santi/AppData/Local/Temp/flynns-source-snapshots/home.html', 'utf8');
    console.log(`Using previous source snapshot: ${error.message}`);
  }
  const entries = await page.evaluate((html) => {
    const doc = new DOMParser().parseFromString(html, 'text/html');
    const fragments = [doc];
    for (const element of doc.querySelectorAll('*')) {
      for (const attribute of element.attributes) {
        if (attribute.value.length < 2000) continue;
        try {
          const items = JSON.parse(atob(attribute.value));
          if (Array.isArray(items)) for (const item of items) {
            if (typeof item === 'string' && item.includes('fg-item-title')) fragments.push(new DOMParser().parseFromString(item, 'text/html'));
          }
        } catch {}
      }
    }
    const found = new Map();
    for (const fragment of fragments) for (const item of fragment.querySelectorAll('.eael-filterable-gallery-item-wrap')) {
      const title = item.querySelector('.fg-item-title')?.textContent.trim();
      if (!title || found.has(title)) continue;
      const stores = Array.from(item.querySelectorAll('.fg-item-content a[href]')).filter((link) => link.querySelector('img')).map((link) => ({ url: link.getAttribute('href'), icon: link.querySelector('img').getAttribute('src') }));
      found.set(title, { title, source: item.querySelector('.fg-item-content p a')?.getAttribute('href') || '', tagline: item.querySelector('.fg-item-content p')?.textContent.trim() || '', stores });
    }
    return Array.from(found.values());
  }, home);
  const manifest = catalog.map((game) => {
    const entry = entries.find((item) => slug(item.title) === slug(game.title));
    if (!entry?.source) throw new Error(`Missing original catalog link for ${game.title}`);
    return { ...entry, slug: slug(game.title), localTitle: game.title };
  });
  fs.writeFileSync(path.join(directory, 'manifest.json'), JSON.stringify(manifest, null, 2));
  console.log(JSON.stringify(manifest.map(({ localTitle, source }) => ({ title: localTitle, source })), null, 2));
  if (process.argv.includes('--discover-only')) { await browser.close(); return; }
  for (const game of manifest) {
    const target = path.join(directory, `${game.slug}.html`);
    if (fs.existsSync(target)) continue;
    const origin = new URL(game.source).hostname;
    if (!/(^|\.)flynnsarcades\.com$/.test(origin)) { console.log(`External catalog target: ${game.localTitle}: ${game.source}`); continue; }
    try {
      const response = await page.request.get(game.source, { timeout: 45000 });
      if (!response.ok()) throw new Error(`HTTP ${response.status()}`);
      fs.writeFileSync(target, await response.text());
      console.log(`Captured: ${game.localTitle}`);
    } catch (error) { console.log(`FAILED ${game.localTitle}: ${error.message}`); }
  }
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
