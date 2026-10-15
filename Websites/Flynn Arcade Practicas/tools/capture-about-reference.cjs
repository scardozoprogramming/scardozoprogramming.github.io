const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const checks = path.resolve(__dirname, '../checks');
const assets = path.resolve(__dirname, '../flynns-arcade-neon/assets/images/about');
let browser;

(async () => {
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
  const response = await page.goto('https://www.flynnsarcades.com/about-3/', { waitUntil: 'domcontentloaded', timeout: 60000 });
  if (!response.ok()) throw new Error(`Reference returned ${response.status()}`);
  fs.writeFileSync(path.join(checks, 'about-source.html'), await response.text());
  await page.locator('.elementor-gallery-item').first().waitFor();
  const reference = await page.locator('[data-elementor-type="wp-page"]').evaluate((main) => {
    const text = (el) => el.textContent.trim().replace(/\s+/g, ' ');
    const bounds = (el) => {
      const rect = el.getBoundingClientRect();
      return { x: rect.x, y: rect.y, width: rect.width, height: rect.height };
    };
    return {
      sections: Array.from(main.querySelectorAll('h2')).filter((el) => ['Description', 'Early history', 'After that'].includes(text(el))).map((heading) => ({
        title: text(heading),
        paragraphs: Array.from(heading.closest('.elementor-column').querySelectorAll('p')).map(text).filter(Boolean),
      })),
      gallery: Array.from(main.querySelectorAll('.elementor-gallery-item')).map((link, index) => ({
        source: link.href,
        image: `about/gallery-${String(index + 1).padStart(2, '0')}.jpg`,
      })),
      projects: Array.from(main.querySelectorAll('.eael-image-accordion-item')).map((item, index) => ({
        title: text(item.querySelector('h2')),
        copy: Array.from(item.querySelectorAll('p')).map(text).filter(Boolean)[0],
        source: item.style.backgroundImage.slice(5, -2),
        image: `about/project-${index + 1}.webp`,
      })),
      layout: Array.from(main.children).map((section) => ({
        headings: Array.from(section.querySelectorAll('h2')).map(text),
        bounds: bounds(section),
        columns: Array.from(section.querySelector(':scope > .elementor-container')?.children || []).map(bounds),
      })),
    };
  });
  if (reference.gallery.length !== 6 || reference.sections.length !== 3 || reference.projects.length !== 2) throw new Error('The source structure changed; review it before importing.');
  fs.mkdirSync(assets, { recursive: true });
  for (const asset of [...reference.gallery, ...reference.projects]) {
    const remote = new URL(asset.source);
    if (remote.origin !== 'https://www.flynnsarcades.com' || !remote.pathname.startsWith('/wp-content/uploads/')) throw new Error('Unexpected asset source.');
    const image = await page.request.get(asset.source, { timeout: 60000 });
    if (!image.ok() || !image.headers()['content-type']?.startsWith('image/')) throw new Error(`Unable to download ${asset.source}`);
    const buffer = await image.body();
    if (asset.image.endsWith('.webp')) {
      fs.writeFileSync(path.join(checks, `${path.basename(asset.image, '.webp')}-source.png`), buffer);
      await sharp(buffer).webp({ lossless: true }).toFile(path.join(assets, path.basename(asset.image)));
    } else {
      fs.writeFileSync(path.join(assets, path.basename(asset.image)), buffer);
    }
  }
  fs.writeFileSync(path.join(checks, 'about-reference.json'), JSON.stringify(reference, null, 2));
  for (const section of await page.locator('[data-elementor-type="wp-page"] > section').all()) {
    await section.scrollIntoViewIfNeeded();
    await section.evaluate((el) => { el.classList.remove('elementor-invisible'); el.style.animation = 'none'; });
  }
  await page.evaluate(async () => { await document.fonts.ready; window.scrollTo(0, 0); });
  await page.screenshot({ path: path.join(checks, 'about-original-desktop.png'), fullPage: true });
  console.log(JSON.stringify(reference, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
