const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const directory = path.resolve(__dirname, '../checks/game-reference');
fs.mkdirSync(directory, { recursive: true });
let browser;

(async () => {
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
  const source = process.argv[2] || 'https://www.flynnsarcades.com/what-comes-after/';
  const response = await page.goto(source, { waitUntil: 'domcontentloaded', timeout: 60000 });
  if (!response?.ok()) throw new Error(`Reference returned ${response?.status()}`);
  const slug = new URL(source).pathname.split('/').filter(Boolean).pop() || 'home';
  fs.writeFileSync(path.join(directory, `${slug}.html`), await response.text());
  const result = await page.evaluate(() => {
    const main = document.querySelector('[data-elementor-type="wp-page"]') || document.querySelector('main');
    const text = (el) => el.textContent.trim().replace(/\s+/g, ' ');
    return {
      sections: Array.from(main.children).map((section) => ({
        tag: section.tagName,
        id: section.getAttribute('data-id'),
        headings: Array.from(section.querySelectorAll('h1,h2,h3,h4')).map((el) => ({ tag: el.tagName, text: text(el) })),
        paragraphs: Array.from(section.querySelectorAll('p')).map(text).filter(Boolean),
        widgets: Array.from(section.querySelectorAll('[data-widget_type]')).map((el) => ({ id: el.getAttribute('data-id'), type: el.getAttribute('data-widget_type'), text: text(el).slice(0, 220) })),
        links: Array.from(section.querySelectorAll('a[href]')).map((el) => ({ text: text(el), href: el.getAttribute('href') })),
        images: Array.from(section.querySelectorAll('img')).map((el) => ({ src: el.getAttribute('src'), alt: el.alt, srcset: el.getAttribute('srcset') })),
        settings: Array.from(section.querySelectorAll('[data-settings]')).map((el) => ({ id: el.getAttribute('data-id'), settings: JSON.parse(el.getAttribute('data-settings')) })),
      })),
    };
  });
  fs.writeFileSync(path.join(directory, `${slug}-structure.json`), JSON.stringify(result, null, 2));
  await page.screenshot({ path: path.join(directory, `${slug}-${Date.now()}.png`), fullPage: true });
  console.log(JSON.stringify(result, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
