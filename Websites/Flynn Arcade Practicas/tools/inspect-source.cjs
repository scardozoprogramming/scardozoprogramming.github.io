const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');

(async () => {
  const browser = await chromium.launch({ channel: 'msedge', headless: true });
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    await page.goto('https://www.flynnsarcades.com/', { waitUntil: 'domcontentloaded', timeout: 60000 });
    await page.locator('h2').first().waitFor();
    await page.screenshot({ path: path.resolve('checks/source-home.png'), fullPage: true });
    console.log(JSON.stringify(await page.evaluate(() => ({
      headings: Array.from(document.querySelectorAll('h1,h2,h3,h4')).map((el) => ({ tag: el.tagName, text: el.textContent.trim() })).filter((el) => el.text),
      logos: Array.from(document.querySelectorAll('header img, [data-elementor-type="header"] img')).map((el) => ({ src: el.src, alt: el.alt, width: el.width, height: el.height })),
      socialLinks: Array.from(document.querySelectorAll('a.elementor-social-icon')).map((el) => ({ label: el.textContent.trim(), href: el.href })),
      media: Array.from(document.querySelectorAll('video, iframe')).map((el) => ({ tag: el.tagName, src: el.src, poster: el.poster })),
      backgrounds: Array.from(document.querySelectorAll('[data-settings], .swiper-slide-bg')).map((el) => ({ id: el.getAttribute('data-id'), settings: el.getAttribute('data-settings'), background: getComputedStyle(el).backgroundImage })).filter((el) => el.background !== 'none'),
    })), null, 2));
  } finally { await browser.close(); }
})().catch((error) => { console.error(error); process.exitCode = 1; });
