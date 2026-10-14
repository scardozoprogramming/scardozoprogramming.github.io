const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const sourceCatalog = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../flynns-arcade-neon/inc/catalog.json'), 'utf8'));
const editorial = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../flynns-arcade-neon/inc/editorial.json'), 'utf8'));

let browser;
(async () => {
  browser = await chromium.launch({ headless: true, channel: 'msedge' });
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const url = pathToFileURL(path.resolve(__dirname, '../flynns-arcade-neon/preview.html')).href;
  const checks = path.resolve(__dirname, '../checks');
  fs.mkdirSync(checks, { recursive: true });
  const layouts = [];
  for (const [name, width, height] of [['desktop', 1672, 941], ['laptop', 1440, 900], ['tablet', 768, 1024], ['mobile', 390, 844], ['small-mobile', 320, 812]]) {
    await page.setViewportSize({ width, height });
    await page.goto(url);
    await page.evaluate(() => document.fonts.ready);
    await page.evaluate(async () => {
      const images = Array.from(document.querySelectorAll('main img'));
      images.forEach((image) => { image.loading = 'eager'; });
      await Promise.all(images.map((image) => image.decode()));
    });
    await page.screenshot({ path: path.join(checks, `${name}.png`), fullPage: true });
    layouts.push(await page.evaluate(({ name, width }) => {
      const bounds = (selector) => {
        const r = document.querySelector(selector).getBoundingClientRect();
        return { x: r.x, y: r.y, width: r.width, height: r.height };
      };
      const overflow = Array.from(document.querySelectorAll('main *, footer *')).filter((el) => {
        if (!(el instanceof HTMLElement)) return false;
        if (!el.getClientRects().length || el.classList.contains('screen-reader-text')) return false;
        const rect = el.getBoundingClientRect();
        return rect.right > width + 1 || rect.left < -1;
      }).map((el) => el.className);
      const textOverflow = Array.from(document.querySelectorAll('.game-title, .hero-title span, .section-title, .newsletter-panel h2')).filter((el) => el.scrollWidth > el.clientWidth + 1).map((el) => el.textContent);
      return { name, width, pageWidth: document.documentElement.scrollWidth, pageHeight: document.documentElement.scrollHeight, overflow, textOverflow, hero: bounds('.hero'), games: bounds('.games-section'), spotlight: bounds('.spotlight-section'), footer: bounds('.site-footer') };
    }, { name, width }));
  }
  await page.setViewportSize({ width: 1672, height: 941 });
  await page.goto(url);
  const homepageTitles = await page.locator('main .game-title').allTextContents();
  if (JSON.stringify(homepageTitles) !== JSON.stringify(sourceCatalog.slice(0, 6).map((game) => game.title))) throw new Error('Homepage order differs from the reference catalog.');
  await page.getByRole('link', { name: 'View All Games' }).click();
  if (await page.locator('#catalog').evaluate((el) => el.open) !== true) throw new Error('Catalog did not open.');
  if (await page.locator('.catalog-grid .game-card:visible').count() !== sourceCatalog.length) throw new Error('Catalog count is incorrect.');
  await page.locator('#catalog img').evaluateAll(async (images) => {
    images.forEach((image) => { image.loading = 'eager'; });
    await Promise.all(images.map((image) => image.decode()));
  });
  await page.getByRole('button', { name: 'Narrative', exact: true }).click();
  if (await page.locator('.catalog-grid .game-card:visible').count() !== sourceCatalog.filter((game) => game.categories.includes('narrative')).length) throw new Error('Narrative filter failed.');
  await page.locator('[data-game-search]').fill('no-matching-game');
  if (!(await page.locator('[data-empty-state]').isVisible())) throw new Error('Empty search state failed.');
  await page.getByRole('button', { name: 'All games', exact: true }).click();
  await page.locator('[data-game-search]').fill('Donut');
  if (await page.locator('.catalog-grid .game-card:visible').count() !== 1) throw new Error('Search failed.');
  await page.keyboard.press('Escape');
  await page.locator('main').getByRole('button', { name: 'View What Comes After', exact: true }).click();
  if (await page.locator('[data-detail-title]').textContent() !== 'What Comes After') throw new Error('Game details failed.');
  await page.keyboard.press('Escape');
  await page.locator('.site-nav').getByRole('link', { name: 'About', exact: true }).click();
  if (await page.locator('#about').evaluate((el) => el.open) !== true) throw new Error('About did not open.');
  await page.keyboard.press('Escape');
  await page.getByRole('link', { name: 'Discover Indies', exact: true }).click();
  if (await page.locator('.studio-list article').count() !== editorial.studios.length) throw new Error('Studio list is incomplete.');
  await page.keyboard.press('Escape');
  await page.locator('.site-nav').getByRole('link', { name: 'Press', exact: true }).click();
  if (await page.locator('.press-mention').count() !== editorial.press.length) throw new Error('Press list is incomplete.');
  if (await page.locator('.press-email').textContent() !== editorial.contact_email) throw new Error('Press contact does not match the source.');
  await page.keyboard.press('Escape');
  await page.getByRole('button', { name: 'Next spotlight', exact: true }).click();
  if (await page.locator('[data-spotlight-title]').textContent() !== editorial.spotlights[1].title) throw new Error('Spotlight controls failed.');
  await page.getByRole('button', { name: 'Previous spotlight', exact: true }).click();
  await page.locator('#newsletter-email').fill('preview@example.test');
  await page.getByRole('button', { name: 'Subscribe', exact: true }).click();
  if (!(await page.locator('[data-newsletter-note]').isVisible())) throw new Error('Newsletter status is missing.');
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(url);
  await page.locator('.nav-toggle').click();
  if (await page.locator('.nav-toggle').getAttribute('aria-expanded') !== 'true') throw new Error('Mobile navigation failed.');
  await page.locator('.site-nav').getByRole('link', { name: 'Indies', exact: true }).click();
  if (await page.locator('.nav-toggle').getAttribute('aria-expanded') !== 'false') throw new Error('Mobile navigation did not close.');
  if (errors.length) throw new Error(errors.join('\n'));
  const externalLinks = await page.locator('a').evaluateAll((links) => links.map((link) => link.getAttribute('href')).filter((href) => /^https?:\/\//i.test(href || '')));
  if (externalLinks.length) throw new Error(`Unexpected external links: ${externalLinks.join(', ')}`);
  if (layouts.some((layout) => layout.pageWidth > layout.width || layout.overflow.length || layout.textOverflow.length)) throw new Error(JSON.stringify(layouts));
  console.log(JSON.stringify({ layouts, errors, catalog: sourceCatalog.length, studios: editorial.studios.length, press: editorial.press.length, interactions: 'Reference content, local artwork, catalog, filters, search, game details, company dialogs, spotlight, newsletter status, and mobile navigation passed.' }, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
