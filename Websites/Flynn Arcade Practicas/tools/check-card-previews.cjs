const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const root = path.resolve(__dirname, '..');
const theme = path.join(root, 'flynns-arcade-neon');
const previews = JSON.parse(fs.readFileSync(path.join(theme, 'inc/card-previews.json'), 'utf8'));
const catalog = JSON.parse(fs.readFileSync(path.join(theme, 'inc/catalog.json'), 'utf8'));
const imports = JSON.parse(fs.readFileSync(path.join(root, 'checks/card-preview-import.json'), 'utf8'));
const output = path.join(root, 'checks/card-previews-' + Date.now());
const slug = (value) => value.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
const assert = (value, message) => { if (!value) throw new Error(message); };
const base = new URL(process.argv[2] || 'http://127.0.0.1:4173/');
const animatedRequests = (page) => page.evaluate(() => performance.getEntriesByType('resource').filter((entry) => entry.name.includes('/previews/')).map((entry) => entry.name));
const imageHas = (page, title, expected) => page.waitForFunction(({ title, expected }) => Array.from(document.querySelectorAll('[data-game-card]')).find((card) => card.dataset.title === title).querySelector('.game-media img').getAttribute('src') === expected, { title, expected });
let browser;

(async () => {
  fs.mkdirSync(output, { recursive: true });
  assert(Object.keys(previews).length === 31 && !previews['eternum-ex'], 'Only the 31 available GIFs should animate.');
  for (const entry of imports.filter((item) => item.animated)) {
    const metadata = await sharp(path.join(theme, 'assets/images', entry.image), { animated: true }).metadata();
    assert(metadata.pages > 1 && metadata.loop === 0, `${entry.game}: preview is not an animated loop.`);
    assert(metadata.delay.reduce((sum, delay) => sum + delay, 0) === entry.duration, `${entry.game}: original duration was lost.`);
  }
  assert(!imports.find((entry) => entry.game === 'the-last-cat-in-the-universe').source.includes('Viral'), 'Last Cat must not use Viral Reload artwork.');
  assert(!imports.find((entry) => entry.game === 'boardwalk-builders').source.includes('Viral'), 'Boardwalk must not use Viral Reload artwork.');
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const context = await browser.newContext({ viewport: { width: 1440, height: 1000 }, reducedMotion: 'no-preference' });
  await context.route('**/*', (route) => /^https?:/.test(route.request().url()) && new URL(route.request().url()).origin !== base.origin ? route.abort() : route.continue());
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const checks = [];
  const motion = [];
  for (const file of ['preview.html', 'games.html']) {
    await page.mouse.move(0, 0);
    await page.goto(new URL(file, base).href);
    assert((await animatedRequests(page)).length === 0, `${file}: animations load before interaction.`);
    assert(await page.locator('[data-game-preview]').count() === 31, `${file}: missing card previews.`);
    const more = page.locator('[data-load-more]');
    while (await more.isVisible()) await more.press('Enter');
    await page.mouse.move(0, 0);
    assert((await animatedRequests(page)).length === 0, 'Load more must not preload the GIF collection.');
    for (const [index, game] of catalog.entries()) {
      const card = page.locator('[data-catalog] [data-game-card]').nth(index);
      const link = card.locator('.game-link');
      const image = card.locator('.game-media img');
      const before = await card.boundingBox();
      const cover = await image.getAttribute('src');
      const href = await link.getAttribute('href');
      await image.hover();
      const preview = previews[slug(game.title)];
      if (preview) {
        await imageHas(page, game.title, 'assets/images/' + preview.image);
        if (['what-comes-after', 'donut-dodo', 'boardwalk-builders'].includes(slug(game.title))) {
          const frames = [];
          for (let attempt = 0; attempt < 8; attempt++) {
            const bounds = await image.boundingBox();
            // Inspect composited page pixels rather than the image element's default frame.
            const screenshot = await page.screenshot({ animations: 'allow' });
            frames.push(await sharp(screenshot).extract({ left: Math.round(bounds.x), top: Math.round(bounds.y), width: Math.floor(bounds.width), height: Math.floor(bounds.height) }).png().toBuffer());
            if (!frames[frames.length - 1].equals(frames[0])) break;
            await page.waitForTimeout(650);
          }
          assert(frames.some((frame) => !frame.equals(frames[0])), `${game.title}: the displayed preview is not moving.`);
          fs.writeFileSync(path.join(output, `${file}-${slug(game.title)}-frame-1.png`), frames[0]);
          fs.writeFileSync(path.join(output, `${file}-${slug(game.title)}-frame-2.png`), frames.find((frame) => !frame.equals(frames[0])));
          motion.push({ file, game: game.title, changingPixels: true });
        }
      } else {
        assert(await image.getAttribute('src') === cover, 'Eternum Ex should stay static.');
      }
      const after = await card.boundingBox();
      assert(before.width === after.width && before.height === after.height && href === await link.getAttribute('href'), `${game.title}: hover changed layout or navigation.`);
      await page.mouse.move(0, 0);
      await imageHas(page, game.title, cover);
      checks.push({ file, game: game.title, hover: Boolean(preview), staticRestored: true, stableDimensions: true });
    }
    await page.keyboard.press('Tab');
    const first = page.locator('[data-catalog] .game-link').first();
    await first.focus();
    await imageHas(page, catalog[0].title, 'assets/images/' + previews['what-comes-after'].image);
    await page.emulateMedia({ reducedMotion: 'reduce' });
    await imageHas(page, catalog[0].title, 'assets/images/' + catalog[0].image);
    await page.emulateMedia({ reducedMotion: 'no-preference' });
    await first.blur();
    await first.hover();
    await imageHas(page, catalog[0].title, 'assets/images/' + previews['what-comes-after'].image);
    await page.locator('[data-game-search]').fill('Donut Dodo');
    await imageHas(page, catalog[0].title, 'assets/images/' + catalog[0].image);
  }
  for (const file of ['about.html', 'game-donut-dodo.html']) {
    await page.goto(new URL(file, base).href);
    assert(await page.locator('[data-game-preview]').count() === 0, `${file}: animations must be scoped to Inicio and Games.`);
  }
  await context.close();

  const reduced = await browser.newContext({ reducedMotion: 'reduce' });
  const reducedPage = await reduced.newPage();
  await reducedPage.goto(new URL('games.html', base).href);
  await reducedPage.locator('[data-catalog] .game-link').first().hover();
  await reducedPage.keyboard.press('Tab');
  await reducedPage.locator('[data-catalog] .game-link').first().focus();
  assert((await animatedRequests(reducedPage)).length === 0, 'Reduced-motion users must not load animated previews.');
  await reduced.close();

  const touch = await browser.newContext({ viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true });
  const touchPage = await touch.newPage();
  const touchRequests = [];
  touchPage.on('request', (request) => { if (request.url().includes('/previews/')) touchRequests.push(request.url()); });
  await touchPage.goto(new URL('games.html', base).href);
  await touchPage.locator('[data-catalog] .game-link').first().tap();
  await touchPage.waitForURL('**/game-what-comes-after.html');
  assert(touchRequests.length === 0, 'Touch navigation should work with one tap, without downloading a GIF.');
  await touch.close();

  const failed = await browser.newContext();
  await failed.route('**/previews/*.webp', (route) => route.fulfill({ status: 404, body: '' }));
  const failedPage = await failed.newPage();
  await failedPage.goto(new URL('games.html', base).href);
  await failedPage.locator('[data-catalog] .game-link').first().hover();
  await failedPage.waitForTimeout(500);
  assert(await failedPage.locator('[data-catalog] .game-media img').first().getAttribute('src') === 'assets/images/' + catalog[0].image, 'A failed GIF must retain its cover.');
  await failed.close();

  const delayed = await browser.newContext();
  await delayed.route('**/previews/what-comes-after.webp', async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 800));
    await route.continue();
  });
  const delayedPage = await delayed.newPage();
  await delayedPage.goto(new URL('games.html', base).href);
  const download = delayedPage.waitForResponse((response) => response.url().endsWith('/previews/what-comes-after.webp'));
  await delayedPage.locator('[data-catalog] .game-link').first().hover();
  await delayedPage.mouse.move(0, 0);
  await download;
  await delayedPage.waitForTimeout(300);
  assert(await delayedPage.locator('[data-catalog] .game-media img').first().getAttribute('src') === 'assets/images/' + catalog[0].image, 'A delayed GIF must not activate after the pointer leaves.');
  await delayed.close();

  const local = await browser.newPage();
  await local.goto(pathToFileURL(path.join(theme, 'games.html')).href);
  await local.locator('[data-catalog] .game-link').first().hover();
  await imageHas(local, catalog[0].title, 'assets/images/' + previews['what-comes-after'].image);
  assert(errors.length === 0, errors.join('\n'));
  fs.writeFileSync(path.join(output, 'verification.json'), JSON.stringify({ checks, motion, animated: 31, static: ['eternum-ex'], keyboard: true, reducedMotion: true, filtersStopAnimation: true, touchNavigation: true, failedImageFallback: true, delayedImageFallback: true, filePreview: true, errors }, null, 2));
  console.log(JSON.stringify({ output, cardChecks: checks.length, motionSamples: motion.length, errors }, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
