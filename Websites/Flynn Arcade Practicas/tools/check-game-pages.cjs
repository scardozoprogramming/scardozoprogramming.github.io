const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const theme = path.resolve(__dirname, '../flynns-arcade-neon');
const catalog = JSON.parse(fs.readFileSync(path.join(theme, 'inc/catalog.json'), 'utf8'));
const details = JSON.parse(fs.readFileSync(path.join(theme, 'inc/game-details.json'), 'utf8'));
const output = path.resolve(__dirname, '../checks/game-pages-' + Date.now());
const slug = (text) => text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
const url = (file) => pathToFileURL(path.join(theme, file)).href;
const assert = (value, message) => { if (!value) throw new Error(message); };
const samples = ['what-comes-after', 'donut-dodo', 'the-last-cat-in-the-universe', 'flora-fang-guardians-of-the-vampire-garden', 'galacticon', 'eternum-ex', 'papertris'];
let browser;

(async () => {
  fs.mkdirSync(output, { recursive: true });
  assert(Object.keys(details).length === catalog.length, 'Each catalog game must have its own detail record.');
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  await page.route(/^https?:\/\//, (route) => route.abort());
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const checks = [];
  for (const width of [1440, 390, 1191, 768, 320]) {
    await page.setViewportSize({ width, height: width > 600 ? 1000 : 844 });
    for (const game of catalog) {
      const id = slug(game.title);
      if (![1440, 390].includes(width) && !samples.includes(id)) continue;
      const expected = details[id];
      const file = `game-${id}.html`;
      await page.goto(url(file));
      await page.evaluate(async () => {
        await document.fonts.ready;
        const images = Array.from(document.images).filter((image) => image.getAttribute('src'));
        images.forEach((image) => { image.loading = 'eager'; });
        await Promise.all(images.map((image) => image.decode()));
      });
      const actual = await page.evaluate(() => {
        const text = (el) => el.textContent.trim().replace(/\s+/g, ' ');
        const blocks = Array.from(document.querySelectorAll('.game-description > *, .game-features > *')).map((el) => el.tagName === 'UL' ? { type: 'list', items: Array.from(el.children).map(text) } : { type: el.tagName === 'H3' ? 'heading' : 'paragraph', text: text(el) });
        const overflow = Array.from(document.querySelectorAll('body *')).filter((el) => {
          if (!(el instanceof HTMLElement) || !el.getClientRects().length || el.matches('.screen-reader-text,.publisher-logo img')) return false;
          const r = el.getBoundingClientRect();
          return r.left < -1 || r.right > innerWidth + 1;
        }).map((el) => el.className || el.tagName);
        const sourceLinks = Array.from(document.querySelectorAll('a[href], img[src], iframe[src]')).map((el) => el.getAttribute('href') || el.getAttribute('src')).filter((value) => /^https?:\/\/(www\.)?flynnsarcades\.com/i.test(value));
        const missingIcons = Array.from(document.querySelectorAll('svg use')).filter((use) => !document.querySelector(use.getAttribute('href'))).map((use) => use.getAttribute('href'));
        const headerItems = Array.from(document.querySelector('.header-inner').children).filter((el) => el.getClientRects().length).map((el) => el.getBoundingClientRect());
        const headerOverlap = headerItems.some((a, i) => headerItems.slice(i + 1).some((b) => a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top));
        return { title: text(document.querySelector('h1')), h1Count: document.querySelectorAll('h1').length, tagline: text(document.querySelector('.game-tagline')), blocks, stores: Array.from(document.querySelectorAll('.game-store-links a')).map((a) => ({ url: a.getAttribute('href'), label: text(a), title: a.getAttribute('aria-label') })), screenshots: Array.from(document.querySelectorAll('.game-gallery img')).map((img) => img.getAttribute('src')), related: Array.from(document.querySelectorAll('.other-games .game-link')).map((a) => a.getAttribute('href')), genres: Array.from(document.querySelectorAll('.game-genres > span')).map(text), order: Array.from(document.querySelector('main').children).filter((el) => el.tagName !== 'DIALOG').map((el) => el.classList[0]), overflow, pageWidth: document.documentElement.scrollWidth, sourceLinks, missingIcons, headerOverlap, iframes: document.querySelectorAll('iframe').length, externalResources: performance.getEntriesByType('resource').filter((resource) => /^https?:/.test(resource.name)).map((resource) => resource.name), imageFits: Array.from(document.querySelectorAll('.game-video img,.game-gallery img,.game-detail-image')).map((img) => getComputedStyle(img).objectFit) };
      });
      assert(actual.title === game.title && actual.h1Count === 1, `${file}: title is not unique and correct.`);
      assert(actual.tagline === expected.tagline, `${file}: original tagline is missing.`);
      const expectedBlocks = expected.blocks.map((block) => block.type === 'heading' ? { ...block, text: block.text.replace(/:+$/, '') } : block);
      assert(JSON.stringify(actual.blocks) === JSON.stringify(expectedBlocks), `${file}: complete original descriptions/features are not preserved.`);
      assert(actual.stores.length === expected.stores.length && actual.stores.every((store, index) => store.url === expected.stores[index].url && store.title.includes(game.title)), `${file}: incorrect store product link.`);
      assert(JSON.stringify(actual.screenshots) === JSON.stringify(expected.gallery.map((image) => `assets/images/${image.image}`)), `${file}: game-specific gallery is missing.`);
      assert(await page.locator('.game-capture-slider').count() === 0 && await page.locator('.game-showcase [data-video-id]').getAttribute('data-video-id') === expected.hero_video.id, `${file}: a video must replace the hero screenshot carousel.`);
      assert(JSON.stringify(actual.genres) === JSON.stringify(expected.genres), `${file}: genre headings differ from the source.`);
      assert(JSON.stringify(actual.related) === JSON.stringify(expected.related.map((other) => `game-${other}.html`)) && !actual.related.includes(file), `${file}: incorrect related-game navigation.`);
      assert(JSON.stringify(actual.order) === JSON.stringify(['game-showcase', ...(expected.gallery.length ? ['game-gallery'] : []), 'game-purchase', 'game-content-band', 'section-inner']), `${file}: reference section order changed.`);
      assert(actual.overflow.length === 0 && actual.pageWidth <= width && !actual.headerOverlap, `${file} ${width}px: layout overflow/overlap ${JSON.stringify(actual.overflow)}.`);
      assert(actual.sourceLinks.length === 0 && actual.externalResources.length === 0 && actual.iframes === 0, `${file}: page depends on an external resource before any user action.`);
      assert(actual.missingIcons.length === 0, `${file}: missing icons ${actual.missingIcons}.`);
      assert(actual.imageFits.every((fit) => fit === 'contain'), `${file}: game artwork should not be cropped.`);
      assert(await page.locator('.site-nav [aria-current="page"]').textContent() === 'Games', `${file}: incorrect navigation state.`);
      if (samples.includes(id)) {
        await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'instant' }));
        await page.screenshot({ path: path.join(output, `${width}-${id}-viewport.png`) });
        if ([1440, 390].includes(width)) await page.screenshot({ path: path.join(output, `${width}-${id}-full.png`), fullPage: true });
      }
      const photo = page.locator('[data-gallery-item]').first();
      await photo.click();
      assert(await page.locator('[data-gallery-dialog]').evaluate((dialog) => dialog.open), `${file}: screenshot viewer failed.`);
      await page.locator('[data-gallery-image]').evaluate((image) => image.decode());
      await page.keyboard.press('ArrowRight');
      assert(await page.locator('[data-gallery-count]').textContent() === `2 / ${expected.gallery.length}`, `${file}: screenshot viewer navigation failed.`);
      await page.keyboard.press('Escape');
      assert(await photo.evaluate((el) => el === document.activeElement), `${file}: viewer did not restore keyboard focus.`);
      const videoLinks = await page.locator('[data-video-id]').evaluateAll((links) => links.map((link) => ({ id: link.dataset.videoId, href: link.href, target: link.target, start: Number(link.dataset.videoStart) })));
      const expectedVideos = [expected.hero_video, ...expected.trailers];
      assert(videoLinks.length === expectedVideos.length && videoLinks.every((link, index) => link.id === expectedVideos[index].id && link.href === expectedVideos[index].url && link.start === (expectedVideos[index].start || 0) && link.target === '_blank'), `${file}: video roles or fallback links are incorrect.`);
      const fileFallback = await page.evaluate(() => {
        let intercepted;
        document.body.addEventListener('click', (event) => { intercepted = event.defaultPrevented; event.preventDefault(); }, { once: true });
        document.querySelector('[data-video-id]').click();
        return { intercepted, iframes: document.querySelectorAll('iframe').length };
      });
      assert(fileFallback.intercepted === false && fileFallback.iframes === 0, `${file}: file previews must open YouTube directly, not a player without a Referer.`);
      checks.push({ game: id, width, screenshots: actual.screenshots.length, stores: actual.stores.length, blocks: actual.blocks.length, offlineVerified: true, screenshotViewerVerified: true, videoLinksVerified: true, fileVideoFallbackVerified: true });
    }
  }
  assert(errors.length === 0, errors.join('\n'));
  const report = { games: catalog.length, layouts: checks.length, checks, errors, output, sourceOrderVerified: true, completeDescriptionsVerified: true, allLocalAssetsDecoded: true, sourceIndependenceVerified: true, videoPlaybackRequiresInternet: true };
  fs.writeFileSync(path.join(output, 'verification.json'), JSON.stringify(report, null, 2));
  const matrix = ['# Revision de las 32 fichas', '', 'Informe generado por `tools/check-game-pages.cjs` tras superar las comprobaciones. Cada ficha se verifica en ordenador y movil; siete ejemplos se comprueban tambien en tres anchos adicionales.', '', '| Juego | Ficha local | Capturas disponibles | Tiendas | Resenas pertinentes |', '| --- | --- | ---: | ---: | ---: |'];
  for (const game of catalog) {
    const id = slug(game.title);
    const detail = details[id];
    matrix.push(`| ${game.title} | [Abrir](../flynns-arcade-neon/game-${id}.html) | ${detail.gallery.length} | ${detail.stores.length} | ${detail.reviews.length} |`);
  }
  matrix.push('', `${checks.length} distribuciones verificadas. Recursos locales decodificados, descripciones completas, capturas ampliables, navegacion y enlaces de video comprobados.`, '', 'Al abrir archivos directamente, los videos enlazan a YouTube para evitar el error 153. La prueba HTTP y la comprobacion de reproduccion real se ejecutan por separado con tools/check-video-players.cjs.');
  fs.writeFileSync(path.resolve(__dirname, '../docs/revision-fichas.md'), matrix.join('\n') + '\n');
  console.log(JSON.stringify({ games: report.games, layouts: report.layouts, errors, output }, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
