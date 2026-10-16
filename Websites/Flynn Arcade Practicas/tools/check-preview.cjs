const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const { createHash } = require('node:crypto');
const { pathToFileURL } = require('node:url');
const theme = path.resolve(__dirname, '../flynns-arcade-neon');
const catalog = JSON.parse(fs.readFileSync(path.join(theme, 'inc/catalog.json'), 'utf8'));
const structure = JSON.parse(fs.readFileSync(path.join(theme, 'inc/structure.json'), 'utf8'));
const aboutProjects = ['Eternum Ex', 'Lambs on the Road'].map((title) => catalog.find((game) => game.title === title));
const checksRoot = path.resolve(__dirname, '../checks');
const checks = path.join(checksRoot, 'preview-' + Date.now());
fs.mkdirSync(checks, { recursive: true });
const aboutReference = JSON.parse(fs.readFileSync(path.join(checksRoot, 'about-reference.json'), 'utf8'));
const assert = (condition, message) => { if (!condition) throw new Error(message); };
const url = (file) => pathToFileURL(path.join(theme, file)).href;
let browser;
(async () => {
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  page.setDefaultNavigationTimeout(15000);
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  await page.route(/^https?:\/\//, (route) => route.abort());
  const layouts = [];
  const backgroundPixels = [];
  for (const [name, width, height] of [['desktop', 1440, 1000], ['browser-comments', 1191, 668], ['compact-desktop', 851, 900], ['tablet', 768, 1024], ['stacked-tablet', 767, 900], ['mobile', 390, 844], ['small-mobile', 320, 812]]) {
    await page.setViewportSize({ width, height });
    for (const file of ['preview.html', 'games.html', 'indies.html', 'about.html', 'press.html', 'game-eternum-ex.html']) {
      await page.mouse.move(0, 0);
      await page.goto(url(file));
      await page.evaluate(async () => {
        await document.fonts.ready;
        const images = Array.from(document.images).filter((image) => image.getAttribute('src'));
        images.forEach((image) => { image.loading = 'eager'; });
        await Promise.all(images.map((image) => image.decode()));
        const backdrop = getComputedStyle(document.body, '::before').backgroundImage.match(/url\("?([^"\)]+)"?\)/);
        if (!backdrop) throw new Error('Shared arcade background is missing.');
        const image = new Image();
        image.src = backdrop[1];
        await image.decode();
      });
      const result = await page.evaluate(({ name, file, width }) => {
        const overflow = Array.from(document.querySelectorAll('body *')).filter((el) => {
          if (!(el instanceof HTMLElement) || !el.getClientRects().length || el.classList.contains('screen-reader-text') || el.matches('.publisher-logo img')) return false;
          const r = el.getBoundingClientRect();
          return r.right > width + 1 || r.left < -1;
        }).map((el) => el.className || el.tagName);
        const dependencies = performance.getEntriesByType('resource').filter((item) => /^https?:/.test(item.name)).map((item) => item.name);
        const platformColors = Array.from(document.querySelectorAll('.platform-icon svg path')).map((el) => getComputedStyle(el).fill);
        const footerCircles = Array.from(document.querySelectorAll('.social-band .social-links a')).map((el) => el.getBoundingClientRect().width);
        const headerCircles = Array.from(document.querySelectorAll('.header-socials .social-links a')).filter((el) => el.getClientRects().length).map((el) => el.getBoundingClientRect().width);
        const headerItems = Array.from(document.querySelector('.header-inner').children).filter((el) => el.getClientRects().length).map((el) => el.getBoundingClientRect());
        const headerOverlap = headerItems.some((a, i) => headerItems.slice(i + 1).some((b) => a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top));
        const backdrop = getComputedStyle(document.body, '::before');
        return { name, file, width, pageWidth: document.documentElement.scrollWidth, overflow, dependencies, platformColors: [...new Set(platformColors)], footerCircles, headerCircles, headerOverlap, backdrop: { image: backdrop.backgroundImage, position: backdrop.position, pointerEvents: backdrop.pointerEvents, isolation: getComputedStyle(document.body).isolation } };
      }, { name, file, width });
      layouts.push(result);
      assert(result.pageWidth <= width && result.overflow.length === 0, JSON.stringify(result));
      assert(result.dependencies.length === 0, 'Page loaded an external resource.');
      assert(result.backdrop.image.includes(width <= 767 ? 'arcade-city-mobile.webp' : 'arcade-city.webp'), 'The correct local desktop or mobile background should load on every page.');
      assert(result.backdrop.position === 'fixed' && result.backdrop.pointerEvents === 'none' && result.backdrop.isolation === 'isolate', 'The background should be fixed, behind the content and non-interactive.');
      assert(result.platformColors.every((color) => color === 'rgb(245, 28, 165)'), 'Platform logos should all use Switch pink.');
      assert(result.footerCircles.length === 9 && result.footerCircles.every((size) => size === 56), 'Footer social circles should be 56px.');
      assert(result.headerCircles.every((size) => size === 44), 'Header social circles should be 44px.');
      assert(!result.headerOverlap, `Header items overlap at ${width}px.`);
      const logos = await page.locator('.publisher-logo').evaluateAll((frames) => frames.map((frame) => {
        const image = frame.querySelector('img');
        const crop = frame.getBoundingClientRect();
        const source = image.getBoundingClientRect();
        const scale = source.width / image.naturalWidth;
        return { path: image.getAttribute('src'), clipped: getComputedStyle(frame).overflow === 'hidden', native: [image.naturalWidth, image.naturalHeight], undistorted: Math.abs(source.width / source.height - 1.6) < .001, left: (crop.left - source.left) / scale, top: (crop.top - source.top) / scale, width: crop.width / scale, height: crop.height / scale };
      }));
      assert(logos.length === (file === 'about.html' ? 2 : 1) && logos.every((logo) => logo.path === `assets/images/${structure.logo}` && logo.clipped && logo.undistorted && Math.abs(logo.left - 500) < .1 && Math.abs(logo.top - 330) < .1 && Math.abs(logo.width - 650) < .1 && Math.abs(logo.height - 318) < .1), 'All publisher logos should use the same undistorted, non-destructive crop of the supplied PNG.');
      assert(await page.locator('.brand').evaluate((brand) => brand.offsetWidth) === (width <= 600 ? 150 : 170), 'Header logo size should stay unchanged.');
      assert(await page.locator('a.contact-email').getAttribute('href') === 'mailto:press@flynnsarcades.com', 'Contact should open the configured mail application.');
      assert(await page.locator('.social-band').evaluate((band) => getComputedStyle(band).backgroundColor) === 'rgba(0, 0, 0, 0.65)', 'The social band should have a translucent black background.');
      if (file === 'preview.html') {
        const banner = await page.locator('.hero-image').boundingBox();
        const featured = await page.locator('.featured-game').boundingBox();
        assert(Math.abs(banner.x) < 1 && Math.abs(banner.width - width) < 1 && Math.abs(banner.width / banner.height - 3) < .01, 'The panoramic banner should fill the viewport width and retain its complete 3:1 composition without cropping.');
        assert(await page.locator('.hero-image').evaluate((image) => image.naturalWidth === 2172 && image.naturalHeight === 724 && getComputedStyle(image).objectFit === 'contain'), 'The complete panoramic artwork should be decoded and displayed without distortion.');
        assert(await page.locator('.hero h1, .hero p, .hero a').count() === 0, 'Game text and controls should be outside the child artwork.');
        assert(featured.y >= banner.y + banner.height, 'The featured game should follow the image in a separate section.');
        assert(await page.locator('.featured-game-image').getAttribute('src') === 'assets/images/games/the-last-cat-in-the-universe.jpg', 'The featured game should use its own cover artwork.');
      }
      if (file === 'press.html') {
        const buttons = await page.locator('.presskit-download').evaluateAll((links) => links.map((link) => {
          const style = getComputedStyle(link);
          return { download: link.hasAttribute('download'), local: link.getAttribute('href').startsWith('downloads/presskit-'), height: link.getBoundingClientRect().height, border: style.borderTopWidth, background: style.backgroundImage, label: link.getAttribute('aria-label') };
        }));
        assert(buttons.length === 4 && buttons.every((button) => button.download && button.local && button.height >= 44 && button.border === '1px' && button.background.includes('linear-gradient') && button.label.startsWith('Download ')), 'Each presskit should have a clear, accessible local download button.');
        if (['desktop', 'browser-comments', 'mobile'].includes(name)) {
          await page.locator('.presskits').screenshot({ path: path.join(checks, `${name}-presskit-buttons.png`) });
          await page.locator('.social-band').screenshot({ path: path.join(checks, `${name}-social-overlay.png`) });
        }
      }
      if (file === 'about.html') {
        const about = await page.evaluate(() => {
          const text = (el) => el.textContent.trim().replace(/\s+/g, ' ');
          return {
            sections: Array.from(document.querySelectorAll('.about-story section')).map((section) => ({ title: text(section.querySelector('h2')), paragraphs: Array.from(section.querySelectorAll('p')).map(text) })),
            gallery: Array.from(document.querySelectorAll('[data-gallery-item] img')).map((img) => img.getAttribute('src')),
            projects: Array.from(document.querySelectorAll('.about-projects .game-card')).map((card) => ({ title: text(card.querySelector('h3')), image: card.querySelector('img').getAttribute('src'), tags: Array.from(card.querySelectorAll('.tag')).map(text), platforms: Array.from(card.querySelectorAll('.platform-icon')).map((icon) => icon.title) })),
            projectColumns: getComputedStyle(document.querySelector('.projects-grid')).gridTemplateColumns.split(' ').length,
            projectSizes: Array.from(document.querySelectorAll('.about-projects .game-card')).map((card) => ({ width: card.getBoundingClientRect().width, height: card.getBoundingClientRect().height })),
            galleryColumns: getComputedStyle(document.querySelector('.about-gallery')).gridTemplateColumns.split(' ').length,
            storyColumns: getComputedStyle(document.querySelector('.about-story')).gridTemplateColumns.split(' ').length,
            photoTint: getComputedStyle(document.querySelector('[data-gallery-item]'), '::after').backgroundColor,
            photoFilters: Array.from(document.querySelectorAll('[data-gallery-item] img')).map((img) => getComputedStyle(img).filter),
            storyHeadingStyles: Array.from(document.querySelectorAll('.about-story h2')).map((heading) => {
              const style = getComputedStyle(heading);
              return { color: style.color, border: style.borderTopColor, radius: style.borderRadius };
            }),
            projectArtwork: Array.from(document.querySelectorAll('.about-projects .game-card')).map((tile) => {
              const image = tile.querySelector('img');
              const artwork = image.getBoundingClientRect();
              const frame = tile.getBoundingClientRect();
              const heading = tile.querySelector('h3').getBoundingClientRect();
              return { fit: getComputedStyle(image).objectFit, inside: artwork.left >= frame.left && artwork.right <= frame.right && artwork.top >= frame.top && artwork.bottom <= frame.bottom, separateCaption: heading.top >= artwork.bottom };
            }),
            order: Array.from(document.querySelector('main').children).filter((el) => !el.classList.contains('screen-reader-text') && el.tagName !== 'DIALOG').map((el) => el.classList[0]),
          };
        });
        assert(JSON.stringify(about.sections) === JSON.stringify(aboutReference.sections), 'About must retain the complete original headings, paragraphs and wording.');
        assert(JSON.stringify(about.gallery) === JSON.stringify(aboutReference.gallery.map((photo) => `assets/images/${photo.image}`)), 'About gallery photos or order differ from the source.');
        assert(JSON.stringify(about.projects) === JSON.stringify(aboutProjects.map((game) => ({ title: game.title, image: `assets/images/${game.image}`, tags: game.tags, platforms: game.platforms }))), 'Project cards must show the requested covers, titles, genres and platforms.');
        assert(about.projectColumns === (width > 767 ? 2 : 1), 'Project cards must use two desktop columns and one mobile column.');
        assert(about.projectSizes.every((size) => size.width <= 400 && size.height <= 420), 'Project cards should remain medium-sized, including on wide screens.');
        assert(about.galleryColumns === (width > 1024 ? 6 : width > 767 ? 2 : 1), 'About photo columns differ from the original responsive structure.');
        assert(about.storyColumns === (width > 767 ? 3 : 1), 'About history should use three desktop columns and one mobile column.');
        assert(about.photoTint === 'rgb(255, 133, 46)', 'Gallery tint should use neon orange to match the supplied logo.');
        assert(about.photoFilters.every((filter) => filter === 'grayscale(1)'), 'Unselected photos should have the neon tint treatment.');
        assert(about.projectArtwork.every((image) => image.fit === 'contain' && image.inside), 'Project illustrations should fit completely inside their frames.');
        assert(about.projectArtwork.every((image) => image.separateCaption), 'Project titles and details must be below the covers.');
        assert(about.storyHeadingStyles.every((style) => style.color === 'rgb(245, 28, 165)' && style.border === 'rgb(245, 28, 165)' && style.radius === '0px'), 'Story headings must have pink text and borders with square corners.');
        assert(JSON.stringify(about.order) === JSON.stringify(['about-brand', 'about-gallery', 'about-story', 'about-projects']), 'About section order differs from the source.');
      }
      await page.screenshot({ path: path.join(checks, `${name}-${file.replace('.html', '')}.png`), fullPage: true });
      if (['preview.html', 'about.html'].includes(file) && ['desktop', 'browser-comments', 'mobile', 'small-mobile'].includes(name)) {
        await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'instant' }));
        await page.screenshot({ path: path.join(checks, `${name}-${file.replace('.html', '')}-viewport.png`) });
      }
      if (name === 'desktop' && file === 'indies.html') await page.locator('.studio-row').first().screenshot({ path: path.join(checks, 'indies-first-row.png') });
      if (name === 'desktop' && file === 'press.html') await page.locator('.press-row').first().screenshot({ path: path.join(checks, 'press-first-row.png') });
      if (file === 'about.html') await page.locator('.about-projects').screenshot({ path: path.join(checks, `${name}-project-sizing.png`) });
      if (name === 'browser-comments' && file === 'about.html') await page.locator('.projects-grid').screenshot({ path: path.join(checks, 'about-project-cards-reference.png') });
      if (file === 'about.html' && ['desktop', 'mobile'].includes(name)) {
        await page.evaluate(() => window.scrollTo({ top: 0, behavior: 'instant' }));
        const visible = await page.screenshot({ path: path.join(checks, `${name}-background-visible.png`) });
        const override = await page.addStyleTag({ content: 'body::before { visibility: hidden !important; }' });
        const hidden = await page.screenshot();
        await override.evaluate((el) => el.remove());
        const a = await sharp(visible).removeAlpha().raw().toBuffer();
        const b = await sharp(hidden).removeAlpha().raw().toBuffer();
        let changed = 0;
        for (let pixel = 0; pixel < a.length; pixel += 3) {
          if (Math.max(Math.abs(a[pixel] - b[pixel]), Math.abs(a[pixel + 1] - b[pixel + 1]), Math.abs(a[pixel + 2] - b[pixel + 2])) > 8) changed++;
        }
        assert(changed > 1000, 'The background exists but is not visibly rendered.');
        backgroundPixels.push({ name, changedPixels: changed });
        await page.locator('.about-story').scrollIntoViewIfNeeded();
        await page.screenshot({ path: path.join(checks, `${name}-background-scrolled.png`) });
      }
    }
  }
  for (const width of [1920, 2560]) {
    await page.setViewportSize({ width, height: 1000 });
    await page.goto(url('preview.html'));
    await page.evaluate(async () => { await document.fonts.ready; await document.querySelector('.hero-image').decode(); });
    const bounds = await page.locator('.hero-image').boundingBox();
    assert(Math.abs(bounds.x) < 1 && Math.abs(bounds.width - width) < 1 && Math.abs(bounds.width / bounds.height - 3) < .01, 'Wide-screen banner should remain full-width and uncropped.');
    assert(await page.evaluate(() => document.documentElement.scrollWidth) === width, 'Wide-screen banner causes horizontal overflow.');
    const { data, info } = await sharp(await page.locator('.hero-image').screenshot()).removeAlpha().raw().toBuffer({ resolveWithObject: true });
    const thirds = [0, 0, 0];
    for (let y = 0; y < info.height; y++) {
      for (let x = 0; x < info.width; x++) {
        const offset = (y * info.width + x) * info.channels;
        const rgb = [data[offset], data[offset + 1], data[offset + 2]];
        if (Math.max(...rgb) > 40 && Math.max(...rgb) - Math.min(...rgb) > 20) thirds[Math.min(2, Math.floor(x * 3 / info.width))]++;
      }
    }
    assert(thirds.every((pixels) => pixels > info.width * info.height / 3 * .05), 'The full-width banner should show artwork at both edges and the center, not empty side panels.');
    await page.screenshot({ path: path.join(checks, `wide-banner-${width}.png`) });
  }
  await page.setViewportSize({ width: 1440, height: 1000 });
  await page.goto(url('preview.html'));
  assert(await page.locator('.hero-image').getAttribute('src') === 'assets/images/hero-arcade-banner.webp', 'Home should use the locally saved panoramic banner.');
  assert(createHash('sha256').update(fs.readFileSync(path.join(theme, 'assets/images/hero-arcade-kid.png'))).digest('hex') === 'a1b9d733a826fb1ed71ad4173fb8f061df912dd4dfc31878661ddaa5a814948b', 'The supplied child image must remain unaltered.');
  assert(createHash('sha256').update(fs.readFileSync(path.join(theme, 'assets/images', structure.logo))).digest('hex') === '18f4ad2cab0379615bf0abbfccf1993c3c6a22fc75ef55e54f075c99dee67b2b', 'The supplied publisher logo must be preserved exactly.');
  await page.locator('.featured-game .arcade-button').click({ noWaitAfter: true });
  await page.waitForURL(url('game-the-last-cat-in-the-universe.html'));
  await page.locator('.brand').click({ noWaitAfter: true });
  await page.waitForURL(url('preview.html'));
  assert(await page.locator('.site-nav [aria-current="page"]').textContent() === 'Inicio', 'Home navigation is not distinct.');
  await page.getByRole('button', { name: 'Next highlight' }).click();
  assert(await page.locator('[data-promotion-slide]:visible h2').textContent() === '100% pure graphic adventure', 'Highlight carousel failed.');
  await page.getByRole('button', { name: 'Previous highlight' }).click();
  await page.locator('.site-nav').getByRole('link', { name: 'Games', exact: true }).click({ noWaitAfter: true });
  await page.waitForURL(url('games.html'));
  assert(await page.locator('.site-nav [aria-current="page"]').textContent() === 'Games', 'Games navigation is not active.');
  assert(await page.locator('main .hero, main .promotions').count() === 0, 'Games should only show the catalog, not home promotions.');
  assert(await page.getByRole('heading', { level: 1 }).textContent() === 'Games', 'Games page heading is missing.');
  assert(await page.locator('.platform-switch, .platform-xbox, .platform-playstation, .platform-pc').count() > 0, 'Platform logos are missing.');
  assert(await page.locator('[data-game-card]:visible').count() === 6, 'Initial catalog should show six games.');
  assert(JSON.stringify(await page.locator('[data-game-card]:visible h3').allTextContents()) === JSON.stringify(catalog.slice(0, 6).map((game) => game.title)), 'Catalog order differs from the source.');
  await page.getByRole('button', { name: 'Load more', exact: false }).click();
  assert(await page.locator('[data-game-card]:visible').count() === 12, 'Load more did not reveal the next six games.');
  while (await page.locator('[data-load-more]').isVisible()) await page.locator('[data-load-more]').click();
  assert(await page.locator('[data-game-card]:visible').count() === catalog.length, 'Not all games are reachable.');
  await page.getByRole('button', { name: 'Point & Click', exact: true }).click();
  assert(await page.locator('[data-game-card]:visible').count() === 1, 'Point & Click filter failed.');
  await page.getByRole('button', { name: 'All games', exact: true }).click();
  await page.locator('[data-game-search]').fill('Murtop');
  assert(await page.locator('[data-game-card]:visible').count() === 1, 'Search failed.');
  await page.locator('[data-game-search]').fill('no-game-matches-this');
  assert(await page.locator('[data-empty-state]').isVisible(), 'Empty state is missing.');
  await page.locator('[data-game-search]').fill('');
  await page.locator('[data-game-card]:visible').first().getByRole('link').click({ noWaitAfter: true });
  await page.locator('.game-detail-copy').waitFor();
  assert(await page.getByRole('heading', { level: 1 }).textContent() === 'What Comes After', 'Game page did not open.');
  assert(await page.locator('.site-nav [aria-current="page"]').textContent() === 'Games', 'Game detail should belong to Games.');
  await page.getByRole('link', { name: 'All games', exact: true }).click({ noWaitAfter: true });
  await page.waitForURL(url('games.html') + '#games');
  assert(await page.locator('main .hero').count() === 0, 'All games should return to the dedicated catalog.');
  await page.locator('.brand').click({ noWaitAfter: true });
  await page.waitForURL(url('preview.html'));
  assert(await page.locator('.hero').count() === 1, 'The brand should return to home.');
  await page.locator('.site-nav').getByRole('link', { name: 'Indies', exact: true }).click({ noWaitAfter: true });
  await page.locator('.studio-row').first().waitFor();
  assert(await page.locator('.studio-row').count() === 16, 'Indies is missing creators.');
  await page.locator('.site-nav').getByRole('link', { name: 'About', exact: true }).click({ noWaitAfter: true });
  await page.locator('.about-story').waitFor();
  assert(await page.locator('.about-story section').count() === 3, 'About sections are missing.');
  assert(await page.locator('.about-projects .game-card').count() === 2, 'About projects are missing.');
  const photo = page.locator('[data-gallery-item]').first();
  await page.locator('.about-gallery img').evaluateAll(async (images) => {
    images.forEach((image) => { image.loading = 'eager'; });
    await Promise.all(images.map((image) => image.decode()));
  });
  await page.locator('.about-gallery').screenshot({ path: path.join(checks, 'about-gallery-orange.png') });
  await photo.hover();
  await page.waitForFunction(() => getComputedStyle(document.querySelector('[data-gallery-item]'), '::after').opacity === '0' && getComputedStyle(document.querySelector('[data-gallery-item] img')).filter === 'none');
  await page.locator('.about-gallery').screenshot({ path: path.join(checks, 'about-gallery-hover.png') });
  await page.locator('[data-gallery-item]').nth(1).focus();
  await page.mouse.move(0, 0);
  await page.waitForFunction(() => {
    const photos = document.querySelectorAll('[data-gallery-item]');
    return getComputedStyle(photos[0], '::after').opacity === '0.8' && getComputedStyle(photos[1], '::after').opacity === '0' && getComputedStyle(photos[1].querySelector('img')).filter === 'none';
  });
  await page.locator('.about-gallery').screenshot({ path: path.join(checks, 'about-gallery-focus.png') });
  await page.locator('[data-gallery-item]').nth(1).evaluate((el) => el.blur());
  const firstProject = page.locator('.about-projects .game-link').first();
  const originalBounds = await firstProject.boundingBox();
  await firstProject.hover();
  await page.waitForFunction(() => getComputedStyle(document.querySelector('.about-projects .game-card')).borderTopColor === 'rgb(0, 222, 238)');
  const hoveredBounds = await firstProject.boundingBox();
  assert(hoveredBounds.width === originalBounds.width && hoveredBounds.height === originalBounds.height, 'Project card dimensions must remain stable on hover.');
  assert(await page.locator('.about-projects .game-body h3').first().isVisible(), 'Project title should remain visible.');
  await page.screenshot({ path: path.join(checks, 'about-project-hover.png'), fullPage: true });
  await page.locator('[data-gallery-item]').first().click();
  assert(await page.locator('[data-gallery-dialog]').evaluate((dialog) => dialog.open), 'Photo viewer did not open.');
  assert(await page.locator('[data-gallery-count]').textContent() === '1 / 6', 'Photo counter is incorrect.');
  await page.locator('[data-gallery-image]').evaluate((img) => img.decode());
  assert(await page.locator('[data-gallery-image]').evaluate((img) => getComputedStyle(img).filter) === 'none', 'Expanded photos must show their original colors.');
  await page.getByRole('button', { name: 'Previous photo', exact: true }).click();
  assert(await page.locator('[data-gallery-count]').textContent() === '6 / 6', 'Photo viewer should wrap to the last photo.');
  await page.keyboard.press('ArrowRight');
  assert(await page.locator('[data-gallery-count]').textContent() === '1 / 6', 'Photo keyboard navigation failed.');
  await page.getByRole('button', { name: 'Next photo', exact: true }).click();
  assert(await page.locator('[data-gallery-image]').getAttribute('src') === url('assets/images/about/gallery-02.jpg'), 'Photo viewer displays the wrong local photo.');
  await page.locator('[data-gallery-image]').evaluate((img) => img.decode());
  await page.screenshot({ path: path.join(checks, 'about-photo-viewer-desktop.png') });
  await page.keyboard.press('Escape');
  assert(!await page.locator('[data-gallery-dialog]').evaluate((dialog) => dialog.open), 'Escape should close the photo viewer.');
  assert(await page.locator('[data-gallery-item]').first().evaluate((el) => el === document.activeElement), 'Photo viewer should restore keyboard focus.');
  await page.locator('[data-gallery-item]').last().click();
  await page.getByRole('button', { name: 'Close photo', exact: true }).click();
  await firstProject.click({ noWaitAfter: true });
  await page.waitForURL(url('game-eternum-ex.html'));
  await page.locator('.site-nav').getByRole('link', { name: 'About', exact: true }).click({ noWaitAfter: true });
  await page.locator('.about-projects .game-link').last().click({ noWaitAfter: true });
  await page.waitForURL(url('game-lambs-on-the-road.html'));
  await page.locator('.site-nav').getByRole('link', { name: 'Press', exact: true }).click({ noWaitAfter: true });
  await page.locator('.press-row').first().waitFor();
  assert(await page.locator('.press-row').count() === 15, 'Press is incomplete.');
  assert(await page.locator('.presskit').count() === 4, 'Presskits are missing.');
  for (const link of await page.locator('.presskit-download').all()) {
    const expected = path.basename(await link.getAttribute('href'));
    const downloadReady = page.waitForEvent('download');
    await link.click();
    const download = await downloadReady;
    assert(download.suggestedFilename() === expected && await download.failure() === null, 'A presskit download failed.');
  }
  const links = await page.locator('a').evaluateAll((items) => items.map((item) => item.getAttribute('href')));
  assert(!links.some((href) => /^https?:\/\/(www\.)?flynnsarcades\.com/i.test(href || '')), 'A link points back to the source website.');
  const pages = fs.readdirSync(theme).filter((file) => file.endsWith('.html') && file !== 'index.html');
  for (const name of pages) {
    const html = fs.readFileSync(path.join(theme, name), 'utf8');
    assert(!/href="https?:\/\/(www\.)?flynnsarcades\.com/i.test(html), `Source navigation link in ${name}.`);
    for (const match of html.matchAll(/(?:src|href)="([^"#?]+)(?:[?#][^"]*)?"/g)) {
      const target = match[1].replaceAll('&amp;', '&');
      if (/^(https?:|mailto:|data:)/.test(target)) continue;
      assert(fs.existsSync(path.join(theme, target)), `Missing local resource in ${name}: ${target}`);
    }
  }
  for (const file of ['arcade-city.webp', 'arcade-city-mobile.webp']) assert(fs.existsSync(path.join(theme, 'assets/images/backgrounds', file)), `Missing background asset: ${file}`);
  await page.locator('#newsletter-email').fill('preview@example.test');
  await page.getByRole('button', { name: 'Subscribe', exact: false }).click();
  assert(await page.locator('[data-newsletter-note]').isVisible(), 'Newsletter status is missing.');
  for (const width of [390, 320]) {
    await page.setViewportSize({ width, height: 844 });
    await page.goto(url('about.html'));
    await page.locator('[data-gallery-item]').nth(1).click();
    await page.locator('[data-gallery-image]').evaluate((img) => img.decode());
    const viewerBounds = await page.locator('[data-gallery-dialog]').boundingBox();
    assert(viewerBounds.x >= 0 && viewerBounds.x + viewerBounds.width <= width, 'Photo viewer exceeds mobile width.');
    await page.screenshot({ path: path.join(checks, `about-photo-viewer-${width}.png`) });
    await page.getByRole('button', { name: 'Close photo', exact: true }).click();
    await page.goto(url('preview.html'));
    await page.locator('.nav-toggle').click();
    assert(await page.locator('.nav-toggle').getAttribute('aria-expanded') === 'true', 'Mobile menu did not open.');
    assert(await page.locator('.site-nav a:visible').count() === 5, 'All five navigation links should be visible.');
    const bounds = await page.locator('.site-nav a').evaluateAll((items) => items.map((item) => ({ left: item.getBoundingClientRect().left, right: item.getBoundingClientRect().right })));
    assert(bounds.every((item) => item.left >= 0 && item.right <= width), 'Mobile navigation exceeds the viewport.');
    await page.screenshot({ path: path.join(checks, `mobile-menu-${width}.png`) });
    await page.locator('.site-nav').getByRole('link', { name: 'Games', exact: true }).click({ noWaitAfter: true });
    await page.waitForURL(url('games.html'));
    assert(await page.locator('main .hero, main .promotions').count() === 0, 'Mobile Games should only show the catalog.');
    await page.locator('.nav-toggle').click();
    await page.keyboard.press('Escape');
    assert(await page.locator('.nav-toggle').getAttribute('aria-expanded') === 'false', 'Escape should close navigation.');
    await page.locator('.nav-toggle').click();
    await page.locator('.site-nav').getByRole('link', { name: 'Inicio', exact: true }).click({ noWaitAfter: true });
    await page.waitForURL(url('preview.html'));
    assert(await page.locator('.hero').count() === 1, 'Mobile Inicio should return to home.');
  }
  assert(errors.length === 0, errors.join('\n'));
  const report = { layouts, errors, checkedPages: pages.length, artifactsDirectory: checks, backgroundPixels, catalog: catalog.length, studios: 16, press: 15, presskits: 4, suppliedPublisherLogoVerified: true, nonDestructiveLogoCropVerified: true, presskitDownloadsVerified: true, translucentSocialBandVerified: true, fullWidthPanoramicBannerVerified: true, uncroppedBannerVerified: true, wideBannerPixelsVerified: true, originalChildImagePreserved: true, separateFeaturedGameVerified: true, about: { photos: 6, originalTextVerified: true, projects: 2, viewerVerified: true, orangeGalleryHoverAndFocusVerified: true, pinkSquareHeadingsVerified: true, referenceProjectCardsVerified: true, mediumProjectSizeVerified: true, completeArtworkFitVerified: true, captionsDoNotOverlap: true }, result: 'Full-width panoramic banner without cropping, desktop/mobile/ultrawide artwork pixels, original image preservation, shared content, download buttons and offline navigation passed.' };
  fs.writeFileSync(path.join(checks, 'verification.json'), JSON.stringify(report, null, 2));
  fs.writeFileSync(path.join(checksRoot, 'verification.json'), JSON.stringify(report, null, 2));
  console.log(JSON.stringify({ result: report.result, layouts: layouts.length, checkedPages: pages.length, backgroundPixels, artifactsDirectory: checks, errors }, null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
