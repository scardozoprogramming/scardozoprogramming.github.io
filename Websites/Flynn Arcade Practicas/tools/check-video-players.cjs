const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const base = new URL(process.argv[2] || 'http://127.0.0.1:4173/');
const output = path.resolve(__dirname, '../checks/video-players-' + Date.now());
const details = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../flynns-arcade-neon/inc/game-details.json'), 'utf8'));
const sources = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../checks/game-reference/details-extracted.json'), 'utf8'));
const samples = ['what-comes-after', 'donut-dodo', 'eternum-ex', 'galacticon', 'papertris'];
const assert = (value, message) => { if (!value) throw new Error(message); };
let browser;

(async () => {
  fs.mkdirSync(output, { recursive: true });
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const context = await browser.newContext();
  const requests = [];
  await context.route('**/*', async (route) => {
    const request = route.request();
    if (new URL(request.url()).origin === base.origin) return route.continue();
    requests.push({ url: request.url(), headers: await request.allHeaders() });
    return route.abort();
  });
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const checks = [];
  for (const width of [1440, 390, 1191, 768, 320]) {
    for (const [slug, detail] of Object.entries(details)) {
    if (![1440, 390].includes(width) && !samples.includes(slug)) continue;
    await page.setViewportSize({ width, height: 900 });
    const response = await page.goto(new URL(`game-${slug}.html`, base).href);
    assert(response.ok(), 'Preview server did not serve the page.');
    await page.evaluate(() => document.fonts.ready);
    assert(await page.locator('.game-capture-slider').count() === 0, 'The old hero carousel remains.');
    assert(await page.locator('.game-gallery [data-gallery-item]').count() === 5, 'The separate screenshot gallery must remain.');
    assert(await page.locator('iframe').count() === 0 && requests.length === 0, 'Video services loaded before user activation.');
    for (const [selector, expected] of [['#game-video', detail.hero_video], ['#trailer', detail.trailers[0]]]) {
      const video = page.locator(selector);
      const before = await video.boundingBox();
      await video.locator('[data-video-id]').click();
      const frame = video.locator('iframe');
      const src = new URL(await frame.getAttribute('src'));
      assert(src.origin === 'https://www.youtube-nocookie.com' && src.pathname === `/embed/${expected.id}`, `${selector}: incorrect video.`);
      assert(src.searchParams.get('origin') === base.origin && Number(src.searchParams.get('start')) === (expected.start || 0), `${selector}: incorrect origin or start time.`);
      assert(await frame.getAttribute('referrerpolicy') === 'strict-origin-when-cross-origin', 'The player suppresses its Referer.');
      const after = await video.boundingBox();
      assert(before.width === after.width && before.height === after.height, 'Player activation shifts the layout.');
      const frameSize = await frame.boundingBox();
      assert(frameSize.width >= 200 && frameSize.height >= 200, 'Player is smaller than YouTube minimum size.');
      assert(await video.locator('.game-video-external').isVisible(), 'The direct video fallback disappears after activation.');
      const request = requests.find((entry) => new URL(entry.url).pathname === src.pathname);
      assert(request?.headers.referer === base.origin + '/', 'The browser did not send its real HTTP Referer.');
    }
    assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), `${width}px: horizontal overflow.`);
    if (samples.includes(slug)) await page.screenshot({ path: path.join(output, `${width}-${slug}-players.png`), fullPage: true });
    checks.push({ game: slug, width, players: 2, expectedVideosVerified: true, realRefererVerified: true, stableLayout: true });
    requests.length = 0;
    }
  }
  const metadata = [];
  const seen = new Set();
  for (const [slug, detail] of Object.entries(details)) {
    const source = sources.find((game) => game.slug === slug);
    for (const video of [detail.hero_video, ...detail.trailers]) {
      assert(source.videos.some((item) => item.id === video.id && (item.start || 0) === (video.start || 0)), `${slug}: video or start time is not from its original page.`);
      if (seen.has(video.id)) continue;
      seen.add(video.id);
      const response = await page.request.get('https://www.youtube.com/oembed?format=json&url=' + encodeURIComponent(video.url));
      assert(response.ok(), `${slug}: YouTube cannot identify video ${video.id}.`);
      const data = await response.json();
      metadata.push({ id: video.id, title: data.title, channel: data.author_name, status: response.status() });
    }
  }
  assert(details['eternum-ex'].hero_video.id !== details['eternum-ex'].trailers[0].id, 'Eternum Ex must keep distinct gameplay and trailer videos.');
  assert(details.papertris.hero_video.id === 'fRrG1LPihvg' && details.papertris.trailers[0].id === 'fRrG1LPihvg', 'Papertris must not show the copied Legend of Gwen trailer.');
  await context.close();

  // Playback is a separate online check, not inferred from successful iframe creation.
  const livePage = await browser.newPage({ viewport: { width: 1191, height: 900 } });
  const playback = [];
  for (const [slug, selector] of [['what-comes-after', '#game-video'], ['what-comes-after', '#trailer'], ['donut-dodo', '#game-video'], ['eternum-ex', '#trailer'], ['papertris', '#game-video'], ['galacticon', '#trailer']]) {
    await livePage.goto(new URL(`game-${slug}.html`, base).href);
    await livePage.locator(`${selector} [data-video-id]`).click();
    let result = { playing: false, error: '', time: 0 };
    const deadline = Date.now() + 20000;
    while (Date.now() < deadline) {
      const frame = livePage.frames().find((item) => item.url().includes('youtube-nocookie.com/embed/'));
      if (frame) result = await frame.evaluate(() => {
        const video = document.querySelector('video');
        const error = document.querySelector('.ytp-error-content-wrap');
        return { playing: Boolean(video && !video.paused && video.currentTime > 0), time: video?.currentTime || 0, error: error?.innerText?.trim() || '', title: document.title };
      }).catch(() => result);
      if (result.playing || result.error) break;
      await livePage.waitForTimeout(1000);
    }
    await livePage.locator(selector).screenshot({ path: path.join(output, `${slug}-${selector.slice(1)}-online.png`) });
    const entry = { game: slug, selector, ...result };
    if (!result.playing) {
      const direct = await browser.newPage();
      const href = await livePage.locator(`${selector} .game-video-external`).getAttribute('href');
      await direct.goto(href, { waitUntil: 'domcontentloaded' });
      await direct.waitForTimeout(3000);
      const rejectCookies = direct.getByRole('button', { name: /^(Rechazar todo|Reject all)$/i }).first();
      if (await rejectCookies.isVisible()) await rejectCookies.click();
      const play = direct.locator('.ytp-large-play-button');
      if (await play.isVisible()) await play.click({ timeout: 5000 }).catch(() => {});
      await direct.waitForTimeout(5000);
      entry.directYouTube = await direct.evaluate(() => {
        const video = document.querySelector('video');
        return { title: document.title, status: window.ytInitialPlayerResponse?.playabilityStatus?.status, time: video?.currentTime || 0, playing: Boolean(video && !video.paused && video.currentTime > 0) };
      });
      await direct.screenshot({ path: path.join(output, `${slug}-${selector.slice(1)}-direct-youtube.png`) });
      await direct.close();
    }
    playback.push(entry);
  }
  assert(errors.length === 0, errors.join('\n'));
  fs.writeFileSync(path.join(output, 'verification.json'), JSON.stringify({ base: base.href, checks, metadata, playback, errors }, null, 2));
  console.log(JSON.stringify({ output, games: Object.keys(details).length, layouts: checks.length, uniqueVideos: metadata.length, playback, errors }, null, 2));
  assert(playback.every((item) => !/153/.test(item.error)), 'Live playback still reports Error 153.');
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
