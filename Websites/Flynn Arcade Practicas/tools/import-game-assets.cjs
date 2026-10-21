const { request } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const sharp = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/sharp');
const fs = require('node:fs');
const path = require('node:path');
const theme = path.resolve(__dirname, '../flynns-arcade-neon');
const references = path.resolve(__dirname, '../checks/game-reference');
const details = JSON.parse(fs.readFileSync(path.join(references, 'details-extracted.json'), 'utf8'));
const manifest = JSON.parse(fs.readFileSync(path.join(references, 'manifest.json'), 'utf8'));
const catalog = JSON.parse(fs.readFileSync(path.join(theme, 'inc/catalog.json'), 'utf8'));
const overrides = JSON.parse(fs.readFileSync(path.join(theme, 'inc/game-overrides.json'), 'utf8'));
const slug = (text) => text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
let client;

(async () => {
  client = await request.newContext({ timeout: 30000 });
  const runtime = {};
  const assets = [];
  const corrections = [];
  for (const game of details) {
    const source = manifest.find((entry) => entry.slug === game.slug);
    const assetDirectory = path.join(theme, 'assets/images/game-details', game.slug);
    fs.mkdirSync(assetDirectory, { recursive: true });
    const cache = new Map();
    async function image(url, name, alt) {
      if (cache.has(url)) return cache.get(url);
      const relative = `game-details/${game.slug}/${name}.webp`;
      const target = path.join(theme, 'assets/images', relative);
      if (!fs.existsSync(target)) {
        const parsed = new URL(url);
        if (parsed.hostname !== 'www.flynnsarcades.com' || !parsed.pathname.startsWith('/wp-content/uploads/')) throw new Error(`Unexpected image origin: ${url}`);
        let response;
        for (let attempt = 0; attempt < 3; attempt++) {
          try {
            response = await client.get(url);
            if (!response.ok() || !response.headers()['content-type']?.startsWith('image/')) throw new Error(`HTTP ${response.status()}: ${url}`);
            await sharp(await response.body(), { animated: false }).resize({ width: 1280, withoutEnlargement: true }).webp({ quality: 85, effort: 4 }).toFile(target);
            break;
          } catch (error) { if (attempt === 2) throw error; }
        }
      }
      const metadata = await sharp(target).metadata();
      const result = { image: relative, width: metadata.width, height: metadata.height, alt };
      cache.set(url, result);
      assets.push({ image: relative, source: url, width: metadata.width, height: metadata.height, bytes: fs.statSync(target).size });
      return result;
    }
    const gallery = [];
    for (const [index, url] of game.gallery.entries()) gallery.push(await image(url, `screenshot-${index + 1}`, `${game.title} gameplay screenshot ${index + 1}`));
    const heroSlides = [...gallery];
    if (!heroSlides.length) for (const [index, url] of game.heroSlides.entries()) heroSlides.push(await image(url, `screenshot-${index + 1}`, `${game.title} gameplay screenshot ${index + 1}`));
    if (!heroSlides.length) throw new Error(`No original gameplay artwork found for ${game.title}`);
    if (game.heroSlides.some((url) => !game.gallery.includes(url)) && gallery.length) corrections.push({ game: game.slug, type: 'hero-media', resolution: 'Use the game-specific gallery images instead of unrelated or duplicate carousel artwork.' });
    const poster = game.poster ? await image(game.poster, 'cover', `${game.title} cover artwork`) : { image: catalog.find((item) => slug(item.title) === game.slug).image, width: 640, height: 400, alt: game.title };
    const stores = [];
    for (const item of source.stores) {
      const parsed = new URL(item.url);
      parsed.search = '';
      const platform = /steampowered/.test(parsed.hostname) ? 'PC' : /xbox/.test(parsed.hostname) ? 'Xbox' : /playstation/.test(parsed.hostname) ? 'PlayStation' : 'Switch';
      stores.push({ label: platform === 'PC' ? 'Steam' : platform === 'Switch' ? 'Nintendo Switch' : platform, platform, url: parsed.href });
    }
    for (const store of overrides[game.slug]?.extra_stores || []) if (!stores.some((item) => item.platform === store.platform)) stores.push(store);
    if (!stores.length) throw new Error(`Missing verified store link for ${game.title}`);
    const published = new Set(game.stores.map((store) => store.url.split('?')[0]));
    if (stores.some((store) => !published.has(store.url))) corrections.push({ game: game.slug, type: 'purchase', resolution: 'Use the correct product links from the original catalog, with separately verified Steam corrections where needed.' });
    const reviews = game.reviews.filter((review) => {
      const context = `${review.quote} ${review.url}`.toLowerCase();
      const own = context.includes(game.title.toLowerCase()) || context.includes(game.slug);
      const other = catalog.find((item) => slug(item.title) !== game.slug && (context.includes(item.title.toLowerCase()) || context.includes(slug(item.title))));
      if (other && !own) { corrections.push({ game: game.slug, type: 'unrelated-review', excluded: other.title }); return false; }
      return true;
    });
    const related = game.related.map((title) => catalog.find((item) => slug(item.title) === slug(title))).filter((item) => item && slug(item.title) !== game.slug).map((item) => slug(item.title));
    const videoOverride = overrides[game.slug];
    const excluded = videoOverride?.excluded_video_ids || [];
    const videos = game.videos.filter((video) => !excluded.includes(video.id));
    const primary = videos.find((video) => video.role === 'hero') || videos.find((video) => video.role === 'hero-lightbox') || videos[0];
    if (!primary) throw new Error(`Missing original video for ${game.title}`);
    const secondary = videos.find((video) => video.role === 'features') || videos.find((video) => video.role === 'hero-lightbox') || primary;
    const videoData = ({ id, url, start }) => ({ id, url, ...(start ? { start } : {}) });
    runtime[game.slug] = { tagline: game.tagline, hero_video: videoOverride?.hero_video || videoData(primary), gallery: gallery.length ? gallery : heroSlides, poster, stores, genres: game.genres, blocks: game.blocks, trailers: videoOverride?.trailers || [videoData(secondary)], reviews, related };
    for (const id of excluded) corrections.push({ game: game.slug, type: 'unrelated-video', excluded: id, resolution: 'Retain the game-specific video; exclude the copied Legend of Gwen lightbox from Papertris.' });
    if (!gallery.length) corrections.push({ game: game.slug, type: 'gallery-placement', resolution: 'Move the original hero screenshots below the primary video, preserving all captures in the shared gallery.' });
    for (const video of [runtime[game.slug].hero_video, ...runtime[game.slug].trailers].filter(Boolean)) {
      if (!game.trailers.some((sourceVideo) => sourceVideo.id === video.id)) throw new Error(`Video not found in the original page: ${game.slug} / ${video.id}`);
    }
    console.log(`Prepared ${game.title}: ${runtime[game.slug].gallery.length} screenshots, primary video ${runtime[game.slug].hero_video.id}, trailer ${runtime[game.slug].trailers[0].id}.`);
  }
  fs.writeFileSync(path.join(theme, 'inc/game-details.json'), JSON.stringify(runtime, null, 2) + '\n');
  fs.writeFileSync(path.join(references, 'asset-manifest.json'), JSON.stringify(assets, null, 2));
  fs.writeFileSync(path.join(references, 'corrections.json'), JSON.stringify(corrections, null, 2));
  console.log(JSON.stringify({ games: Object.keys(runtime).length, assets: assets.length, assetBytes: assets.reduce((sum, asset) => sum + asset.bytes, 0), corrections: corrections.length }));
  await client.dispose();
})().catch(async (error) => { console.error(error); await client?.dispose(); process.exitCode = 1; });
