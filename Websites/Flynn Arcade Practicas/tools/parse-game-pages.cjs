const { chromium } = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs = require('node:fs');
const path = require('node:path');
const directory = path.resolve(__dirname, '../checks/game-reference');
const manifest = JSON.parse(fs.readFileSync(path.join(directory, 'manifest.json'), 'utf8'));
let browser;

(async () => {
  browser = await chromium.launch({ channel: 'msedge', headless: true });
  const page = await browser.newPage();
  const details = [];
  for (const game of manifest) {
    const html = fs.readFileSync(path.join(directory, `${game.slug}.html`), 'utf8');
    const data = await page.evaluate(({ html, game }) => {
      const doc = new DOMParser().parseFromString(html, 'text/html');
      const main = doc.querySelector('[data-elementor-type="wp-page"]') || doc.querySelector('main');
      if (!main) throw new Error(`Missing main content for ${game.localTitle}`);
      const clean = (value) => (value || '').replace(/\s+/g, ' ').trim();
      const text = (el) => clean(el?.textContent);
      const unique = (items) => [...new Set(items.filter(Boolean))];
      const sections = Array.from(main.children).filter((el) => ['SECTION', 'DIV'].includes(el.tagName));
      const headings = (section) => Array.from(section.querySelectorAll('.elementor-heading-title')).map(text).filter(Boolean);
      const purchase = sections.find((section) => headings(section).some((heading) => /^purchase$/i.test(heading)));
      const body = sections.find((section) => section.querySelector('[data-widget_type="text-editor.default"]'));
      const galleryWidgets = Array.from(main.querySelectorAll('[data-widget_type="gallery.default"], [data-widget_type="image-gallery.default"]'));
      const imageUrl = (value) => {
        try {
          const url = new URL(value, game.source);
          return /\.(png|jpe?g|gif|webp)(\?|$)/i.test(url.href) && /(^|\.)flynnsarcades\.com$/i.test(url.hostname) ? url.href : '';
        } catch { return ''; }
      };
      const gallery = unique(galleryWidgets.flatMap((widget) => Array.from(widget.querySelectorAll('a[href]')).map((link) => imageUrl(link.getAttribute('href')))));
      const backgroundUrl = (el) => imageUrl(el.style.backgroundImage.match(/url\(["']?(.*?)["']?\)/)?.[1]);
      const carousel = main.querySelector('[data-widget_type="media-carousel.default"]');
      const heroSlides = unique(Array.from(carousel?.querySelectorAll('.elementor-carousel-image') || []).map(backgroundUrl)).map((url) => {
        if (!/\.gif$/i.test(url)) return url;
        const matching = gallery.find((image) => new URL(image).pathname.replace(/\.[^.]+$/, '') === new URL(url).pathname.replace(/\.[^.]+$/, ''));
        return matching || url;
      });
      const stores = purchase ? Array.from(purchase.querySelectorAll('a[href]')).map((link) => ({ label: text(link), url: link.getAttribute('href') })).filter((link) => link.label && /^https?:\/\//i.test(link.url) && !/(^|\.)flynnsarcades\.com$/i.test(new URL(link.url).hostname)) : [];
      const blocks = [];
      const genres = [];
      if (body) {
        const firstText = body.querySelector('[data-widget_type="text-editor.default"]');
        for (const heading of body.querySelectorAll('.elementor-heading-title')) {
          if (heading.compareDocumentPosition(firstText) & Node.DOCUMENT_POSITION_FOLLOWING) genres.push(text(heading));
        }
        for (const widget of body.querySelectorAll('[data-widget_type="text-editor.default"]')) {
          const root = widget.querySelector('.elementor-widget-container') || widget;
          const nodes = Array.from(root.querySelectorAll('p,ul,ol,h1,h2,h3,h4')).filter((node) => !node.parentElement.closest('p,ul,ol'));
          if (!nodes.length && text(root)) blocks.push({ type: 'paragraph', text: text(root) });
          for (const node of nodes) {
            if (node.matches('ul,ol')) {
              const items = Array.from(node.children).map(text).filter(Boolean);
              if (items.length) blocks.push({ type: 'list', items });
              continue;
            }
            const copy = node.cloneNode(true);
            copy.querySelectorAll('br').forEach((br) => br.replaceWith('\u0000'));
            const lines = copy.textContent.split('\u0000').map(clean).filter(Boolean);
            if (!lines.length) continue;
            const bullet = /^(?:[\u2013\u2014\u2022-]\s*|\*\s+)/;
            if (lines.some((line) => bullet.test(line))) {
              let paragraph = [];
              let items = [];
              const flush = () => {
                if (paragraph.length) blocks.push({ type: 'paragraph', text: paragraph.join(' ') });
                if (items.length) blocks.push({ type: 'list', items });
                paragraph = [];
                items = [];
              };
              for (const line of lines) {
                if (bullet.test(line)) {
                  if (paragraph.length) flush();
                  items.push(line.replace(bullet, ''));
                } else if (/^(?:(?:key|game|other)\s+)?features\s*:?$/i.test(line)) {
                  flush();
                  blocks.push({ type: 'heading', text: line });
                } else if (items.length) {
                  items[items.length - 1] += ' ' + line;
                } else {
                  paragraph.push(line);
                }
              }
              flush();
            } else {
              const value = lines.join(' ');
              blocks.push({ type: node.matches('h1,h2,h3,h4') || /^features\s*:?(\s|$)/i.test(value) && value.length < 30 ? 'heading' : 'paragraph', text: value });
            }
          }
        }
      }
      const videos = [];
      const addVideo = (url, role, widget, startSetting) => {
        try {
          const parsed = new URL(url);
          if (!['www.youtube.com', 'youtube.com', 'youtu.be', 'www.youtube-nocookie.com'].includes(parsed.hostname)) return;
          let id = parsed.searchParams.get('v');
          if (!id && (parsed.hostname === 'youtu.be' || parsed.pathname.includes('/embed/'))) id = parsed.pathname.split('/').filter(Boolean).pop();
          if (!/^[\w-]{11}$/.test(id || '')) return;
          const offset = String(startSetting ?? parsed.searchParams.get('start') ?? parsed.searchParams.get('t') ?? '0');
          const time = offset.match(/^(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?$/);
          const seconds = /^\d+$/.test(offset) ? Number(offset) : time ? Number(time[1] || 0) * 3600 + Number(time[2] || 0) * 60 + Number(time[3] || 0) : 0;
          const start = Number.isSafeInteger(seconds) && seconds > 0 ? seconds : 0;
          videos.push({ role, widget, id, url: `https://www.youtube.com/watch?v=${id}${start ? `&t=${start}s` : ''}`, ...(start ? { start } : {}) });
        } catch {}
      };
      for (const element of main.querySelectorAll('[data-settings]')) {
        try {
          const settings = JSON.parse(element.getAttribute('data-settings'));
          if (settings.youtube_url) addVideo(settings.youtube_url, body?.contains(element) ? 'features' : 'hero', element.getAttribute('data-id'), settings.start);
        } catch {}
      }
      main.querySelectorAll('[data-elementor-lightbox-video]').forEach((element) => addVideo(element.getAttribute('data-elementor-lightbox-video'), body?.contains(element) ? 'features' : 'hero-lightbox', element.closest('[data-id]')?.getAttribute('data-id')));
      const trailers = [];
      for (const { id, url, start } of videos) if (!trailers.some((video) => video.id === id)) trailers.push({ id, url, ...(start ? { start } : {}) });
      const reviews = Array.from(main.querySelectorAll('.elementor-testimonial')).map((review) => {
        const quote = text(review.querySelector('.elementor-testimonial__text'));
        const words = quote.split(/\s+/);
        return { quote: words.slice(0, 24).join(' ') + (words.length > 24 ? '...' : ''), author: text(review.querySelector('.elementor-testimonial__name')), outlet: text(review.querySelector('.elementor-testimonial__title')), url: review.querySelector('.elementor-testimonial__title a')?.getAttribute('href') || '' };
      }).filter((review) => review.quote && review.outlet);
      const related = unique(Array.from(main.querySelectorAll('.eael-image-accordion-item h2,.eael-image-accordion-item h3')).map(text));
      const bodyImages = Array.from(body?.querySelectorAll('img') || []);
      const poster = imageUrl(bodyImages[0]?.getAttribute('src'));
      const heroHeadings = sections[0] ? headings(sections[0]) : [];
      return { slug: game.slug, title: game.localTitle, tagline: heroHeadings[1] || game.tagline, heroSlides: unique(heroSlides), gallery, stores, genres: unique(genres), blocks, videos, trailers, reviews, related, poster,
        sourceOrder: sections.map((section) => section === purchase ? 'purchase' : section === body ? 'body' : section.querySelector('[data-widget_type="gallery.default"]') ? 'gallery' : section.querySelector('.eael-image-accordion-item') ? 'related' : section.querySelector('[data-widget_type="media-carousel.default"],[data-widget_type="video.default"]') ? 'hero' : 'other') };
    }, { html, game });
    details.push(data);
  }
  fs.writeFileSync(path.join(directory, 'details-extracted.json'), JSON.stringify(details, null, 2));
  console.log(JSON.stringify(details.map((game) => ({ title: game.title, hero: game.heroSlides.length, screenshots: game.gallery.length, stores: game.stores, genres: game.genres, paragraphs: game.blocks.filter((block) => block.type === 'paragraph').length, lists: game.blocks.filter((block) => block.type === 'list').length, trailers: game.trailers, reviews: game.reviews.length, related: game.related, order: game.sourceOrder })), null, 2));
  await browser.close();
})().catch(async (error) => { console.error(error); await browser?.close(); process.exitCode = 1; });
