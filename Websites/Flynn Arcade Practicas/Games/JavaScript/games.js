
/* Bounded, shared loading for hover animations. Included inline in Home. */
(function (global, create) {
  if (typeof module === 'object' && module.exports) module.exports = create;
  else global.FlynnsHomePreviews = create;
})(typeof window !== 'undefined' ? window : globalThis, function (root, options) {
  'use strict';
  options = options || {};
  const doc = root.ownerDocument;
  const view = doc.defaultView;
  const motion = view.matchMedia('(prefers-reduced-motion: reduce)');
  const pointer = view.matchMedia('(hover: hover) and (pointer: fine)');
  const connection = view.navigator.connection;
  const limited = () => connection?.saveData || /(^|-)2g$/.test(connection?.effectiveType || '');
  const cache = new Map();
  const queue = [];
  const states = [];
  let running = 0;
  let scheduled = false;
  const hidden = state => state.host.hidden || !!state.host.closest('[hidden]');
  const active = state => !doc.hidden && !motion.matches && !hidden(state) && ((pointer.matches && state.hovered) || state.host.contains(doc.activeElement));
  const prefetchable = state => !doc.hidden && !motion.matches && pointer.matches && !limited() && state.near && !hidden(state);

  function load(url, priority) {
    return new Promise(resolve => {
      const image = new view.Image();
      let settled = false;
      const finish = result => {
        if (settled) return;
        settled = true;
        clearTimeout(timeout);
        image.onload = image.onerror = null;
        if (!result) image.removeAttribute?.('src');
        resolve(result);
      };
      const timeout = setTimeout(() => finish(null), 12000);
      image.decoding = 'async';
      image.fetchPriority = priority === 0 ? 'high' : 'low';
      image.onload = () => {
        const done = () => finish(image.naturalWidth ? { image, url } : null);
        if (image.decode) image.decode().then(done, done);
        else done();
      };
      image.onerror = () => finish(null);
      image.src = url;
    });
  }
  function mark(entry, status) {
    entry.status = status;
    entry.states.forEach(state => { state.link.dataset.fcPreviewState = status; });
  }
  function trim() {
    const ready = [...cache.entries()].filter(([, entry]) => entry.status === 'ready');
    ready.sort((a, b) => a[1].used - b[1].used);
    while (ready.length > 8) {
      const index = ready.findIndex(([, entry]) => ![...entry.states].some(active));
      if (index < 0) break;
      const [key, entry] = ready.splice(index, 1)[0];
      cache.delete(key);
      entry.image = null;
    }
  }
  function schedule() {
    if (scheduled) return;
    scheduled = true;
    Promise.resolve().then(() => { scheduled = false; pump(); });
  }
  async function run(entry) {
    running++;
    mark(entry, 'loading');
    let result = null;
    for (const url of entry.urls) {
      result = await load(url, entry.priority);
      if (result) break;
    }
    running--;
    entry.used = Date.now();
    if (result) {
      entry.image = result.image;
      entry.url = result.url;
      mark(entry, 'ready');
    } else {
      entry.failedAt = Date.now();
      mark(entry, 'error');
    }
    entry.resolve(result);
    trim();
    schedule();
  }
  function pump() {
    if (doc.hidden) return;
    queue.sort((a, b) => a.priority - b.priority);
    while (queue.length && running < (limited() ? 1 : 2)) {
      const entry = queue.shift();
      const needed = [...entry.states].some(state => active(state) || prefetchable(state));
      if (!needed) {
        cache.delete(entry.key);
        mark(entry, 'idle');
        entry.resolve(null);
        continue;
      }
      run(entry);
    }
  }
  function ensure(state, priority) {
    let entry = cache.get(state.preview);
    if (entry?.status === 'error' && priority === 0 && Date.now() - entry.failedAt > 5000) {
      cache.delete(state.preview);
      entry = null;
    }
    if (!entry) {
      entry = { key: state.preview, urls: [...new Set([state.preview, state.fallback].filter(Boolean))], states: new Set(), status: 'queued', priority, used: Date.now() };
      entry.promise = new Promise(resolve => { entry.resolve = resolve; });
      cache.set(state.preview, entry);
      queue.push(entry);
    }
    entry.states.add(state);
    entry.used = Date.now();
    entry.priority = Math.min(entry.priority, priority);
    state.link.dataset.fcPreviewState = entry.status;
    schedule();
    return entry.promise;
  }
  function restore(state) {
    const { image, cover, srcset, sizes } = state;
    if (image.getAttribute('src') !== cover) image.setAttribute('src', cover);
    if (srcset && image.getAttribute('srcset') !== srcset) image.setAttribute('srcset', srcset);
    if (sizes && image.getAttribute('sizes') !== sizes) image.setAttribute('sizes', sizes);
    image.classList.remove('fc-preview-active');
  }
  function update(state) {
    if (!active(state)) { restore(state); return; }
    ensure(state, 0).then(result => {
      if (!result || !active(state)) return;
      state.image.removeAttribute('srcset');
      state.image.removeAttribute('sizes');
      if (state.image.getAttribute('src') !== result.url) state.image.src = result.url;
      state.image.classList.add('fc-preview-active');
    });
  }
  root.querySelectorAll('a[data-fc-preview]').forEach(link => {
    const image = link.querySelector('img');
    if (!image) return;
    const original = link.dataset.fcPreview;
    const media = options.media?.[original] || {};
    const preview = media.preview || original;
    const state = { link, image, host: link.closest('.fc-card, .fc-promo-slide, .fc-featured') || link, preview, fallback: link.dataset.fcPreviewFallback || media.fallback || (preview !== original ? original : ''), cover: image.getAttribute('src'), srcset: image.getAttribute('srcset'), sizes: image.getAttribute('sizes'), hovered: false, near: false };
    link.dataset.fcPreview = preview;
    link.dataset.fcPreviewState = 'idle';
    if (!image.alt) image.alt = link.getAttribute('aria-label')?.replace(/^Discover /, '') || '';
    state.host.addEventListener('pointerenter', () => { state.hovered = true; update(state); });
    state.host.addEventListener('pointerleave', () => { state.hovered = false; update(state); });
    state.host.addEventListener('focusin', () => update(state));
    state.host.addEventListener('focusout', () => view.requestAnimationFrame(() => update(state)));
    states.push(state);
  });
  const refresh = () => {
    states.forEach(state => {
      update(state);
      if (prefetchable(state)) ensure(state, 1);
    });
    schedule();
  };
  if (view.IntersectionObserver) {
    const observer = new view.IntersectionObserver(entries => {
      entries.forEach(entry => states.filter(state => state.host === entry.target).forEach(state => {
        state.near = entry.isIntersecting;
        if (prefetchable(state)) ensure(state, 1);
      }));
    }, { rootMargin: '300px 0px', threshold: 0 });
    [...new Set(states.map(state => state.host))].forEach(host => observer.observe(host));
  }
  doc.addEventListener('visibilitychange', refresh);
  motion.addEventListener('change', refresh);
  pointer.addEventListener('change', refresh);
  connection?.addEventListener?.('change', refresh);
  return { refresh };
});

(() => {
  'use strict';
  function init() {
    const root = document.querySelector('[data-elementor-id="6953"]');
    if (!root || !root.querySelector('.fc-catalog') || root.dataset.fcReady) return;
    if (document.body.classList.contains('elementor-editor-active')) return;
    root.dataset.fcReady = 'true';
    const catalog = root.querySelector('.fc-catalog');
    catalog.id = 'games';
    const cards = [...catalog.querySelectorAll('.fc-card')];
    const search = catalog.querySelector('[data-fc-search]');
    const filters = [...catalog.querySelectorAll('a[data-fc-filter]')];
    const count = catalog.querySelector('.fc-catalog-count .elementor-heading-title');
    const more = catalog.querySelector('.fc-loadmore');
    const empty = catalog.querySelector('.fc-empty');
    const reset = catalog.querySelector('[data-fc-reset]');
    const previews = window.FlynnsHomePreviews(root);
    let category = 'all';
    let limit = 6;

    catalog.setAttribute('aria-label', 'Games catalog');
    const filterbar = catalog.querySelector('.fc-filterbar');
    filterbar.setAttribute('role', 'group');
    filterbar.setAttribute('aria-label', 'Filter games by genre');
    count.setAttribute('role', 'status');
    count.setAttribute('aria-live', 'polite');
    function readLocation() {
      const params = new URL(location.href).searchParams;
      category = filters.some(a => a.dataset.fcFilter === params.get('genre')) ? params.get('genre') : 'all';
      search.value = (params.get('q') || '').slice(0, 100);
      const shown = Number(params.get('shown'));
      limit = Number.isFinite(shown) ? Math.max(6, Math.min(cards.length, Math.floor(shown))) : 6;
    }
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      cards.forEach(card => {
        const title = card.querySelector('.fc-card-title')?.textContent.toLowerCase() || '';
        const match = (category === 'all' || card.classList.contains('fc-category-' + category)) && title.includes(query);
        if (match) matches++;
        card.hidden = !match || matches > limit;
      });
      count.textContent = `${Math.min(matches, limit)} / ${matches} games`;
      more.hidden = matches <= limit;
      empty.hidden = matches !== 0;
      if (reset) reset.hidden = !query && category === 'all';
      filters.forEach(a => {
        const selected = a.dataset.fcFilter === category;
        a.setAttribute('aria-pressed', String(selected));
        a.setAttribute('role', 'button');
        a.closest('.fc-filter').classList.toggle('fc-current', selected);
      });
      if (save) {
        const url = new URL(location.href);
        const values = { q: search.value.trim(), genre: category === 'all' ? '' : category, shown: limit > 6 ? String(limit) : '' };
        Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
        history.replaceState(null, '', url);
      }
      previews.refresh();
    }
    filters.forEach(a => {
      a.addEventListener('click', event => { event.preventDefault(); category = a.dataset.fcFilter; limit = 6; render(); });
      a.addEventListener('keydown', event => { if (event.key === ' ') { event.preventDefault(); a.click(); } });
    });
    search.addEventListener('input', () => { limit = 6; render(); });
    reset?.addEventListener('click', () => { category = 'all'; search.value = ''; limit = 6; render(); search.focus(); });
    more.querySelector('a').addEventListener('click', event => { event.preventDefault(); limit += 6; render(); });
    window.addEventListener('popstate', () => { readLocation(); render(false); });
    readLocation();
    render(false);
  }
  const start = () => {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, { once: true });
    else init();
  };
  // Compatibility with the existing footer's legacy catalog entry points.
  if (!window.FlynnsArcade && document.querySelector('[data-elementor-id="6953"]')) {
    window.FlynnsArcade = { navigation: start, carousels: start, catalog: start };
  }
  start();
})();

(() => {
  'use strict';
  function prepare() {
    const root = document.querySelector('[data-elementor-id="6953"]');
    if (!root) return;
    const banner = root.querySelector('.fc-banner img');
    if (banner && !banner.alt) banner.alt = 'Panoramic neon arcade overlooking the city';
    const logo = root.querySelector('.fc-home-logo img');
    if (logo) logo.alt = "Flynn's Arcade Publishing";
    const header = root.querySelector('.fc-local-header');
    const nav = header?.querySelector('.fc-nav-col');
    const toggle = header?.querySelector('.fc-home-nav-toggle a');
    if (!header || !nav || !toggle || header.dataset.fcNavReady) return;
    header.dataset.fcNavReady = 'true';
    nav.id = 'fc-games-navigation';
    nav.setAttribute('role','navigation');
    nav.setAttribute('aria-label','Primary');
    toggle.setAttribute('aria-controls',nav.id);
    const setOpen = open => {
      header.classList.toggle('fc-nav-open',open);
      toggle.setAttribute('aria-expanded',String(open));
      toggle.setAttribute('aria-label',open ? 'Close navigation' : 'Open navigation');
    };
    toggle.addEventListener('click',event => { event.preventDefault(); setOpen(!header.classList.contains('fc-nav-open')); });
    toggle.addEventListener('keydown',event => { if (event.key===' ') { event.preventDefault(); toggle.click(); } });
    nav.addEventListener('click',event => { if (event.target.closest('a')) setOpen(false); });
    header.addEventListener('keydown',event => { if(event.key==='Escape') { setOpen(false); toggle.focus(); } });
    setOpen(false);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',prepare,{capture:true,once:true});
  else prepare();
})();
