
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
    const root = document.querySelector('[data-elementor-id="6925"]');
    if (!root || root.dataset.fcAboutReady) return;
    if (document.body.classList.contains('elementor-editor-active')) return;
    root.dataset.fcAboutReady = 'true';
    root.querySelectorAll('.fc-about-logo img').forEach(img => { img.alt = "Flynn's Arcade Publishing"; });
    const brand = root.querySelector('.fc-about-brand');
    brand.id = 'about-studio';
    const gallery = root.querySelector('.fc-about-gallery');
    gallery.setAttribute('role', 'region');
    gallery.setAttribute('aria-label', 'Studio photos');
    gallery.querySelectorAll('.fc-about-photo a').forEach(link => {
      const image = link.querySelector('img');
      if (image) image.alt = link.getAttribute('aria-label') || image.alt;
    });
    // Keep this page's native Elementor photo viewer in English as well.
    gallery.addEventListener('click', () => {
      const translate = () => {
        const viewer = document.querySelector('.elementor-lightbox');
        if (!viewer) return;
        const labels = { '.dialog-close-button':'Close photo (Esc)', '.eicon-share-arrow':'Share', '.eicon-frame-expand':'Full screen' };
        Object.entries(labels).forEach(([selector, label]) => viewer.querySelector(selector)?.setAttribute('aria-label', label));
        [['.elementor-swiper-button-next', 'Next photo'], ['.elementor-swiper-button-prev', 'Previous photo']].forEach(([selector, label]) => {
          const button = viewer.querySelector(selector);
          button?.setAttribute('aria-label', label);
          const text = button?.querySelector('.screen-reader-text');
          if (text && text.textContent !== label) text.textContent = label;
        });
      };
      const observer = new MutationObserver(translate);
      observer.observe(document.body, { childList:true, subtree:true });
      translate();
      setTimeout(() => observer.disconnect(), 5000);
    });
    const projects = root.querySelector('.fc-about-projects');
    projects.setAttribute('role', 'region');
    projects.setAttribute('aria-label', 'Projects');
    window.FlynnsHomePreviews(root);
  }
  const start = () => {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, { once: true });
    else init();
  };
  // Existing shared footer code can still invoke these legacy entry points.
  if (!window.FlynnsArcade && document.querySelector('[data-elementor-id="6925"]')) {
    window.FlynnsArcade = { navigation: start, carousels: start, catalog: start, gallery: start };
  }
  start();
})();

(() => {
  'use strict';
  function prepare() {
    const root = document.querySelector('[data-elementor-id="6925"]');
    if (!root) return;
    const logo = root.querySelector('.fc-home-logo img');
    if (logo) logo.alt = "Flynn's Arcade Publishing";
    const header = root.querySelector('.fc-local-header');
    const nav = header?.querySelector('.fc-nav-col');
    const toggle = header?.querySelector('.fc-home-nav-toggle a');
    if (!header || !nav || !toggle || header.dataset.fcNavReady) return;
    header.dataset.fcNavReady = 'true';
    nav.id = 'fc-about-navigation';
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
