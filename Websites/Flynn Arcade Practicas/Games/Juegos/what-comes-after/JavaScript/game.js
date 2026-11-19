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
  function prepare() {
    const setup = document.querySelector('.fc-game-setup');
    const root = setup?.closest('[data-elementor-id]');
    if (!root || root.dataset.fcGameReady) return;
    root.dataset.fcGameReady = 'true';
    for(const [selector,anchor] of [['.fc-game-page','overview'],['.fc-game-gallery','screenshots'],['.fc-game-purchase','purchase'],['.fc-game-about','about-game'],['.fc-game-reviews','reviews']]) {
      const section = root.querySelector(selector);
      if(section) section.id = anchor;
    }
    const title = root.querySelector('.fc-game-title')?.textContent.trim() || 'Game';
    root.querySelectorAll('.fc-game-gallery img').forEach((img,i) => { img.alt = `${title} gameplay screenshot ${i+1}`; });
    const artwork = root.querySelector('.fc-game-artwork img');
    if (artwork) artwork.alt = title + ' cover artwork';
    const videoCover = root.querySelector('.fc-game-video img');
    if (videoCover) videoCover.alt = title + ' video preview';
    root.querySelectorAll('.fc-game-video [role="button"]').forEach(button => button.setAttribute('aria-label','Play video: '+title));
    if (window.FlynnsHomePreviews) window.FlynnsHomePreviews(root);

    const animation = root.querySelector('.fc-game-animation img');
    const toggle = root.querySelector('.fc-game-animation-toggle a');
    if (animation && toggle) {
      const animated = animation.getAttribute('src');
      const still = root.querySelector('.fc-game-gallery img')?.getAttribute('src') || videoCover?.getAttribute('src') || artwork?.getAttribute('src');
      const motion = window.matchMedia('(prefers-reduced-motion: reduce)');
      let paused = motion.matches || !!navigator.connection?.saveData;
      let near = false;
      animation.alt = title + ' gameplay preview';
      animation.removeAttribute('srcset');
      const update = () => {
        const running = !paused && near && !document.hidden;
        if (still) animation.src = running ? animated : still;
        toggle.setAttribute('aria-label', paused ? 'Play animation' : 'Pause animation');
        toggle.setAttribute('aria-pressed', String(paused));
        toggle.querySelector('.elementor-button-text').textContent = paused ? 'Play animation' : 'Pause animation';
        const icon = toggle.querySelector('i');
        if (icon) icon.className = paused ? 'fas fa-play' : 'fas fa-pause';
      };
      toggle.addEventListener('click', e => { e.preventDefault(); paused = !paused; update(); });
      toggle.addEventListener('keydown', e => { if(e.key === ' ') { e.preventDefault(); toggle.click(); } });
      motion.addEventListener('change', e => { paused = e.matches; update(); });
      document.addEventListener('visibilitychange', update);
      const observer = new IntersectionObserver(entries => { near = entries[0].isIntersecting; update(); }, {rootMargin:'100px'});
      observer.observe(animation);
      update();
    }

    const slides = [...root.querySelectorAll('.fc-game-review-slide')];
    let current = 0;
    const show = index => {
      current = (index + slides.length) % slides.length;
      slides.forEach((slide,i) => { slide.hidden = i !== current; });
      const count = root.querySelector('.fc-game-review-count .elementor-heading-title');
      if (count) { count.textContent = `${current+1} / ${slides.length}`; count.setAttribute('aria-live','polite'); }
    };
    root.querySelectorAll('.fc-game-review-prev a,.fc-game-review-next a').forEach(a => a.addEventListener('click', e => {
      e.preventDefault(); show(current + (a.closest('.fc-game-review-prev') ? -1 : 1));
    }));
    if (slides.length) show(0);

    const dialog = root.querySelector('[data-fc-gallery-dialog]');
    const shots = [...root.querySelectorAll('.fc-game-gallery a')];
    if (dialog && shots.length) {
      document.body.append(dialog);
      let index = 0, opener;
      const img = dialog.querySelector('img');
      const showPhoto = next => {
        index = (next+shots.length)%shots.length;
        img.src = shots[index].href;
        img.alt = shots[index].querySelector('img').alt;
        dialog.querySelector('[data-fc-photo-count]').textContent = `${index+1} / ${shots.length}`;
      };
      shots.forEach((a,i) => a.addEventListener('click', e => {
        e.preventDefault(); e.stopPropagation(); opener = a; showPhoto(i); dialog.showModal();
      }));
      dialog.querySelector('[data-fc-photo-close]').addEventListener('click', () => dialog.close());
      dialog.querySelectorAll('[data-fc-photo-step]').forEach(b => b.addEventListener('click', () => showPhoto(index+Number(b.dataset.fcPhotoStep))));
      dialog.addEventListener('keydown', e => {
        if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') { e.preventDefault(); showPhoto(index+(e.key==='ArrowLeft'?-1:1)); }
      });
      dialog.addEventListener('click', e => { if(e.target === dialog) dialog.close(); });
      dialog.addEventListener('close', () => opener?.focus());
    }
  }
  if(document.readyState === 'loading') document.addEventListener('DOMContentLoaded',prepare,{once:true});
  else prepare();
})();
