'use strict';
(() => {
  if (window.FlynnsCardozoHome) return;
  const mounted = new Map();
  const handlers = new Map();
  const api = { behaviors: {
 navigation(root, listen) { root.classList.add('js-enabled');
const toggle = root.querySelector('.nav-toggle');
  const nav = root.querySelector('.site-nav');
  function closeNavigation() {
    nav?.classList.remove('is-open');
    toggle?.setAttribute('aria-expanded', 'false');
    toggle?.setAttribute('aria-label', 'Open navigation');
  }
  toggle?.addEventListener('click', () => {
    const open = nav.classList.toggle('is-open');
    toggle.setAttribute('aria-expanded', String(open));
    toggle.setAttribute('aria-label', open ? 'Close navigation' : 'Open navigation');
  });
  listen(document, 'keydown', (event) => {
    if (event.key === 'Escape' && nav?.classList.contains('is-open')) {
      closeNavigation();
      toggle.focus();
    }
  });
  listen(document, 'click', (event) => {
    if (!event.target.closest('.site-header')) closeNavigation();
  });
  listen(document, 'focusin', (event) => {
    if (!event.target.closest('.site-header')) closeNavigation();
  });
  window.matchMedia('(min-width: 601px)').addEventListener('change', closeNavigation);
},
 gallery(root, listen) {
const viewer = root.querySelector('[data-gallery-dialog]');
  const photos = Array.from(root.querySelectorAll('[data-gallery-item]'));
  if (viewer && typeof viewer.showModal === 'function' && photos.length) {
    const image = viewer.querySelector('[data-gallery-image]');
    const count = viewer.querySelector('[data-gallery-count]');
    let current = 0;
    let opener;
    function showPhoto(index) {
      current = (index + photos.length) % photos.length;
      image.src = photos[current].href;
      image.alt = photos[current].querySelector('img').alt;
      count.textContent = `${current + 1} / ${photos.length}`;
    }
    photos.forEach((photo, index) => photo.addEventListener('click', (event) => {
      if (event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
      event.preventDefault();
      opener = photo;
      showPhoto(index);
      viewer.showModal();
      viewer.querySelector('[data-gallery-close]').focus();
    }));
    viewer.querySelector('[data-gallery-close]').addEventListener('click', () => viewer.close());
    viewer.querySelectorAll('[data-gallery-step]').forEach((button) => button.addEventListener('click', () => showPhoto(current + Number(button.dataset.galleryStep))));
    viewer.addEventListener('keydown', (event) => {
      if (event.key === 'ArrowLeft' || event.key === 'ArrowRight') {
        event.preventDefault();
        showPhoto(current + (event.key === 'ArrowRight' ? 1 : -1));
      }
    });
    viewer.addEventListener('click', (event) => {
      if (event.target !== viewer) return;
      const bounds = viewer.getBoundingClientRect();
      if (event.clientX < bounds.left || event.clientX > bounds.right || event.clientY < bounds.top || event.clientY > bounds.bottom) viewer.close();
    });
    viewer.addEventListener('close', () => opener?.focus({ preventScroll: true }));
  }
},
 carousels(root, listen) {
root.querySelectorAll('[data-promotions], [data-carousel]').forEach((carousel) => {
    const slides = Array.from(carousel.querySelectorAll('[data-promotion-slide], [data-carousel-slide]'));
    const dots = Array.from(carousel.querySelectorAll('[data-promotion-index], [data-carousel-index]'));
    const count = carousel.querySelector('[data-carousel-count]');
    let current = 0;
    function show(index) {
      current = (index + slides.length) % slides.length;
      slides.forEach((slide, i) => { slide.hidden = i !== current; });
      dots.forEach((dot, i) => dot.setAttribute('aria-pressed', String(i === current)));
      if (count) count.textContent = `${current + 1} / ${slides.length}`;
    }
    carousel.querySelectorAll('[data-promotion-step], [data-carousel-step]').forEach((button) => button.addEventListener('click', () => show(current + Number(button.dataset.carouselStep ?? button.dataset.promotionStep))));
    dots.forEach((dot, i) => dot.addEventListener('click', () => show(i)));
    carousel.addEventListener('keydown', (event) => {
      if (event.key === 'ArrowLeft' || event.key === 'ArrowRight') {
        event.preventDefault();
        show(current + (event.key === 'ArrowRight' ? 1 : -1));
      }
    });
  });
},
 directory(root, listen) {
function updateLocation(values) {
    const url = new URL(location.href);
    Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
    try { history.replaceState(null, '', url); } catch { /* File previews may restrict history changes. */ }
  }
root.querySelectorAll('[data-directory]').forEach((directory) => {
    const rows = Array.from(directory.querySelectorAll('[data-directory-item]'));
    const search = directory.querySelector('[data-directory-search]');
    const clear = directory.querySelector('[data-directory-clear]');
    const more = directory.querySelector('[data-directory-more]');
    const count = directory.querySelector('[data-directory-count]');
    const empty = directory.querySelector('[data-directory-empty]');
    const pageSize = Math.max(1, Math.floor(Number(directory.dataset.directoryPageSize) || 6));
    const params = new URL(location.href).searchParams;
    let limit = Math.max(pageSize, Math.min(rows.length, Number(params.get('shown')) || pageSize));
    search.value = (params.get('q') || '').slice(0, 100);
    directory.querySelector('[data-directory-tools]').hidden = false;
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      rows.forEach((row) => {
        const match = row.dataset.directoryQuery.toLowerCase().includes(query);
        if (match) matches++;
        row.hidden = !match || matches > limit;
      });
      count.textContent = `${Math.min(matches, limit)} / ${matches} ${directory.dataset.directory}`;
      more.hidden = matches <= limit;
      empty.hidden = matches > 0;
      clear.hidden = !search.value;
      if (save) updateLocation({ q: search.value.trim(), shown: limit > pageSize ? limit : '' });
    }
    search.addEventListener('input', () => { limit = pageSize; render(); });
    clear.addEventListener('click', () => { search.value = ''; limit = pageSize; render(); search.focus(); });
    more.addEventListener('click', () => { limit += pageSize; render(); });
    listen(window, 'popstate', () => {
      const params = new URL(location.href).searchParams;
      search.value = (params.get('q') || '').slice(0, 100);
      limit = Math.max(pageSize, Math.min(rows.length, Number(params.get('shown')) || pageSize));
      render(false);
    });
    render(false);
  });
}
}, register(view, handler) { handlers.set(view, handler); scan(document); } };
  window.FlynnsCardozoHome = api;
  function scan(scope) {
    const roots = [...(scope.querySelectorAll?.('[data-flynns-cardozo-home]') || [])];
    if (scope.matches?.('[data-flynns-cardozo-home]')) roots.unshift(scope);
    roots.forEach((root) => {
      const handler = handlers.get(root.dataset.view);
      if (!handler || mounted.has(root)) return;
      const controller = new AbortController();
      const listen = (object, event, callback) => object.addEventListener(event, callback, {signal: controller.signal});
      mounted.set(root, controller);
      handler(root, listen);
    });
  }
  function observe() {
    scan(document);
    new MutationObserver((records) => {
      records.forEach((record) => record.addedNodes.forEach((node) => { if (node.nodeType === 1) scan(node); }));
      mounted.forEach((controller, root) => { if (!root.isConnected) { controller.abort(); mounted.delete(root); } });
    }).observe(document.body, {childList: true, subtree: true});
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', observe, {once: true});
  else observe();
})();

'use strict';
window.FlynnsCardozoHome.behaviors.catalog = function (root, listen) {
const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');
  const hoverPointer = window.matchMedia('(hover: hover) and (pointer: fine)');
  const previewCache = new Map();
  const gamePreviews = Array.from(root.querySelectorAll('[data-game-preview]')).map((image) => {
    const link = image.closest('a');
    const cover = image.getAttribute('src');
    const preview = image.dataset.gamePreview;
    const isActive = () => !document.hidden && !image.closest('[hidden]') && !reducedMotion.matches && ((hoverPointer.matches && link.matches(':hover')) || link.matches(':focus-visible'));
    function update() {
      if (!isActive()) {
        if (image.getAttribute('src') !== cover) image.setAttribute('src', cover);
        return;
      }
      // Decode on demand and keep the cover visible if loading fails or focus moves.
      if (!previewCache.has(preview)) {
        const preload = new Image();
        preload.src = preview;
        previewCache.set(preview, preload.decode().then(() => true, () => false));
      }
      previewCache.get(preview).then((ready) => {
        if (ready && isActive() && image.getAttribute('src') !== preview) image.setAttribute('src', preview);
      });
    }
    ['pointerenter', 'pointerleave', 'focus', 'blur'].forEach((event) => link.addEventListener(event, update));
    return update;
  });
  const refreshGamePreviews = () => gamePreviews.forEach((update) => update());
  listen(reducedMotion, 'change', refreshGamePreviews);
  listen(hoverPointer, 'change', refreshGamePreviews);
  listen(document, 'visibilitychange', refreshGamePreviews);

  const catalog = root.querySelector('[data-catalog]');
  if (catalog) {
    const cards = Array.from(catalog.querySelectorAll('[data-game-card]'));
    const filters = Array.from(catalog.querySelectorAll('[data-filter]'));
    const search = catalog.querySelector('[data-game-search]');
    const more = catalog.querySelector('[data-load-more]');
    const count = catalog.querySelector('[data-catalog-count]');
    const empty = catalog.querySelector('[data-empty-state]');
    const reset = catalog.querySelectorAll('[data-reset-catalog]');
    let category = 'all';
    let limit = 6;

    function readLocation() {
      const params = new URL(location.href).searchParams;
      category = filters.some((button) => button.dataset.filter === params.get('genre')) ? params.get('genre') : 'all';
      search.value = (params.get('q') || '').slice(0, 100);
      limit = Math.max(6, Math.min(cards.length, Number(params.get('shown')) || 6));
    }
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      cards.forEach((card) => {
        const match = (category === 'all' || card.dataset.categories.split(',').includes(category)) && card.dataset.title.toLowerCase().includes(query);
        if (match) matches++;
        card.hidden = !match || matches > limit;
      });
      more.hidden = matches <= limit;
      empty.hidden = matches > 0;
      count.textContent = `${Math.min(matches, limit)} / ${matches} games`;
      filters.forEach((item) => item.setAttribute('aria-pressed', String(item.dataset.filter === category)));
      reset[0].hidden = category === 'all' && !search.value;
      if (save) updateLocation({ q: search.value.trim(), genre: category === 'all' ? '' : category, shown: limit > 6 ? limit : '' });
      refreshGamePreviews();
    }
    filters.forEach((button) => button.addEventListener('click', () => {
      category = button.dataset.filter;
      limit = 6;
      render();
    }));
    search.addEventListener('input', () => { limit = 6; render(); });
    more.addEventListener('click', () => { limit += 6; render(); });
    reset.forEach((button) => button.addEventListener('click', () => {
      category = 'all';
      search.value = '';
      limit = 6;
      render();
      search.focus();
    }));
    listen(window, 'popstate', () => { readLocation(); render(false); });
    readLocation();
    render(false);
  }

  function updateLocation(values) {
    const url = new URL(location.href);
    Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
    try { history.replaceState(null, '', url); } catch { /* File previews may restrict history changes. */ }
  }
};

'use strict';
window.FlynnsCardozoHome.register('home', (root, listen) => {
const FlynnsArcade = window.FlynnsCardozoHome.behaviors;
FlynnsArcade.navigation(root, listen);
FlynnsArcade.carousels(root, listen);
FlynnsArcade.catalog(root, listen);

});
