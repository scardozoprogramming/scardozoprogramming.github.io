'use strict';
window.FlynnsArcade.catalog = function () {
const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');
  const hoverPointer = window.matchMedia('(hover: hover) and (pointer: fine)');
  const previewCache = new Map();
  const gamePreviews = Array.from(document.querySelectorAll('[data-game-preview]')).map((image) => {
    const link = image.closest('.game-link');
    const card = image.closest('[data-game-card]');
    const cover = image.getAttribute('src');
    const preview = image.dataset.gamePreview;
    const isActive = () => !document.hidden && !card.hidden && !reducedMotion.matches && ((hoverPointer.matches && link.matches(':hover')) || link.matches(':focus-visible'));
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
  reducedMotion.addEventListener('change', refreshGamePreviews);
  hoverPointer.addEventListener('change', refreshGamePreviews);
  document.addEventListener('visibilitychange', refreshGamePreviews);

  const catalog = document.querySelector('[data-catalog]');
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
    window.addEventListener('popstate', () => { readLocation(); render(false); });
    readLocation();
    render(false);
  }

  function updateLocation(values) {
    const url = new URL(location.href);
    Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
    try { history.replaceState(null, '', url); } catch { /* File previews may restrict history changes. */ }
  }
};
