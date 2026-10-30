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
      reset.hidden = !query && category === 'all';
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
    reset.addEventListener('click', () => { category = 'all'; search.value = ''; limit = 6; render(); search.focus(); });
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
