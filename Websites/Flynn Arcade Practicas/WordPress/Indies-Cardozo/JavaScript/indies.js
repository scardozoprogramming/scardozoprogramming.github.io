(() => {
  'use strict';
  function init() {
    const root = document.querySelector('[data-elementor-id="6961"]');
    if (!root || root.dataset.fcIndiesReady || document.body.classList.contains('elementor-editor-active')) return;
    root.dataset.fcIndiesReady = 'true';
    const rows = [...root.querySelectorAll('.fc-studio-row')];
    const search = root.querySelector('[data-fc-studio-search]');
    const clear = root.querySelector('[data-fc-studio-clear]');
    const count = root.querySelector('.fc-directory-count .elementor-heading-title');
    const more = root.querySelector('.fc-directory-more');
    const empty = root.querySelector('.fc-directory-empty');
    const pageSize = 7;
    let limit = pageSize;
    const extras = { 'fc-studio-key-apamate-studio':'Wilmar Wilderman Niño', 'fc-studio-key-dust-games':'Barcelona' };
    root.querySelector('.fc-directory-heading').id = 'independent-creators';
    count.setAttribute('role', 'status');
    count.setAttribute('aria-live', 'polite');
    rows.forEach((row, index) => {
      const name = row.querySelector('.fc-studio-name').textContent.trim();
      const games = row.querySelector('.fc-studio-games').textContent.trim();
      const extra = Object.entries(extras).filter(([key]) => row.classList.contains(key)).map(([, value]) => value).join(' ');
      row.id = 'studio-' + (index + 1);
      row.setAttribute('aria-label', name);
      row.dataset.fcQuery = [name, games, extra].join(' ').replace(/\s+/g, ' ').trim().toLowerCase();
      const logo = row.querySelector('.fc-studio-logo img');
      const portrait = row.querySelector('.fc-studio-portrait img');
      if (logo) logo.alt = name + ' logo';
      if (portrait) portrait.alt = name;
      row.querySelector('.elementor-tab-title')?.setAttribute('aria-label', 'Full story: ' + name);
    });
    function readLocation() {
      const url = new URL(location.href);
      const shown = Number(url.searchParams.get('shown'));
      search.value = (url.searchParams.get('q') || '').slice(0, 100);
      limit = Number.isFinite(shown) ? Math.max(pageSize, Math.min(rows.length, Math.floor(shown))) : pageSize;
      const target = /^#studio-(\d+)$/.exec(url.hash);
      if (target) limit = Math.max(limit, Math.min(rows.length, Number(target[1])));
    }
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      rows.forEach(row => {
        const match = row.dataset.fcQuery.includes(query);
        if (match) matches++;
        row.hidden = !match || matches > limit;
      });
      count.textContent = `${Math.min(matches, limit)} / ${matches} studios`;
      more.hidden = matches <= limit;
      empty.hidden = matches !== 0;
      clear.hidden = !search.value;
      if (save) {
        const url = new URL(location.href);
        const values = { q:search.value.trim(), shown:limit > pageSize ? String(limit) : '' };
        Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
        history.replaceState(null, '', url);
      }
    }
    search.addEventListener('input', () => { limit = pageSize; render(); });
    clear.addEventListener('click', () => { search.value = ''; limit = pageSize; render(); search.focus(); });
    const moreButton = more.querySelector('a');
    moreButton.setAttribute('role', 'button');
    moreButton.addEventListener('click', event => { event.preventDefault(); limit += pageSize; render(); });
    moreButton.addEventListener('keydown', event => { if (event.key === ' ') { event.preventDefault(); moreButton.click(); } });
    window.addEventListener('popstate', () => { readLocation(); render(false); });
    readLocation();
    render(false);
  }
  const start = () => {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, { once:true });
    else init();
  };
  if (!window.FlynnsArcade && document.querySelector('[data-elementor-id="6961"]')) {
    window.FlynnsArcade = { navigation:start, carousels:start, catalog:start, directory:start };
  }
  start();
})();
