(() => {
  'use strict';
  function init() {
    const root = document.querySelector('[data-elementor-id="7098"]');
    if (!root || root.dataset.fcPressReady || document.body.classList.contains('elementor-editor-active')) return;
    root.dataset.fcPressReady = 'true';
    root.querySelector('.fc-press-heading').id = 'press-media';
    root.querySelector('.fc-press-tools').id = 'coverage';
    root.querySelector('.fc-presskits').id = 'presskits';
    root.querySelector('.fc-contact-footer').id = 'contact';
    const rows = [...root.querySelectorAll('.fc-press-row')];
    const search = root.querySelector('[data-fc-press-search]');
    const clear = root.querySelector('[data-fc-press-clear]');
    const count = root.querySelector('.fc-press-count .elementor-heading-title');
    const more = root.querySelector('.fc-press-pagination');
    const empty = root.querySelector('.fc-press-empty');
    let limit = 6;
    count.setAttribute('role', 'status');
    count.setAttribute('aria-live', 'polite');
    rows.forEach(row => {
      const title = row.querySelector('.fc-article-title').textContent.trim();
      const outlet = row.querySelector('.fc-article-eyebrow').textContent.split('/').slice(1).join('/').trim();
      row.dataset.fcQuery = (title + ' ' + outlet).replace(/\s+/g, ' ').toLowerCase();
      row.setAttribute('aria-label', title);
      row.querySelector('img').alt = title.replace(/^Review: /, '');
    });
    root.querySelectorAll('.fc-presskit').forEach(kit => { kit.querySelector('img').alt = kit.querySelector('.fc-kit-title').textContent.trim(); });
    function readLocation() {
      const params = new URL(location.href).searchParams;
      const shown = Number(params.get('shown'));
      search.value = (params.get('q') || '').slice(0, 100);
      limit = Number.isFinite(shown) ? Math.max(6, Math.min(rows.length, Math.floor(shown))) : 6;
    }
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      rows.forEach(row => { const match = row.dataset.fcQuery.includes(query); if (match) matches++; row.hidden = !match || matches > limit; });
      count.textContent = `${Math.min(matches, limit)} / ${matches} articles`;
      more.hidden = matches <= limit;
      empty.hidden = matches !== 0;
      clear.hidden = !search.value;
      if (save) {
        const url = new URL(location.href);
        const values = { q:search.value.trim(), shown:limit > 6 ? String(limit) : '' };
        Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
        history.replaceState(null, '', url);
      }
    }
    search.addEventListener('input', () => { limit = 6; render(); });
    clear.addEventListener('click', () => { search.value = ''; limit = 6; render(); search.focus(); });
    more.querySelector('a').addEventListener('click', event => { event.preventDefault(); limit += 6; render(); });
    more.querySelector('a').addEventListener('keydown', event => { if (event.key === ' ') { event.preventDefault(); more.querySelector('a').click(); } });
    window.addEventListener('popstate', () => { readLocation(); render(false); });
    readLocation(); render(false);
    const promotions = root.querySelector('.fc-promotions');
    const slides = [...promotions.querySelectorAll('.fc-promo-slide')];
    slides.forEach(slide => { slide.querySelector('img').alt = slide.querySelector('.fc-eyebrow').textContent.trim(); });
    const previews = window.FlynnsHomePreviews(root);
    const dots = [...promotions.querySelectorAll('a[data-fc-slide]')];
    const motion = matchMedia('(prefers-reduced-motion: reduce)');
    let current = 0;
    promotions.setAttribute('aria-label', 'Game highlights');
    promotions.setAttribute('aria-roledescription', 'carousel');
    function showSlide(index) {
      current = (index + slides.length) % slides.length;
      slides.forEach((slide, i) => { slide.hidden = i !== current; slide.setAttribute('role', 'group'); slide.setAttribute('aria-label', `${i + 1} of ${slides.length}`); });
      dots.forEach((a, i) => { a.setAttribute('aria-pressed', String(i === current)); a.setAttribute('role', 'button'); a.closest('.fc-promo-dot').classList.toggle('fc-current', i === current); });
      previews.refresh();
    }
    dots.forEach(a => a.addEventListener('click', event => { event.preventDefault(); showSlide(Number(a.dataset.fcSlide)); }));
    promotions.querySelector('a[data-fc-prev]').addEventListener('click', event => { event.preventDefault(); showSlide(current - 1); });
    promotions.querySelector('a[data-fc-next]').addEventListener('click', event => { event.preventDefault(); showSlide(current + 1); });
    showSlide(0);
    setInterval(() => { if (!motion.matches && !document.hidden && !promotions.matches(':hover') && !promotions.contains(document.activeElement)) showSlide(current + 1); }, 7000);
  }
  const start = () => { if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, {once:true}); else init(); };
  if (!window.FlynnsArcade && document.querySelector('[data-elementor-id="7098"]')) window.FlynnsArcade = { navigation:start, carousels:start, catalog:start, directory:start };
  start();
})();
