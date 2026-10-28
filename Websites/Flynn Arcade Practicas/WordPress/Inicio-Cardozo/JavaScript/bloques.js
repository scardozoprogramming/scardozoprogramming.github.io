(() => {
  'use strict';
  function init() {
    const root = document.querySelector('[data-elementor-id="6876"]');
    if (!root || !root.querySelector('.fc-catalog') || root.dataset.fcReady) return;
    if (document.body.classList.contains('elementor-editor-active')) return;
    root.dataset.fcReady = 'true';
    const banner = root.querySelector('.fc-banner img');
    if (banner && !banner.alt) banner.alt = 'Panoramic neon arcade overlooking the city';
    const motion = matchMedia('(prefers-reduced-motion: reduce)');
    const pointer = matchMedia('(hover: hover) and (pointer: fine)');
    const cache = new Map();
    const refresh = [];
    root.querySelectorAll('a[data-fc-preview]').forEach(link => {
      const image = link.querySelector('img');
      if (!image) return;
      const host = link.closest('.fc-card, .fc-promo-slide, .fc-featured') || link;
      const cover = image.getAttribute('src');
      const srcset = image.getAttribute('srcset');
      const sizes = image.getAttribute('sizes');
      const preview = link.dataset.fcPreview;
      if (!image.alt) image.alt = link.getAttribute('aria-label')?.replace(/^Discover /,'') || '';
      const active = () => !document.hidden && !host.hidden && !motion.matches && ((pointer.matches && host.matches(':hover')) || host.matches(':focus-within'));
      function update() {
        if (!active()) {
          if (image.getAttribute('src') !== cover) image.setAttribute('src',cover);
          if (srcset) image.setAttribute('srcset',srcset);
          if (sizes) image.setAttribute('sizes',sizes);
          image.classList.remove('fc-preview-active');
          return;
        }
        if (!cache.has(preview)) {
          const preload = new Image();
          preload.src = preview;
          cache.set(preview,preload.decode().then(() => true,() => false));
        }
        cache.get(preview).then(ready => {
          if (!ready || !active()) return;
          image.removeAttribute('srcset');
          image.removeAttribute('sizes');
          image.src = preview;
          image.classList.add('fc-preview-active');
        });
      }
      ['pointerenter','pointerleave','focusin'].forEach(name => host.addEventListener(name,update));
      host.addEventListener('focusout',() => requestAnimationFrame(update));
      refresh.push(update);
    });
    const refreshPreviews = () => refresh.forEach(update => update());
    document.addEventListener('visibilitychange',refreshPreviews);
    motion.addEventListener('change',refreshPreviews);
    pointer.addEventListener('change',refreshPreviews);

    const catalog = root.querySelector('.fc-catalog');
    const cards = [...catalog.querySelectorAll('.fc-card')];
    const search = catalog.querySelector('[data-fc-search]');
    const filters = [...catalog.querySelectorAll('a[data-fc-filter]')];
    const count = catalog.querySelector('.fc-catalog-count .elementor-heading-title');
    const more = catalog.querySelector('.fc-loadmore');
    const empty = catalog.querySelector('.fc-empty');
    let category = 'all';
    let limit = 6;
    function readLocation() {
      const params = new URL(location.href).searchParams;
      category = filters.some(a => a.dataset.fcFilter === params.get('genre')) ? params.get('genre') : 'all';
      search.value = (params.get('q') || '').slice(0,100);
      limit = Math.max(6,Math.min(cards.length,Number(params.get('shown')) || 6));
    }
    function render(save = true) {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      cards.forEach(card => {
        const title = card.querySelector('.fc-card-title')?.textContent.toLowerCase() || '';
        const match = (category === 'all' || card.classList.contains('fc-category-'+category)) && title.includes(query);
        if (match) matches++;
        card.hidden = !match || matches > limit;
      });
      count.textContent = `${Math.min(matches,limit)} / ${matches} games`;
      count.setAttribute('role','status');
      count.setAttribute('aria-live','polite');
      more.hidden = matches <= limit;
      empty.style.display = matches ? 'none' : 'block';
      filters.forEach(a => {
        const selected = a.dataset.fcFilter === category;
        a.setAttribute('aria-pressed',String(selected));
        a.setAttribute('role','button');
        a.closest('.fc-filter').classList.toggle('fc-current',selected);
      });
      if (save) {
        const url = new URL(location.href);
        const values = {q:search.value.trim(),genre:category === 'all' ? '' : category,shown:limit > 6 ? String(limit) : ''};
        Object.entries(values).forEach(([key,value]) => value ? url.searchParams.set(key,value) : url.searchParams.delete(key));
        history.replaceState(null,'',url);
      }
      refreshPreviews();
    }
    filters.forEach(a => {
      a.addEventListener('click',event => { event.preventDefault(); category = a.dataset.fcFilter; limit = 6; render(); });
      a.addEventListener('keydown',event => { if (event.key === ' ') { event.preventDefault(); a.click(); } });
    });
    search.addEventListener('input',() => { limit = 6; render(); });
    more.querySelector('a').addEventListener('click',event => { event.preventDefault(); limit += 6; render(); });
    window.addEventListener('popstate',() => { readLocation(); render(false); });
    readLocation();
    render(false);

    const promotions = root.querySelector('.fc-promotions');
    const slides = [...promotions.querySelectorAll('.fc-promo-slide')];
    const dots = [...promotions.querySelectorAll('a[data-fc-slide]')];
    let current = 0;
    promotions.setAttribute('aria-label','Game highlights');
    promotions.setAttribute('aria-roledescription','carousel');
    function showSlide(index) {
      current = (index + slides.length) % slides.length;
      slides.forEach((slide,i) => {
        slide.hidden = i !== current;
        slide.setAttribute('role','group');
        slide.setAttribute('aria-label',`${i+1} of ${slides.length}`);
      });
      dots.forEach((a,i) => {
        a.setAttribute('aria-pressed',String(i === current));
        a.setAttribute('role','button');
        a.closest('.fc-promo-dot').classList.toggle('fc-current',i === current);
      });
      refreshPreviews();
    }
    dots.forEach(a => a.addEventListener('click',event => { event.preventDefault(); showSlide(Number(a.dataset.fcSlide)); }));
    promotions.querySelector('a[data-fc-prev]').addEventListener('click',event => { event.preventDefault(); showSlide(current-1); });
    promotions.querySelector('a[data-fc-next]').addEventListener('click',event => { event.preventDefault(); showSlide(current+1); });
    showSlide(0);
    setInterval(() => {
      if (!motion.matches && !document.hidden && !promotions.matches(':hover') && !promotions.contains(document.activeElement)) showSlide(current+1);
    },7000);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',init,{once:true});
  else init();
})();
