(() => {
  const toggle = document.querySelector('.nav-toggle');
  const nav = document.querySelector('.site-nav');
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
  document.addEventListener('keydown', (event) => { if (event.key === 'Escape') closeNavigation(); });
  document.addEventListener('click', (event) => {
    if (!event.target.closest('.site-header')) closeNavigation();
  });

  const viewer = document.querySelector('[data-gallery-dialog]');
  const photos = Array.from(document.querySelectorAll('[data-gallery-item]'));
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

  document.querySelectorAll('[data-promotions], [data-carousel]').forEach((carousel) => {
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

  document.querySelectorAll('[data-video-id]').forEach((link) => {
    link.addEventListener('click', (event) => {
      // Local files have no HTTP Referer; keep the working YouTube link in that case.
      if (!/^https?:$/.test(location.protocol) || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey || !/^[\w-]{11}$/.test(link.dataset.videoId)) return;
      event.preventDefault();
      const player = new URL(`https://www.youtube-nocookie.com/embed/${link.dataset.videoId}`);
      player.searchParams.set('autoplay', '1');
      player.searchParams.set('rel', '0');
      player.searchParams.set('origin', location.origin);
      const start = Number(link.dataset.videoStart);
      if (Number.isInteger(start) && start > 0) player.searchParams.set('start', String(start));
      const frame = document.createElement('iframe');
      frame.referrerPolicy = 'strict-origin-when-cross-origin';
      frame.src = player.href;
      frame.title = link.dataset.videoTitle;
      frame.allow = 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share';
      frame.allowFullscreen = true;
      link.closest('.game-video-frame').replaceChildren(frame);
      frame.focus();
    });
  });

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
    let category = 'all';
    let limit = 6;

    function render() {
      const query = search.value.trim().toLowerCase();
      let matches = 0;
      cards.forEach((card) => {
        const match = (category === 'all' || card.dataset.categories.split(',').includes(category)) && card.dataset.title.toLowerCase().includes(query);
        if (match) matches++;
        card.hidden = !match || matches > limit;
      });
      more.hidden = matches <= limit;
      empty.hidden = matches > 0;
      count.textContent = matches ? `${Math.min(matches, limit)} / ${matches} games` : '';
      refreshGamePreviews();
    }
    filters.forEach((button) => button.addEventListener('click', () => {
      category = button.dataset.filter;
      limit = 6;
      filters.forEach((item) => item.setAttribute('aria-pressed', String(item === button)));
      render();
    }));
    search.addEventListener('input', () => { limit = 6; render(); });
    more.addEventListener('click', () => { limit += 6; render(); });
    render();
  }

  document.querySelector('[data-newsletter]')?.addEventListener('submit', (event) => {
    event.preventDefault();
    const note = document.querySelector('[data-newsletter-note]');
    note.textContent = 'Subscriptions are not open yet. Please check back soon.';
    note.hidden = false;
  });
})();
