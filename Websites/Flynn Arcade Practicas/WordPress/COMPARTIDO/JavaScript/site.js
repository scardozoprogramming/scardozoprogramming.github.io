'use strict';
window.FlynnsArcade = {
 navigation() { document.documentElement.classList.add('js-enabled');
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
  document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape' && nav?.classList.contains('is-open')) {
      closeNavigation();
      toggle.focus();
    }
  });
  document.addEventListener('click', (event) => {
    if (!event.target.closest('.site-header')) closeNavigation();
  });
  document.addEventListener('focusin', (event) => {
    if (!event.target.closest('.site-header')) closeNavigation();
  });
  window.matchMedia('(min-width: 601px)').addEventListener('change', closeNavigation);
},
 gallery() {
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
},
 carousels() {
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
},
 directory() {
function updateLocation(values) {
    const url = new URL(location.href);
    Object.entries(values).forEach(([key, value]) => value ? url.searchParams.set(key, value) : url.searchParams.delete(key));
    try { history.replaceState(null, '', url); } catch { /* File previews may restrict history changes. */ }
  }
document.querySelectorAll('[data-directory]').forEach((directory) => {
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
    window.addEventListener('popstate', () => {
      const params = new URL(location.href).searchParams;
      search.value = (params.get('q') || '').slice(0, 100);
      limit = Math.max(pageSize, Math.min(rows.length, Number(params.get('shown')) || pageSize));
      render(false);
    });
    render(false);
  });
}
};
