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
