/* Page-local compatibility with the existing shared footer entry points. */
if (document.querySelector('.flynns-privacy-v41') && !window.FlynnsArcade) {
  window.FlynnsArcade = { navigation() {}, carousels() {}, catalog() {} };
}
(() => {
  'use strict';
  const root = document.querySelector('.flynns-privacy-v41');
  if (!root || root.dataset.faprivReady) return;
  root.dataset.faprivReady = 'true';

  const sections = [...root.querySelectorAll('.fapriv-section')];
  const sectionLinks = [...root.querySelectorAll('.fapriv-sidebar a[data-fapriv-link]')];
  sections.forEach((section,index) => { if (!section.id) section.id = sectionLinks[index]?.hash.slice(1) || 'fapriv-section-' + (index+1); });
  for (const section of sections) {
    const heading = section.querySelector('h2');
    heading.tabIndex = -1;
    heading.id = section.id + '-heading';
  }
  const links = [...root.querySelectorAll('[data-fapriv-link]')];
  const mobileIndex = root.querySelector('.fapriv-mobile-toc');
  let activeSection;
  let frameRequested = false;

  function setActive(id) {
    if (id === activeSection) return;
    activeSection = id;
    for (const link of links) {
      const isActive = link.hash === '#' + id;
      link.classList.toggle('fapriv-active', isActive);
      if (isActive) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    }
  }

  function updateActive() {
    frameRequested = false;
    const margin = parseFloat(getComputedStyle(sections[0]).scrollMarginTop) || 0;
    const padding = parseFloat(getComputedStyle(document.documentElement).scrollPaddingTop) || 0;
    const offset = margin + padding;
    let current = sections[0];
    for (const section of sections) {
      if (section.getBoundingClientRect().top > offset + 12) break;
      current = section;
    }
    if (current) setActive(current.id);
  }

  function scheduleUpdate() {
    if (frameRequested) return;
    frameRequested = true;
    window.requestAnimationFrame(updateActive);
  }

  root.addEventListener('click', (event) => {
    const link = event.target.closest('[data-fapriv-link]');
    if (!link || event.defaultPrevented || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    const target = sections.find((section) => link.hash === '#' + section.id);
    if (!target) return;
    event.preventDefault();
    if (mobileIndex.open) mobileIndex.open = false;
    target.querySelector('h2').focus({ preventScroll: true });
    target.scrollIntoView({ behavior: 'instant', block: 'start' });
    const address = new URL(window.location.href);
    address.hash = target.id;
    if (window.location.hash !== address.hash) window.history.pushState(null, '', address);
    setActive(target.id);
  });

  mobileIndex.addEventListener('keydown', (event) => {
    if (event.key !== 'Escape' || !mobileIndex.open) return;
    mobileIndex.open = false;
    mobileIndex.querySelector('summary').focus();
  });

  window.addEventListener('scroll', scheduleUpdate, { passive: true });
  window.addEventListener('resize', scheduleUpdate, { passive: true });
  window.addEventListener('hashchange', scheduleUpdate);
  updateActive();
})();
