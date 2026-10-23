'use strict';
(() => {
FlynnsArcade.navigation();
FlynnsArcade.gallery();
FlynnsArcade.carousels();
  document.querySelectorAll('[data-video-id]').forEach((link) => {
    link.addEventListener('click', (event) => {
      // Local files have no HTTP Referer; keep the working YouTube link in that case.
      if (!/^https?:$/.test(location.protocol) || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey || !/^[\w-]{11}$/.test(link.dataset.videoId)) return;
      event.preventDefault();
      const player = new URL(`https://www.youtube-nocookie.com/embed/${link.dataset.videoId}`);
      player.searchParams.set('autoplay', '1');
      player.searchParams.set('rel', '0');
      player.searchParams.set('hl', 'en');
      player.searchParams.set('origin', location.origin);
      const start = Number(link.dataset.videoStart);
      if (Number.isInteger(start) && start > 0) player.searchParams.set('start', String(start));
      const frame = document.createElement('iframe');
      frame.referrerPolicy = 'strict-origin-when-cross-origin';
      frame.src = player.href;
      frame.title = link.dataset.videoTitle;
      frame.allow = 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share';
      frame.allowFullscreen = true;
      const container = link.closest('.game-video-frame');
      container.replaceChildren(frame);
      frame.focus();
    });
  });
const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)');
  document.querySelectorAll('[data-game-animation]').forEach((preview) => {
    const image = preview.querySelector('[data-animation-image]');
    const source = preview.querySelector('source');
    const toggle = preview.querySelector('[data-animation-toggle]');
    const icon = toggle.querySelector('use');
    let playing;
    function setPlaying(value) {
      playing = value;
      image.src = playing ? image.dataset.animationSrc : image.dataset.animationStill;
      // The picture source protects reduced-motion visitors before JavaScript loads.
      source.media = 'not all';
      const label = playing ? 'Pause animation' : 'Play animation';
      toggle.setAttribute('aria-label', label);
      toggle.title = label;
      icon.setAttribute('href', playing ? '#icon-pause' : '#icon-play');
    }
    setPlaying(!reducedMotion.matches);
    toggle.hidden = false;
    toggle.addEventListener('click', () => setPlaying(!playing));
    reducedMotion.addEventListener('change', () => setPlaying(!reducedMotion.matches));
  });
const back = document.querySelector('.back-link');
  if (back && document.referrer) {
    const previous = new URL(document.referrer);
    if (previous.origin === location.origin && previous.pathname === new URL(back.href).pathname) {
      previous.hash = 'games';
      back.href = previous.href;
    }
  }
})();
