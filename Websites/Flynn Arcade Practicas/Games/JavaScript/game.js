'use strict';
(() => {
FlynnsArcade.navigation();
FlynnsArcade.gallery();
FlynnsArcade.carousels();
const players = new Map();
  document.querySelectorAll('[data-video-id]').forEach((link) => {
    link.addEventListener('click', (event) => {
      // Local files have no HTTP Referer; keep the working YouTube link in that case.
      if (!/^https?:$/.test(location.protocol) || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey || !/^[\w-]{11}$/.test(link.dataset.videoId)) return;
      event.preventDefault();
      // Restore inactive posters so two trailers never play over one another.
      players.forEach((poster, container) => container.replaceChildren(...poster));
      players.clear();
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
      players.set(container, Array.from(container.childNodes));
      container.replaceChildren(frame);
      frame.focus();
    });
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
