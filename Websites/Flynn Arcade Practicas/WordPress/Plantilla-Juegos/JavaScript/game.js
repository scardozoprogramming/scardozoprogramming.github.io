(() => {
  'use strict';
  function prepare() {
    const setup = document.querySelector('.fc-game-setup');
    const root = setup?.closest('[data-elementor-id]');
    if (!root || root.dataset.fcGameReady) return;
    root.dataset.fcGameReady = 'true';
    for(const [selector,anchor] of [['.fc-game-page','overview'],['.fc-game-gallery','screenshots'],['.fc-game-purchase','purchase'],['.fc-game-about','about-game'],['.fc-game-reviews','reviews']]) {
      const section = root.querySelector(selector);
      if(section) section.id = anchor;
    }
    const title = root.querySelector('.fc-game-title')?.textContent.trim() || 'Game';
    root.querySelectorAll('.fc-game-gallery img').forEach((img,i) => { img.alt = `${title} gameplay screenshot ${i+1}`; });
    const artwork = root.querySelector('.fc-game-artwork img');
    if (artwork) artwork.alt = title + ' cover artwork';
    const videoCover = root.querySelector('.fc-game-video img');
    if (videoCover) videoCover.alt = title + ' video preview';
    root.querySelectorAll('.fc-game-video [role="button"]').forEach(button => button.setAttribute('aria-label','Play video: '+title));
    if (window.FlynnsHomePreviews) window.FlynnsHomePreviews(root);

    const animation = root.querySelector('.fc-game-animation img');
    const toggle = root.querySelector('.fc-game-animation-toggle a');
    if (animation && toggle) {
      const animated = animation.getAttribute('src');
      const still = root.querySelector('.fc-game-gallery img')?.getAttribute('src') || videoCover?.getAttribute('src') || artwork?.getAttribute('src');
      const motion = window.matchMedia('(prefers-reduced-motion: reduce)');
      let paused = motion.matches || !!navigator.connection?.saveData;
      let near = false;
      animation.alt = title + ' gameplay preview';
      animation.removeAttribute('srcset');
      const update = () => {
        const running = !paused && near && !document.hidden;
        if (still) animation.src = running ? animated : still;
        toggle.setAttribute('aria-label', paused ? 'Play animation' : 'Pause animation');
        toggle.setAttribute('aria-pressed', String(paused));
        toggle.querySelector('.elementor-button-text').textContent = paused ? 'Play animation' : 'Pause animation';
        const icon = toggle.querySelector('i');
        if (icon) icon.className = paused ? 'fas fa-play' : 'fas fa-pause';
      };
      toggle.addEventListener('click', e => { e.preventDefault(); paused = !paused; update(); });
      toggle.addEventListener('keydown', e => { if(e.key === ' ') { e.preventDefault(); toggle.click(); } });
      motion.addEventListener('change', e => { paused = e.matches; update(); });
      document.addEventListener('visibilitychange', update);
      const observer = new IntersectionObserver(entries => { near = entries[0].isIntersecting; update(); }, {rootMargin:'100px'});
      observer.observe(animation);
      update();
    }

    const slides = [...root.querySelectorAll('.fc-game-review-slide')];
    let current = 0;
    const show = index => {
      current = (index + slides.length) % slides.length;
      slides.forEach((slide,i) => { slide.hidden = i !== current; });
      const count = root.querySelector('.fc-game-review-count .elementor-heading-title');
      if (count) { count.textContent = `${current+1} / ${slides.length}`; count.setAttribute('aria-live','polite'); }
    };
    root.querySelectorAll('.fc-game-review-prev a,.fc-game-review-next a').forEach(a => a.addEventListener('click', e => {
      e.preventDefault(); show(current + (a.closest('.fc-game-review-prev') ? -1 : 1));
    }));
    if (slides.length) show(0);

    const dialog = root.querySelector('[data-fc-gallery-dialog]');
    const shots = [...root.querySelectorAll('.fc-game-gallery a')];
    if (dialog && shots.length) {
      document.body.append(dialog);
      let index = 0, opener;
      const img = dialog.querySelector('img');
      const showPhoto = next => {
        index = (next+shots.length)%shots.length;
        img.src = shots[index].href;
        img.alt = shots[index].querySelector('img').alt;
        dialog.querySelector('[data-fc-photo-count]').textContent = `${index+1} / ${shots.length}`;
      };
      shots.forEach((a,i) => a.addEventListener('click', e => {
        e.preventDefault(); e.stopPropagation(); opener = a; showPhoto(i); dialog.showModal();
      }));
      dialog.querySelector('[data-fc-photo-close]').addEventListener('click', () => dialog.close());
      dialog.querySelectorAll('[data-fc-photo-step]').forEach(b => b.addEventListener('click', () => showPhoto(index+Number(b.dataset.fcPhotoStep))));
      dialog.addEventListener('keydown', e => {
        if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') { e.preventDefault(); showPhoto(index+(e.key==='ArrowLeft'?-1:1)); }
      });
      dialog.addEventListener('click', e => { if(e.target === dialog) dialog.close(); });
      dialog.addEventListener('close', () => opener?.focus());
    }
  }
  if(document.readyState === 'loading') document.addEventListener('DOMContentLoaded',prepare,{once:true});
  else prepare();
})();
