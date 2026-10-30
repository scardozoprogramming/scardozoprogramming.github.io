(() => {
  'use strict';
  function prepare() {
    const root = document.querySelector('[data-elementor-id="6961"]');
    if (!root) return;
    const logo = root.querySelector('.fc-home-logo img');
    if (logo) logo.alt = "Flynn's Arcade Publishing";
    const header = root.querySelector('.fc-local-header');
    const nav = header?.querySelector('.fc-nav-col');
    const toggle = header?.querySelector('.fc-home-nav-toggle a');
    if (!header || !nav || !toggle || header.dataset.fcNavReady) return;
    header.dataset.fcNavReady = 'true';
    nav.id = 'fc-indies-navigation';
    nav.setAttribute('role','navigation');
    nav.setAttribute('aria-label','Primary');
    toggle.setAttribute('aria-controls',nav.id);
    const setOpen = open => {
      header.classList.toggle('fc-nav-open',open);
      toggle.setAttribute('aria-expanded',String(open));
      toggle.setAttribute('aria-label',open ? 'Close navigation' : 'Open navigation');
    };
    toggle.addEventListener('click',event => { event.preventDefault(); setOpen(!header.classList.contains('fc-nav-open')); });
    toggle.addEventListener('keydown',event => { if (event.key===' ') { event.preventDefault(); toggle.click(); } });
    nav.addEventListener('click',event => { if (event.target.closest('a')) setOpen(false); });
    header.addEventListener('keydown',event => { if(event.key==='Escape') { setOpen(false); toggle.focus(); } });
    setOpen(false);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',prepare,{capture:true,once:true});
  else prepare();
})();
