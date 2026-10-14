(() => {
  const nav = document.querySelector('.site-nav');
  const navToggle = document.querySelector('.nav-toggle');
  const dialogs = Array.from(document.querySelectorAll('dialog'));

  function closeNavigation() {
    nav?.classList.remove('is-open');
    navToggle?.setAttribute('aria-expanded', 'false');
  }

  navToggle?.addEventListener('click', () => {
    const open = nav.classList.toggle('is-open');
    navToggle.setAttribute('aria-expanded', String(open));
  });

  function openDialog(id) {
    const dialog = document.getElementById(id);
    if (!(dialog instanceof HTMLDialogElement)) return;
    dialogs.forEach((item) => { if (item.open && item !== dialog) item.close(); });
    if (!dialog.open) dialog.showModal();
    document.body.classList.add('has-dialog');
    closeNavigation();
  }

  dialogs.forEach((dialog) => {
    dialog.querySelector('[data-close-dialog]')?.addEventListener('click', () => dialog.close());
    dialog.addEventListener('close', () => {
      document.body.classList.toggle('has-dialog', dialogs.some((item) => item.open));
    });
    dialog.addEventListener('click', (event) => {
      if (event.target !== dialog) return;
      const bounds = dialog.getBoundingClientRect();
      if (event.clientX < bounds.left || event.clientX > bounds.right || event.clientY < bounds.top || event.clientY > bounds.bottom) dialog.close();
    });
  });

  document.querySelectorAll('[data-dialog]').forEach((link) => {
    link.addEventListener('click', (event) => {
      event.preventDefault();
      openDialog(link.dataset.dialog);
    });
  });

  nav?.querySelectorAll('a').forEach((link) => {
    link.addEventListener('click', () => {
      closeNavigation();
      if (link.dataset.dialog) return;
      nav.querySelectorAll('a').forEach((item) => item.classList.toggle('is-active', item === link));
    });
  });

  document.addEventListener('keydown', (event) => {
    if (event.key !== 'Escape') return;
    closeNavigation();
    const dialog = dialogs.find((item) => item.open);
    if (dialog) {
      event.preventDefault();
      dialog.close();
    }
  });

  const catalog = document.getElementById('catalog');
  const cards = Array.from(catalog?.querySelectorAll('[data-game-card]') || []);
  const filterButtons = Array.from(catalog?.querySelectorAll('[data-filter]') || []);
  const searchInput = catalog?.querySelector('[data-game-search]');
  const emptyState = catalog?.querySelector('[data-empty-state]');
  let selectedFilter = 'all';

  function applyFilters() {
    const query = (searchInput?.value || '').trim().toLowerCase();
    let count = 0;
    cards.forEach((card) => {
      const categoryMatch = selectedFilter === 'all' || card.dataset.categories.split(',').includes(selectedFilter);
      const titleMatch = card.dataset.title.toLowerCase().includes(query);
      card.hidden = !(categoryMatch && titleMatch);
      if (!card.hidden) count++;
    });
    if (emptyState) emptyState.hidden = count > 0;
    catalog.querySelector('[data-game-details]').hidden = true;
  }

  filterButtons.forEach((button) => {
    button.addEventListener('click', () => {
      selectedFilter = button.dataset.filter;
      filterButtons.forEach((item) => item.setAttribute('aria-pressed', String(item === button)));
      applyFilters();
    });
  });
  searchInput?.addEventListener('input', applyFilters);

  document.querySelectorAll('[data-game]').forEach((button) => {
    button.addEventListener('click', () => {
      const details = catalog.querySelector('[data-game-details]');
      details.querySelector('[data-detail-title]').textContent = button.dataset.game;
      details.querySelector('[data-detail-summary]').textContent = button.dataset.summary;
      const platforms = Array.from(button.querySelectorAll('.platform-icon')).map((icon) => icon.title);
      details.querySelector('[data-detail-platforms]').textContent = `Platforms: ${platforms.join(', ')}`;
      details.hidden = false;
      openDialog('catalog');
      details.focus();
    });
  });

  const spotlights = JSON.parse(document.getElementById('spotlight-data')?.textContent || '[]');
  const spotlightTitle = document.querySelector('[data-spotlight-title]');
  const spotlightCopy = document.querySelector('[data-spotlight-copy]');
  const dots = Array.from(document.querySelectorAll('[data-spotlight-index]'));
  let spotlightIndex = 0;

  function showSpotlight(index) {
    if (!spotlights.length) return;
    spotlightIndex = (index + spotlights.length) % spotlights.length;
    if (spotlightTitle) spotlightTitle.textContent = spotlights[spotlightIndex].title;
    if (spotlightCopy) spotlightCopy.textContent = spotlights[spotlightIndex].summary;
    dots.forEach((dot, i) => dot.setAttribute('aria-pressed', String(i === spotlightIndex)));
  }
  dots.forEach((dot) => dot.addEventListener('click', () => showSpotlight(Number(dot.dataset.spotlightIndex))));
  document.querySelectorAll('[data-spotlight-step]').forEach((button) => {
    button.addEventListener('click', () => showSpotlight(spotlightIndex + Number(button.dataset.spotlightStep)));
  });

  document.querySelector('[data-newsletter]')?.addEventListener('submit', (event) => {
    event.preventDefault();
    const note = document.querySelector('[data-newsletter-note]');
    note.textContent = 'Subscriptions are not open yet. Please check back soon.';
    note.hidden = false;
  });

  const initialDialog = location.hash.slice(1);
  if (dialogs.some((dialog) => dialog.id === initialDialog)) openDialog(initialDialog);
})();
