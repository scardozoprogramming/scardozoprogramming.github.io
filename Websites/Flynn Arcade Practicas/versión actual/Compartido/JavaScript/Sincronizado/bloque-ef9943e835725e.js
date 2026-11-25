
(() => {
  const template = document.querySelector('[data-elementor-id="8134"]');
  const catalog = template?.querySelector('.fc-catalog');
  const bar = catalog?.querySelector('.fc-filterbar .elementor-widget-wrap');
  if (!bar || catalog.dataset.fcGenreTagsReady) return;
  const slug = label => label.normalize('NFKD').replace(/[̀-ͯ]/g, '').toLowerCase()
    .replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
  const genres = new Map();
  for (const card of catalog.querySelectorAll('.fc-card')) {
    const tags = Array.from(card.querySelectorAll('.fc-tags .elementor-icon-list-text'), node => node.textContent.trim()).filter(Boolean);
    if (!tags.length) continue;
    const stale = Array.from(card.classList).filter(name => name.startsWith('fc-category-'));
    card.classList.remove(...stale);
    for (const tag of tags) {
      const key = slug(tag);
      if (!key) continue;
      card.classList.add('fc-category-' + key);
      if (!genres.has(key)) genres.set(key, tag);
    }
  }
  const existing = new Set(Array.from(bar.querySelectorAll('[data-fc-filter]'), link => link.dataset.fcFilter));
  const editing = new URLSearchParams(location.search).has('elementor-preview') || document.body.classList.contains('elementor-editor-active');
  for (const [key, tag] of [...genres].sort((a, b) => a[1].localeCompare(b[1], 'en'))) {
    if (existing.has(key)) continue;
    const widget = document.createElement('div');
    widget.className = 'fc-filter elementor-widget elementor-widget-button';
    const container = document.createElement('div');
    container.className = 'elementor-widget-container';
    const link = document.createElement('a');
    link.className = 'elementor-button elementor-size-sm';
    link.href = '#' + key;
    link.dataset.fcFilter = key;
    link.textContent = tag;
    if (editing) link.addEventListener('click', event => event.preventDefault());
    container.append(link);
    widget.append(container);
    bar.append(widget);
  }
  catalog.dataset.fcGenreTagsReady = 'true';
})();
