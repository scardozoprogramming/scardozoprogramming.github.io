'use strict';
(() => {
  const menuButton = document.getElementById('menu-toggle');
  const menu = document.getElementById('main-nav');
  const dialog = document.getElementById('site-dialog');
  const dialogTitle = document.getElementById('modal-title');
  const dialogBody = document.getElementById('modal-body');
  const closeButton = document.getElementById('modal-close');
  const modalData = window.RETRO_SITE_DATA || { machines:{}, activities:{}, blogs:{} };
  let focusBeforeDialog = null;

  function closeMenu() {
    if (!menuButton || !menu) return;
    menu.classList.remove('is-open');
    menuButton.setAttribute('aria-expanded', 'false');
  }
  if (menuButton && menu) {
    menuButton.addEventListener('click', () => {
      const isOpen = menu.classList.toggle('is-open');
      menuButton.setAttribute('aria-expanded', String(isOpen));
    });
    menu.querySelectorAll('a').forEach(a => a.addEventListener('click', closeMenu));
    document.addEventListener('click', e => {
      if (!menu.contains(e.target) && !menuButton.contains(e.target)) closeMenu();
    });
    window.addEventListener('resize', () => { if (window.innerWidth > 800) closeMenu(); });
  }

  function node(tag, className, contents) {
    const el = document.createElement(tag);
    if (className) el.className = className;
    if (contents !== undefined) el.textContent = contents;
    return el;
  }
  function openDialog(title, text, meta, image) {
    if (!dialog || !dialogTitle || !dialogBody) return;
    focusBeforeDialog = document.activeElement;
    dialogTitle.textContent = title;
    dialogBody.replaceChildren();
    if (image) {
      const fig = node('div', 'modal-figure');
      const img = document.createElement('img');
      img.src = image;
      img.alt = 'Ilustración de ' + title;
      img.width = 320; img.height = 220;
      fig.append(img); dialogBody.append(fig);
    }
    if (meta) dialogBody.append(node('p', 'modal-detail', meta));
    dialogBody.append(node('p', '', text));
    if (dialog.open) dialog.close();
    dialog.showModal();
    closeButton?.focus();
  }
  closeButton?.addEventListener('click', () => dialog.close());
  dialog?.addEventListener('click', e => { if (e.target === dialog) dialog.close(); });
  dialog?.addEventListener('close', () => { focusBeforeDialog?.focus?.(); });
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape') closeMenu();
  });

  document.addEventListener('click', e => {
    const machineButton = e.target.closest('[data-machine]');
    if (machineButton) {
      const item = modalData.machines[machineButton.dataset.machine];
      if (item) openDialog(item.title, item.text, item.meta, item.image);
      return;
    }
    const activityButton = e.target.closest('[data-activity]');
    if (activityButton) {
      const item = modalData.activities[activityButton.dataset.activity];
      if (item) openDialog(item.title, item.text, item.meta);
      return;
    }
    const blogButton = e.target.closest('[data-blog]');
    if (blogButton) {
      const item = modalData.blogs[blogButton.dataset.blog];
      if (item) openDialog(item.title, item.text, item.meta);
      return;
    }
    const infoButton = e.target.closest('[data-info]');
    if (infoButton) {
      const legal = infoButton.dataset.info === 'legal';
      openDialog(legal ? 'Aviso legal' : 'Privacidad',
        legal ? 'La información legal y los datos del titular de este sitio deben incorporarse por la entidad responsable antes de habilitar su publicación institucional.'
              : 'El formulario informativo únicamente comprueba los campos en tu navegador; no envía datos ni realiza inscripciones. La política de privacidad oficial deberá publicarse antes de habilitar cualquier recogida de datos.',
        'Información del sitio');
    }
  });

  document.querySelectorAll('.filter-strip').forEach(strip => {
    strip.addEventListener('click', e => {
      const button = e.target.closest('button[data-filter]');
      if (!button) return;
      const [group, type] = button.dataset.filter.split(':');
      const list = group === 'activity' ? document.querySelectorAll('.activity-card') : document.querySelectorAll('.machine-card[data-machine-type]');
      strip.querySelectorAll('button[data-filter]').forEach(b => b.setAttribute('aria-pressed', String(b === button)));
      let shown = 0;
      list.forEach(card => {
        const isMatch = type === 'all' || (group === 'activity' ? card.dataset.category === type : card.dataset.machineType === type);
        card.hidden = !isMatch;
        if (isMatch) shown++;
      });
      const status = document.getElementById(group === 'activity' ? 'activity-filter-status' : 'machine-filter-status');
      if (status) status.textContent = shown + (group === 'activity' ? (shown===1 ? ' actividad disponible para explorar' : ' actividades disponibles para explorar') : (shown===1 ? ' máquina' : ' máquinas'));
    });
  });

  const form = document.getElementById('formulario');
  if (form) {
    const params = new URLSearchParams(window.location.search);
    const motivo = params.get('motivo');
    if (['participar', 'propuesta', 'consulta'].includes(motivo)) form.elements.motivo.value = motivo;
    form.addEventListener('submit', e => {
      e.preventDefault();
      const result = document.getElementById('form-result');
      if (!form.reportValidity()) { if (result) result.hidden = true; return; }
      if (result) {
        result.textContent = '✓ Campos correctos. Este formulario aún no está conectado a un servicio de contacto: no se han enviado ni guardado los datos.';
        result.hidden = false;
        result.scrollIntoView({block:'nearest', behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'instant' : 'smooth'});
      }
    });
  }
})();
