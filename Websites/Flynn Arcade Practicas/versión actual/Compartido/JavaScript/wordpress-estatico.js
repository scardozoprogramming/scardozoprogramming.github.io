/* Enhancements for the static copy; no WordPress server or editor is required. */
(() => {
  'use strict';
  function init() {
    const page = document.body.dataset.page;
    const labels = {home:'Home',games:'Games',game:'Games',indies:'Indies',about:'About',press:'Press'};
    for (const item of document.querySelectorAll('.fc-shared-header .fc-home-menu-item')) {
      const link = item.querySelector('a');
      const current = link?.textContent.trim().toLowerCase() === labels[page]?.toLowerCase();
      item.classList.toggle('fc-active',current);
      if(current) link.setAttribute('aria-current','page');
      else link?.removeAttribute('aria-current');
    }
    document.querySelectorAll('.elementor-invisible').forEach(e=>e.classList.remove('elementor-invisible'));
    for(const title of document.querySelectorAll('.elementor-accordion .elementor-tab-title, .elementor-toggle .elementor-tab-title')){
      const panel=title.nextElementSibling;
      if(!panel?.classList.contains('elementor-tab-content'))continue;
      title.setAttribute('role','button');title.tabIndex=0;
      // The outer title is the keyboard control; avoid a second nested tab stop.
      title.querySelector('a')?.setAttribute('tabindex','-1');
      const setOpen=open=>{title.classList.toggle('elementor-active',open);panel.classList.toggle('elementor-active',open);title.setAttribute('aria-expanded',String(open));panel.hidden=!open;panel.style.display=open?'block':'none'};
      setOpen(title.classList.contains('elementor-active'));
      title.addEventListener('click',e=>{e.preventDefault();setOpen(title.getAttribute('aria-expanded')!=='true')});
      title.addEventListener('keydown',e=>{if(['Enter',' '].includes(e.key)){e.preventDefault();setOpen(title.getAttribute('aria-expanded')!=='true')}});
    }

    let dialog, image, caption, previous, next, close, group=[], index=0, opener;
    const show = () => {
      const link=group[index];
      image.src=link.href;
      image.alt=link.querySelector('img')?.alt || link.getAttribute('aria-label') || '';
      caption.textContent=`${index+1} / ${group.length}${image.alt?' · '+image.alt:''}`;
      previous.hidden=next.hidden=group.length<2;
    };
    function createViewer() {
      dialog=document.createElement('dialog');
      dialog.className='fa-local-lightbox elementor-lightbox';
      dialog.setAttribute('aria-label','Image viewer');
      dialog.innerHTML='<button type="button" class="fa-viewer-close dialog-close-button" aria-label="Close photo (Esc)">×</button><button type="button" class="fa-viewer-previous elementor-swiper-button-prev" aria-label="Previous photo">‹</button><figure><img alt=""><figcaption></figcaption></figure><button type="button" class="fa-viewer-next elementor-swiper-button-next" aria-label="Next photo">›</button>';
      document.body.append(dialog);
      image=dialog.querySelector('img'); caption=dialog.querySelector('figcaption');
      previous=dialog.querySelector('.fa-viewer-previous'); next=dialog.querySelector('.fa-viewer-next');close=dialog.querySelector('.fa-viewer-close');
      const move=step=>{index=(index+step+group.length)%group.length;show()};
      previous.addEventListener('click',()=>move(-1)); next.addEventListener('click',()=>move(1));
      close.addEventListener('click',()=>dialog.close());
      dialog.addEventListener('click',e=>{if(e.target===dialog)dialog.close()});
      dialog.addEventListener('close',()=>{image.removeAttribute('src');opener?.focus()});
      dialog.addEventListener('keydown',e=>{if(e.key==='ArrowRight'||e.key==='ArrowLeft'){e.preventDefault();move(e.key==='ArrowRight'?1:-1)}});
    }
    document.addEventListener('click',event=>{
      const link=event.target.closest('a[data-elementor-open-lightbox="yes"]');
      if(!link||event.ctrlKey||event.metaKey||event.shiftKey||event.altKey)return;
      if(!/\.(?:webp|png|jpe?g|gif|avif)(?:[?#]|$)/i.test(link.href))return;
      event.preventDefault();
      const key=link.getAttribute('data-elementor-lightbox-slideshow');
      group=[...document.querySelectorAll('a[data-elementor-open-lightbox="yes"]')].filter(a=>
        key?a.getAttribute('data-elementor-lightbox-slideshow')===key:a===link);
      index=group.indexOf(link);opener=link;
      if(!dialog)createViewer();show();dialog.showModal();close.focus();
    });
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',init,{once:true});
  else init();
})();
