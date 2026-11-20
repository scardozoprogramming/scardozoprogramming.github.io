/* Offline interactions for the recovered archive. No WordPress or plugin runtime. */
(()=>{'use strict';
for(const gallery of document.querySelectorAll('.eael-filter-gallery-container')){
 const cards=[...gallery.querySelectorAll('.eael-filterable-gallery-item-wrap')];
 const area=gallery.closest('.elementor-widget-container')||gallery.parentElement;
 const more=area.querySelector('.eael-gallery-load-more');
 let limit=Number(gallery.dataset.archiveInitial)||cards.length,filter='*',query='';
 const render=()=>{let n=0;for(const c of cards){const match=(filter==='*'||c.matches(filter))&&c.textContent.toLowerCase().includes(query);if(match)n++;c.hidden=!match||n>limit;}if(more)more.hidden=n<=limit;};
 const search=document.querySelector('input[type="search"]');search?.addEventListener('input',()=>{query=search.value.trim().toLowerCase();limit=cards.length;render()});
 for(const control of area.querySelectorAll('[data-filter]')){control.tabIndex=0;control.setAttribute('role','button');const choose=()=>{filter=control.dataset.filter;limit=cards.length;area.querySelectorAll('[data-filter]').forEach(c=>c.classList.toggle('active',c===control));render()};control.addEventListener('click',choose);control.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();choose()}})}
 more?.addEventListener('click',e=>{e.preventDefault();limit+=6;render()});render();
}
for(const container of document.querySelectorAll('.swiper,.swiper-container')){
 const wrapper=container.querySelector('.swiper-wrapper');if(!wrapper)continue;
 const slides=[...wrapper.children].filter(e=>e.classList.contains('swiper-slide'));if(!slides.length)continue;
 let settings={};try{settings=JSON.parse(container.closest('[data-settings]')?.dataset.settings||'{}')}catch{}
 const count=innerWidth<768?1:Math.min(slides.length,Math.max(1,Number(settings.slides_per_view||settings.slides_to_show)||1));
 let index=0;container.classList.add('legacy-slider','swiper-initialized','swiper-horizontal');
 const render=()=>slides.forEach((s,i)=>{s.hidden=i<index||i>=index+count;s.style.width=100/count+'%';s.classList.toggle('swiper-slide-active',i===index)});
 const scope=container.closest('.elementor-widget-container')||container;
 const previous=scope.querySelector('.elementor-swiper-button-prev,.swiper-button-prev'),next=scope.querySelector('.elementor-swiper-button-next,.swiper-button-next');
 const move=step=>{index=(index+step+slides.length)%slides.length;render()};previous?.addEventListener('click',()=>move(-1));next?.addEventListener('click',()=>move(1));render();
}
for(const title of document.querySelectorAll('.elementor-tab-title')){const panel=title.nextElementSibling;if(!panel?.classList.contains('elementor-tab-content'))continue;title.tabIndex=0;const open=value=>{title.classList.toggle('elementor-active',value);panel.classList.toggle('elementor-active',value);panel.hidden=!value;panel.style.display=value?'block':'none';title.setAttribute('aria-expanded',String(value))};open(title.classList.contains('elementor-active'));title.addEventListener('click',e=>{e.preventDefault();open(title.getAttribute('aria-expanded')!=='true')});title.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();title.click()}})}
for(const toggle of document.querySelectorAll('.elementor-menu-toggle')){const menu=toggle.closest('.elementor-widget-container')?.querySelector('.elementor-nav-menu--dropdown');if(!menu)continue;toggle.addEventListener('click',()=>{const open=toggle.getAttribute('aria-expanded')!=='true';toggle.classList.toggle('elementor-active',open);toggle.setAttribute('aria-expanded',String(open));menu.classList.toggle('legacy-nav-open',open)})}
for(const form of document.forms)form.addEventListener('submit',e=>e.preventDefault());
document.addEventListener('click',e=>{const a=e.target.closest('a[data-elementor-open-lightbox="yes"]');if(!a||!/\.(?:jpe?g|png|webp|gif)(?:[?#]|$)/i.test(a.href))return;e.preventDefault();const dialog=document.createElement('dialog');dialog.className='legacy-photo-viewer';const close=document.createElement('button');close.textContent='Close';close.addEventListener('click',()=>dialog.close());const image=document.createElement('img');image.src=a.href;image.alt=a.querySelector('img')?.alt||'';dialog.append(close,image);document.body.append(dialog);dialog.addEventListener('close',()=>{dialog.remove();a.focus()});dialog.showModal()});
document.documentElement.dataset.archiveReady='true';
})();
