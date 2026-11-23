'use strict';
(() => {
 const menuButton = document.querySelector('.menu-toggle');
 const nav = document.querySelector('.main-nav');
 const links = [...document.querySelectorAll('.main-nav a[href^="#"]')];
 const progress = document.querySelector('.scroll-progress');
 const year = document.querySelector('#year');
 if (year) year.textContent = String(new Date().getFullYear());
 const closeMenu = (focusButton=false) => {
   if (!menuButton || !nav) return;
   nav.classList.remove('open');
   menuButton.setAttribute('aria-expanded', 'false');
   menuButton.setAttribute('aria-label', 'Abrir menú');
   if (focusButton) menuButton.focus();
 };
 if (menuButton && nav) {
   menuButton.addEventListener('click', () => {
     const opening = menuButton.getAttribute('aria-expanded') !== 'true';
     nav.classList.toggle('open', opening);
     menuButton.setAttribute('aria-expanded', String(opening));
     menuButton.setAttribute('aria-label', opening ? 'Cerrar menú' : 'Abrir menú');
   });
   links.forEach(a => a.addEventListener('click', () => closeMenu()));
   nav.querySelectorAll('a[href$=".pdf"]').forEach(a => a.addEventListener('click', () => closeMenu()));
   document.addEventListener('keydown', event => {
     if (event.key === 'Escape' && nav.classList.contains('open')) closeMenu(true);
   });
   document.addEventListener('pointerdown', event => {
     if (nav.classList.contains('open') && !nav.contains(event.target) && !menuButton.contains(event.target)) closeMenu();
   });
   window.matchMedia('(min-width:1021px)').addEventListener('change', e => { if (e.matches) closeMenu(); });
 }
 let queued=false;
 const updateProgress=()=>{
  const maximum=document.documentElement.scrollHeight-window.innerHeight;
  if (progress) progress.style.transform = `scaleX(${maximum > 0 ? Math.max(0,Math.min(1,window.scrollY/maximum)) : 0})`;
  queued=false;
 };
 window.addEventListener('scroll',()=>{if(!queued){queued=true;requestAnimationFrame(updateProgress)}},{passive:true});
 window.addEventListener('resize',updateProgress,{passive:true});
 updateProgress();
 // Navigation: derive the active link from the real scroll position rather
 // than from individual IntersectionObserver change events. This also works
 // when a section is taller than the viewport or scrolling skips a section.
 const navSections = links
   .map(link => ({ link, section: document.getElementById(link.hash.slice(1)) }))
   .filter(item => item.section);
 const setActiveSection = id => {
   navSections.forEach(({link, section}) => {
     const active = section.id === id;
     link.classList.toggle('active', active);
     if (active) link.setAttribute('aria-current', 'location');
     else link.removeAttribute('aria-current');
   });
 };
 const updateActiveSection = () => {
   const header = document.querySelector('.site-header');
   const headerHeight = header ? header.getBoundingClientRect().height : 0;
   // The reference point is just below the sticky header. Adding a small
   // viewport-relative margin makes the active section match the reader's view.
   const marker = headerHeight + Math.min(90, window.innerHeight * 0.12);
   let current = null;
   navSections.forEach(({section}) => {
     if (section.getBoundingClientRect().top <= marker) current = section.id;
   });
   // Always highlight Contacto when the user reaches the document bottom.
   const atBottom = window.scrollY > 0 &&
     window.innerHeight + window.scrollY >= document.documentElement.scrollHeight - 4;
   if (atBottom && navSections.length) current = navSections[navSections.length - 1].section.id;
   setActiveSection(current);
 };
 let sectionTick = false;
 const queueSectionUpdate = () => {
   if (sectionTick) return;
   sectionTick = true;
   requestAnimationFrame(() => {
     sectionTick = false;
     updateActiveSection();
   });
 };
 // Show the requested section as selected immediately after a click, even
 // during the smooth-scroll animation. Scroll tracking then keeps it accurate.
 document.querySelectorAll('a[href^="#"]').forEach(link => {
   const id = link.getAttribute('href').slice(1);
   if (id === 'inicio') {
     link.addEventListener('click', () => setActiveSection(null));
   } else if (navSections.some(({section}) => section.id === id)) {
     link.addEventListener('click', () => setActiveSection(id));
   }
 });
 window.addEventListener('scroll', queueSectionUpdate, {passive: true});
 window.addEventListener('resize', queueSectionUpdate, {passive: true});
 window.addEventListener('hashchange', queueSectionUpdate);
 window.addEventListener('pageshow', queueSectionUpdate);
 updateActiveSection();
 // Delayed video download: the video sources remain in the HTML with preload="none".
 document.querySelectorAll('.videos-panel').forEach(panel=>{
  panel.addEventListener('toggle',()=>{
   if(!panel.open){panel.querySelectorAll('video').forEach(v=>{if(!v.paused) v.pause()})}
  });
 });
})();
