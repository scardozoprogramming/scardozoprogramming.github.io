'use strict';
(() => {
  const languageButton = document.getElementById('langToggle');
  const dictionary = JSON.parse(document.getElementById('i18n').textContent);
  let currentLang = 'es';
  try { currentLang = localStorage.getItem('scardozo_case_lang') === 'en' ? 'en' : 'es'; } catch (_) {}
  function translate() {
    document.documentElement.lang = currentLang;
    document.querySelectorAll('[data-i18n]').forEach(element => {
      const value = dictionary[currentLang][element.dataset.i18n];
      if (value !== undefined) element.textContent = value;
    });
    document.querySelectorAll('[data-i18n-aria]').forEach(element => {
      const value = dictionary[currentLang][element.dataset.i18nAria];
      if (value !== undefined) element.setAttribute('aria-label', value);
    });
    languageButton.textContent = currentLang === 'es' ? 'English' : 'Español';
    languageButton.setAttribute('aria-label', currentLang === 'es' ? 'Switch to English' : 'Cambiar idioma a español');
    document.title = dictionary[currentLang].metaTitle;
  }
  languageButton.addEventListener('click', () => {
    currentLang = currentLang === 'es' ? 'en' : 'es';
    try { localStorage.setItem('scardozo_case_lang', currentLang); } catch (_) {}
    translate();
  });
  translate();
  const year = document.getElementById('year');
  if (year) year.textContent = String(new Date().getFullYear());
  const demoFrame = document.getElementById('demoFrame');
  const demoPlaceholder = document.getElementById('demoPlaceholder');
  const demoWindow = document.getElementById('demoWindow');
  const demoLoad = document.getElementById('demoLoad');
  const demoButtons = document.querySelectorAll('[data-demo-mode]');
  demoButtons.forEach(button => button.addEventListener('click', () => {
    demoWindow.dataset.mode = button.dataset.demoMode;
    demoButtons.forEach(other => other.setAttribute('aria-pressed', String(other === button)));
  }));
  demoLoad.addEventListener('click', () => {
    if (demoFrame.src) return;
    demoFrame.src = 'https://scardozoprogramming.github.io/Websites/PortafolioFisioNatalia/index.html';
    demoFrame.hidden = false;
    demoPlaceholder.hidden = true;
    demoLoad.disabled = true;
    demoLoad.setAttribute('aria-disabled', 'true');
  });
})();
