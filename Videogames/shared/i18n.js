(() => {
  const projectKey = document.body?.dataset.project;
  const projectData = window.portfolioProjectData?.[projectKey];
  const translations = projectData?.translations;

  if (!translations) return;

  const DEFAULT_LANGUAGE = "en";
  const STORAGE_KEY = "language";
  const LANGUAGE_VERSION_KEY = "languageDefaultVersion";
  const LANGUAGE_VERSION = "english-default-v2";

  if (localStorage.getItem(LANGUAGE_VERSION_KEY) !== LANGUAGE_VERSION) {
    localStorage.setItem(STORAGE_KEY, DEFAULT_LANGUAGE);
    localStorage.setItem(LANGUAGE_VERSION_KEY, LANGUAGE_VERSION);
  }

  let currentLang = translations[localStorage.getItem(STORAGE_KEY)]
    ? localStorage.getItem(STORAGE_KEY)
    : DEFAULT_LANGUAGE;

  function getDictionary() {
    return translations[currentLang] || translations[DEFAULT_LANGUAGE];
  }

  function applyTranslations() {
    const dictionary = getDictionary();
    document.documentElement.lang = currentLang;

    document.querySelectorAll("[data-i18n]").forEach((element) => {
      const key = element.dataset.i18n;
      if (Object.prototype.hasOwnProperty.call(dictionary, key)) {
        element.textContent = dictionary[key];
      }
    });

    document.querySelectorAll("[data-i18n-content]").forEach((element) => {
      const key = element.dataset.i18nContent;
      if (Object.prototype.hasOwnProperty.call(dictionary, key)) {
        element.setAttribute("content", dictionary[key]);
      }
    });

    if (dictionary.title) {
      document.title = dictionary.title;
    }

    const langButton = document.getElementById("langToggle");
    if (langButton) {
      langButton.textContent = currentLang === "en" ? "Español" : "English";
      langButton.setAttribute(
        "aria-label",
        currentLang === "en" ? "Change language to Spanish" : "Cambiar idioma a inglés"
      );
    }
  }

  function toggleLanguage() {
    currentLang = currentLang === "en" ? "es" : "en";
    localStorage.setItem(STORAGE_KEY, currentLang);
    applyTranslations();
  }

  document.addEventListener("DOMContentLoaded", () => {
    applyTranslations();
    document.getElementById("langToggle")?.addEventListener("click", toggleLanguage);
  });
})();
