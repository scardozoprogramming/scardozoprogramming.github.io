const navToggle = document.querySelector(".nav-toggle");
const mainNav = document.querySelector(".main-nav");
const navLinks = [...document.querySelectorAll(".main-nav a")];
const focusTabs = [...document.querySelectorAll(".focus-tab")];
const focusPanels = [...document.querySelectorAll(".focus-panel")];
const year = document.querySelector("#year");

if (year) {
  year.textContent = new Date().getFullYear();
}

navToggle?.addEventListener("click", () => {
  const isOpen = navToggle.getAttribute("aria-expanded") === "true";
  navToggle.setAttribute("aria-expanded", String(!isOpen));
  mainNav?.classList.toggle("is-open", !isOpen);
});

navLinks.forEach((link) => {
  link.addEventListener("click", () => {
    navToggle?.setAttribute("aria-expanded", "false");
    mainNav?.classList.remove("is-open");
  });
});

focusTabs.forEach((tab) => {
  tab.addEventListener("click", () => {
    focusTabs.forEach((item) => item.setAttribute("aria-selected", String(item === tab)));
    focusPanels.forEach((panel) => {
      const isSelected = panel.id === tab.getAttribute("aria-controls");
      panel.hidden = !isSelected;
      panel.classList.toggle("is-active", isSelected);
    });
  });
});

const sections = navLinks
  .map((link) => document.querySelector(link.getAttribute("href")))
  .filter(Boolean);

if ("IntersectionObserver" in window && sections.length) {
  const observer = new IntersectionObserver(
    (entries) => {
      entries.forEach((entry) => {
        if (!entry.isIntersecting) return;
        navLinks.forEach((link) => {
          link.classList.toggle("is-active", link.getAttribute("href") === `#${entry.target.id}`);
        });
      });
    },
    { rootMargin: "-35% 0px -55% 0px", threshold: 0 }
  );

  sections.forEach((section) => observer.observe(section));
}
