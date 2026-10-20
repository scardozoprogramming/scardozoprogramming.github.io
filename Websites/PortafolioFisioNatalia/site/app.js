const navToggle = document.querySelector(".nav-toggle");
const mainNav = document.querySelector(".main-nav");
const navLinks = [...document.querySelectorAll(".main-nav a")];
const navLabel = navToggle?.querySelector(".sr-only");
const scrollProgress = document.querySelector(".scroll-progress");
const year = document.querySelector("#year");

if (year) year.textContent = new Date().getFullYear();

const closeMenu = () => {
  navToggle?.setAttribute("aria-expanded", "false");
  mainNav?.classList.remove("is-open");
  if (navLabel) navLabel.textContent = "Abrir menú";
};

navToggle?.addEventListener("click", () => {
  const open = navToggle.getAttribute("aria-expanded") === "true";
  navToggle.setAttribute("aria-expanded", String(!open));
  mainNav?.classList.toggle("is-open", !open);
  if (navLabel) navLabel.textContent = open ? "Abrir menú" : "Cerrar menú";
});

navLinks.forEach((link) => link.addEventListener("click", closeMenu));

document.addEventListener("keydown", (event) => {
  if (event.key === "Escape") closeMenu();
});

const updateScrollProgress = () => {
  if (!scrollProgress) return;
  const scrollable = document.documentElement.scrollHeight - window.innerHeight;
  const progress = scrollable > 0 ? Math.min(window.scrollY / scrollable, 1) : 0;
  scrollProgress.style.transform = `scaleX(${progress})`;
};

updateScrollProgress();
window.addEventListener("scroll", updateScrollProgress, { passive: true });
window.addEventListener("resize", updateScrollProgress);

const sections = navLinks
  .map((link) => document.querySelector(link.getAttribute("href")))
  .filter(Boolean);

if ("IntersectionObserver" in window) {
  const observer = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      if (!entry.isIntersecting) return;
      navLinks.forEach((link) => {
        link.classList.toggle("is-active", link.getAttribute("href") === `#${entry.target.id}`);
      });
    });
  }, { rootMargin: "-35% 0px -55% 0px" });
  sections.forEach((section) => observer.observe(section));
}

const revealItems = [...document.querySelectorAll(
  ".proof-ribbon>div,.profile-intro,.value-grid,.section-heading,.experience-list,.focus-grid,.education-grid,.credentials-heading,.achievements-layout,.evidence-grid,.media-details,.contact-section>div"
)];
const prefersReducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

if ("IntersectionObserver" in window && !prefersReducedMotion) {
  document.documentElement.classList.add("reveal-ready");
  revealItems.forEach((item) => item.classList.add("reveal-item"));

  const revealObserver = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      if (!entry.isIntersecting) return;
      entry.target.classList.add("is-visible");
      revealObserver.unobserve(entry.target);
    });
  }, { threshold: 0.12, rootMargin: "0px 0px -8% 0px" });

  revealItems.forEach((item) => revealObserver.observe(item));
}
