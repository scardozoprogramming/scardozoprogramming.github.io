(() => {
  const carousel = document.querySelector("[data-carousel]");
  if (!carousel) return;

  const ANIMATION_DURATION = 520;

  const previousCard = carousel.querySelector(".carousel-card.side:first-child");
  const nextCard = carousel.querySelector(".carousel-card.side:last-child");
  const prefersReducedMotion = window.matchMedia(
    "(prefers-reduced-motion: reduce)"
  ).matches;

  let isAnimating = false;

  function navigateToProject(card, direction) {
    if (!card || isAnimating) return;

    const url = card.dataset.url;
    if (!url) return;

    isAnimating = true;

    if (prefersReducedMotion) {
      window.location.assign(url);
      return;
    }

    carousel.classList.remove("go-left", "go-right");

    // Reinicia la animación si el usuario intenta repetirla rápido.
    void carousel.offsetWidth;

    carousel.classList.add(direction === "left" ? "go-left" : "go-right");

    window.setTimeout(() => {
      window.location.assign(url);
    }, ANIMATION_DURATION);
  }

  function isTypingElement(element) {
    return (
      element instanceof HTMLInputElement ||
      element instanceof HTMLTextAreaElement ||
      element instanceof HTMLSelectElement ||
      element?.isContentEditable
    );
  }

  previousCard?.addEventListener("click", () => {
    navigateToProject(previousCard, "right");
  });

  nextCard?.addEventListener("click", () => {
    navigateToProject(nextCard, "left");
  });

  window.addEventListener("keydown", (event) => {
    if (event.defaultPrevented || isTypingElement(event.target)) return;

    if (event.key === "ArrowLeft") {
      event.preventDefault();
      navigateToProject(previousCard, "right");
    }

    if (event.key === "ArrowRight") {
      event.preventDefault();
      navigateToProject(nextCard, "left");
    }
  });
})();