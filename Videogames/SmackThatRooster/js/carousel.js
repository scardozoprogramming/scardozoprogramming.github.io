const carousel = document.querySelector("[data-carousel]");

if (carousel) {
  const previousCard = carousel.querySelector(".carousel-card.side:first-child");
  const nextCard = carousel.querySelector(".carousel-card.side:last-child");

  const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  let isAnimating = false;

  function goToProject(card, direction) {
    if (!card || isAnimating) return;

    const url = card.dataset.url;
    if (!url) return;

    isAnimating = true;

    if (reducedMotion) {
      window.location.href = url;
      return;
    }

    carousel.classList.remove("go-left", "go-right");

    void carousel.offsetWidth;

    if (direction === "left") {
      carousel.classList.add("go-left");
    }

    if (direction === "right") {
      carousel.classList.add("go-right");
    }

    setTimeout(() => {
      window.location.href = url;
    }, 520);
  }

  previousCard?.addEventListener("click", () => {
    goToProject(previousCard, "right");
  });

  nextCard?.addEventListener("click", () => {
    goToProject(nextCard, "left");
  });

  window.addEventListener("keydown", event => {
    if (event.key === "ArrowLeft") {
      goToProject(previousCard, "right");
    }

    if (event.key === "ArrowRight") {
      goToProject(nextCard, "left");
    }
  });
}