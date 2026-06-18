(() => {
  updateCurrentYear();

  const prefersReducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  if (!prefersReducedMotion) {
    initBackgroundCanvas();
  }

  function updateCurrentYear() {
    const year = document.getElementById("year");
    if (year) {
      year.textContent = new Date().getFullYear();
    }
  }

  function initBackgroundCanvas() {
    const canvas = document.getElementById("projectCanvas");
    if (!canvas) return;

    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    const symbols = ["0", "1", "OSS", "MASK", "SYS", "RUN", ">_", "PROTOCOL"];
    const particles = Array.from({ length: 82 }, createParticle);

    function resizeCanvas() {
      const ratio = Math.min(window.devicePixelRatio || 1, 2);
      const width = window.innerWidth;
      const height = window.innerHeight;

      canvas.width = Math.floor(width * ratio);
      canvas.height = Math.floor(height * ratio);
      canvas.style.width = `${width}px`;
      canvas.style.height = `${height}px`;
      ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    }

    function createParticle() {
      return {
        x: Math.random() * window.innerWidth,
        y: Math.random() * window.innerHeight,
        vx: (Math.random() - 0.5) * 0.58,
        vy: Math.random() * -0.24 - 0.04,
        size: Math.random() * 20 + 12,
        text: symbols[Math.floor(Math.random() * symbols.length)],
        opacity: Math.random() * 0.45 + 0.12,
        color: Math.random() > 0.6 ? "#00e676" : "#ff5a1f"
      };
    }

    function draw() {
      ctx.clearRect(0, 0, window.innerWidth, window.innerHeight);
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";

      particles.forEach((particle) => {
        ctx.globalAlpha = particle.opacity;
        ctx.font = `bold ${particle.size}px Inter, monospace`;
        ctx.fillStyle = particle.color;
        ctx.shadowColor = particle.color;
        ctx.shadowBlur = 16;
        ctx.fillText(particle.text, particle.x, particle.y);

        particle.x += particle.vx;
        particle.y += particle.vy;

        if (particle.y < -40) particle.y = window.innerHeight + 40;
        if (particle.x < -100) particle.x = window.innerWidth + 100;
        if (particle.x > window.innerWidth + 100) particle.x = -100;
      });

      ctx.globalAlpha = 1;
      ctx.shadowBlur = 0;
      requestAnimationFrame(draw);
    }

    resizeCanvas();
    window.addEventListener("resize", resizeCanvas);
    draw();
  }
})();
