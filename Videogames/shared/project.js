(() => {
  updateCurrentYear();

  const projectKey = document.body?.dataset.project;
  const particles = window.portfolioProjectData?.[projectKey]?.particles;
  const prefersReducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  if (particles && !prefersReducedMotion) {
    initBackgroundCanvas(particles);
  }

  function updateCurrentYear() {
    const year = document.getElementById("year");
    if (year) {
      year.textContent = new Date().getFullYear();
    }
  }

  function initBackgroundCanvas(config) {
    const canvas = document.getElementById(config.canvasId || "projectCanvas");
    if (!canvas) return;

    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    const symbols = config.symbols || [];
    const particleCount = config.count || 72;
    const particleList = Array.from({ length: particleCount }, createParticle);

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

    function pickColor() {
      const colors = config.colors || ["#00e676", "#00bcd4"];
      const firstColorChance = config.firstColorChance ?? 0.5;
      return Math.random() < firstColorChance ? colors[0] : colors[1];
    }

    function createParticle() {
      return {
        x: Math.random() * window.innerWidth,
        y: Math.random() * window.innerHeight,
        vx: (Math.random() - 0.5) * (config.vx ?? 0.5),
        vy: Math.random() * (config.vyBase ?? -0.35) + (config.vyOffset ?? -0.05),
        size: Math.random() * (config.sizeRange ?? 24) + (config.sizeBase ?? 14),
        text: symbols[Math.floor(Math.random() * symbols.length)] || "",
        opacity: Math.random() * (config.opacityRange ?? 0.42) + (config.opacityBase ?? 0.12),
        color: pickColor()
      };
    }

    function draw() {
      const width = window.innerWidth;
      const height = window.innerHeight;
      const wrapX = config.wrapX ?? 80;

      ctx.clearRect(0, 0, width, height);
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";

      particleList.forEach((particle) => {
        ctx.globalAlpha = particle.opacity;
        ctx.font = `bold ${particle.size}px Inter, monospace`;
        ctx.fillStyle = particle.color;
        ctx.shadowColor = particle.color;
        ctx.shadowBlur = config.shadowBlur ?? 18;
        ctx.fillText(particle.text, particle.x, particle.y);

        particle.x += particle.vx;
        particle.y += particle.vy;

        if (particle.y < -40) particle.y = height + 40;
        if (particle.x < -wrapX) particle.x = width + wrapX;
        if (particle.x > width + wrapX) particle.x = -wrapX;
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
