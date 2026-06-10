const year = document.getElementById("year");

if (year) {
  year.textContent = new Date().getFullYear();
}

const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

if (!reducedMotion) {
  initBackgroundCanvas();
}

function initBackgroundCanvas() {
  const canvas = document.getElementById("projectCanvas");
  if (!canvas) return;

  const ctx = canvas.getContext("2d");

  const symbols = [
    "🐓",
    "★",
    "✦",
    "POW!",
    "SMACK!",
    "KO!",
    "1P",
    "2P",
    "VS",
    "!",
    "●"
  ];

  const resize = () => {
    canvas.width = innerWidth;
    canvas.height = innerHeight;
  };

  resize();
  addEventListener("resize", resize);

  const particles = Array.from({ length: 75 }, () => ({
    x: Math.random() * canvas.width,
    y: Math.random() * canvas.height,
    vx: (Math.random() - 0.5) * 0.6,
    vy: Math.random() * -0.35 - 0.05,
    size: Math.random() * 26 + 14,
    text: symbols[Math.floor(Math.random() * symbols.length)],
    opacity: Math.random() * 0.45 + 0.15,
    color: Math.random() > 0.5 ? "#ffcc33" : "#ff5a2f"
  }));

  function draw() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";

    particles.forEach((p) => {
      ctx.globalAlpha = p.opacity;
      ctx.font = `bold ${p.size}px Inter, monospace`;
      ctx.fillStyle = p.color;
      ctx.shadowColor = p.color;
      ctx.shadowBlur = 18;
      ctx.fillText(p.text, p.x, p.y);

      p.x += p.vx;
      p.y += p.vy;

      if (p.y < -40) p.y = canvas.height + 40;
      if (p.x < -80) p.x = canvas.width + 80;
      if (p.x > canvas.width + 80) p.x = -80;
    });

    ctx.globalAlpha = 1;
    requestAnimationFrame(draw);
  }

  draw();
}