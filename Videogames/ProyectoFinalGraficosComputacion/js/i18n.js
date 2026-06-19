(() => {
  const translations = {
    en: {
      title: "Final Project — Computer Graphics | Portfolio Project",
      meta_description:
        "Computer graphics portfolio project exploring lighting, shading models, fog and interactive scene presentation.",

      skip_link: "Skip to content",
      role_label: "Game Programmer",
      back_link: "← Back",
      demo_link: "Demo",
      gallery_link: "Gallery",

      hero_badge: "Computer Graphics Project",
      subtitle:
        "Interactive 3D scene exploring advanced lighting, atmospheric effects and multiple shading models.",

      hero_cta_demo: "Open Demo",
      hero_cta_technical: "Technical Details",

      hero_stat1_p: "Interactive scene",
      hero_stat2_p: "Implementation",
      hero_stat3_p: "Main focus",

      info_kicker: "Quick read",
      info_h2: "Project Info",
      info1_h: "Role",
      info1_p: "Graphics Programmer",
      info2_h: "Tech",
      info2_p: "JavaScript / WebGL / GLSL",
      info3_h: "Type",
      info3_p: "Academic technical prototype",
      info4_h: "Focus",
      info4_p: "Lighting, shading and fog",

      preview_h2: "Scene Preview",
      preview_caption:
        "Initial scene view used to frame the lighting, shading and atmosphere comparisons.",

      demo_kicker: "Interactive Demo",
      demo_h2: "Open the Computer Graphics Scene",
      demo_p:
        "The original interactive project remains available as a separate page, preserving the scene controls and rendering behavior.",
      demo_button: "Open Interactive Demo",

      overview_h2: "Project Overview",
      overview_p:
        "This academic computer graphics project presents a small 3D diorama scene with advanced lighting, fog and multiple shading models. The page highlights the rendering concepts explored in the final project.",

      overview_fact1_h: "Lighting",
      overview_fact1_p: "Core technical topic",
      overview_fact2_h: "Shading",
      overview_fact2_p: "Main comparison",

      proof_kicker: "Portfolio evidence",
      proof_h2: "Technical Proof",
      proof_p:
        "This project works best as a technical artifact, so the page makes the rendering topics and interactive demo explicit.",
      proof1_h: "Interactive Demo",
      proof1_p:
        "The demo link preserves the original scene where evaluators can inspect rendering modes and controls.",
      proof2_h: "Rendering Comparisons",
      proof2_p:
        "The page identifies the lighting and shading comparisons: Gouraud, Phong, Blinn-Phong, flat, smooth and toon.",
      proof3_h: "Visual Evidence",
      proof3_p:
        "GIF captures show the project in motion without requiring the viewer to open the full demo first.",

      render_kicker: "Rendering concepts",
      render_h2: "Rendering Comparisons",
      render_p:
        "This section makes the graphics project easier to evaluate by naming each visual comparison explicitly.",
      render1_h: "Gouraud vs Phong",
      render1_p:
        "Compares vertex-based and fragment-based shading to show differences in highlight smoothness and precision.",
      render2_h: "Phong vs Blinn-Phong",
      render2_p:
        "Highlights how specular response changes between classic Phong and Blinn-Phong lighting.",
      render3_h: "Fog and Spotlight",
      render3_p:
        "Shows how atmospheric depth and spotlight attenuation affect scene readability and mood.",
      render4_h: "Flat, Smooth and Toon",
      render4_p:
        "Demonstrates how different surface styles change the same 3D scene's visual language.",

      role_kicker: "My contribution",
      role_h2: "My Role in the Project",
      role_p:
        "I implemented and presented the graphics features, focusing on lighting comparisons, shading models, atmospheric effects and clear interactive controls for evaluation.",

      contribution1_h: "Lighting Models",
      contribution1_p:
        "Explored spotlight behavior, attenuation and lighting setup inside the scene.",
      contribution2_h: "Shading Comparison",
      contribution2_p:
        "Compared Gouraud, Phong, Blinn-Phong, flat, smooth and toon shading modes.",
      contribution3_h: "Atmosphere",
      contribution3_p:
        "Integrated fog and presentation elements to improve scene readability and mood.",
      contribution4_h: "Interaction",
      contribution4_p:
        "Created keyboard controls that let viewers switch rendering modes and projection settings.",

      technical_kicker: "Technical Details",
      technical_h2: "Technical Implementation",

      tech_lighting_h2: "Lighting",
      tech_lighting_li1: "Spotlight attenuation and light direction control.",
      tech_lighting_li2: "Scene setup for comparing illumination behavior.",
      tech_lighting_li3: "Lighting choices connected to atmosphere and readability.",

      tech_shading_h2: "Shading Models",
      tech_shading_li1: "Gouraud and Phong comparison.",
      tech_shading_li2: "Blinn-Phong and classic Phong comparison.",
      tech_shading_li3: "Flat, smooth and toon presentation modes.",

      tech_scene_h2: "Scene Presentation",
      tech_scene_li1: "Fog and atmosphere controls.",
      tech_scene_li2: "Projection switching for inspection.",
      tech_scene_li3: "GIF captures prepared to show interaction and visual states.",

      gallery_h2: "Interactive Demonstrations",
      gallery_caption1:
        "Lighting and shading changes shown in motion for easier visual comparison.",
      gallery_caption2:
        "Toon, fog and surface-style comparisons captured from the interactive scene.",
      controls_h2: "Controls",
      tools_h2: "Tools Used"
    },

    es: {
      title: "Proyecto Final — Gráficos por Computación | Proyecto de Portafolio",
      meta_description:
        "Proyecto de gráficos por computación que explora iluminación, modelos de sombreado, niebla y presentación de escena interactiva.",

      skip_link: "Saltar al contenido",
      role_label: "Programador de Videojuegos",
      back_link: "← Volver",
      demo_link: "Demo",
      gallery_link: "Galería",

      hero_badge: "Proyecto de Gráficos por Computación",
      subtitle:
        "Escena 3D interactiva que explora iluminación avanzada, efectos atmosféricos y múltiples modelos de sombreado.",

      hero_cta_demo: "Abrir Demo",
      hero_cta_technical: "Detalles Técnicos",

      hero_stat1_p: "Escena interactiva",
      hero_stat2_p: "Implementación",
      hero_stat3_p: "Enfoque principal",

      info_kicker: "Lectura rápida",
      info_h2: "Ficha del Proyecto",
      info1_h: "Rol",
      info1_p: "Programador de Gráficos",
      info2_h: "Tecnología",
      info2_p: "JavaScript / WebGL / GLSL",
      info3_h: "Tipo",
      info3_p: "Prototipo técnico académico",
      info4_h: "Enfoque",
      info4_p: "Iluminación, sombreado y niebla",

      preview_h2: "Vista Previa de la Escena",
      preview_caption:
        "Vista inicial de la escena usada para contextualizar comparaciones de iluminación, sombreado y atmósfera.",

      demo_kicker: "Demo Interactiva",
      demo_h2: "Abrir la Escena de Gráficos",
      demo_p:
        "El proyecto interactivo original sigue disponible como una página separada, conservando los controles y el comportamiento de renderizado.",
      demo_button: "Abrir Demo Interactiva",

      overview_h2: "Resumen del Proyecto",
      overview_p:
        "Este proyecto académico de gráficos por computación presenta un pequeño diorama 3D con iluminación avanzada, niebla y múltiples modelos de sombreado. La página destaca los conceptos de renderizado explorados en el proyecto final.",

      overview_fact1_h: "Iluminación",
      overview_fact1_p: "Tema técnico central",
      overview_fact2_h: "Sombreado",
      overview_fact2_p: "Comparación principal",

      proof_kicker: "Evidencia de portafolio",
      proof_h2: "Prueba Técnica",
      proof_p:
        "Este proyecto funciona mejor como artefacto técnico, así que la página hace explícitos los temas de renderizado y la demo interactiva.",
      proof1_h: "Demo Interactiva",
      proof1_p:
        "El enlace a la demo conserva la escena original para inspeccionar modos de renderizado y controles.",
      proof2_h: "Comparaciones de Render",
      proof2_p:
        "La página identifica las comparaciones de iluminación y sombreado: Gouraud, Phong, Blinn-Phong, flat, smooth y toon.",
      proof3_h: "Evidencia Visual",
      proof3_p:
        "Los GIFs muestran el proyecto en movimiento sin obligar al visitante a abrir la demo completa primero.",

      render_kicker: "Conceptos de render",
      render_h2: "Comparaciones de Renderizado",
      render_p:
        "Esta sección hace que el proyecto de gráficos sea más fácil de evaluar al nombrar explícitamente cada comparación visual.",
      render1_h: "Gouraud vs Phong",
      render1_p:
        "Compara sombreado por vértice y por fragmento para mostrar diferencias en suavidad y precisión de brillos.",
      render2_h: "Phong vs Blinn-Phong",
      render2_p:
        "Destaca cómo cambia la respuesta especular entre Phong clásico y Blinn-Phong.",
      render3_h: "Niebla y Spotlight",
      render3_p:
        "Muestra cómo la profundidad atmosférica y la atenuación del spotlight afectan legibilidad y ambiente.",
      render4_h: "Flat, Smooth y Toon",
      render4_p:
        "Demuestra cómo distintos estilos de superficie cambian el lenguaje visual de la misma escena 3D.",

      role_kicker: "Mi contribución",
      role_h2: "Mi Rol en el Proyecto",
      role_p:
        "Implementé y presenté las características gráficas, enfocándome en comparaciones de iluminación, modelos de sombreado, efectos atmosféricos y controles interactivos claros para evaluación.",

      contribution1_h: "Modelos de Iluminación",
      contribution1_p:
        "Exploré comportamiento de spotlight, atenuación y configuración de luces dentro de la escena.",
      contribution2_h: "Comparación de Sombreado",
      contribution2_p:
        "Comparé modos Gouraud, Phong, Blinn-Phong, flat, smooth y toon.",
      contribution3_h: "Atmósfera",
      contribution3_p:
        "Integré niebla y elementos de presentación para mejorar legibilidad y ambiente.",
      contribution4_h: "Interacción",
      contribution4_p:
        "Creé controles de teclado para cambiar modos de renderizado y ajustes de proyección.",

      technical_kicker: "Detalles Técnicos",
      technical_h2: "Implementación Técnica",

      tech_lighting_h2: "Iluminación",
      tech_lighting_li1: "Atenuación de spotlight y control de dirección de luz.",
      tech_lighting_li2: "Configuración de escena para comparar comportamiento de iluminación.",
      tech_lighting_li3: "Decisiones de iluminación conectadas a atmósfera y legibilidad.",

      tech_shading_h2: "Modelos de Sombreado",
      tech_shading_li1: "Comparación entre Gouraud y Phong.",
      tech_shading_li2: "Comparación entre Blinn-Phong y Phong clásico.",
      tech_shading_li3: "Modos de presentación flat, smooth y toon.",

      tech_scene_h2: "Presentación de Escena",
      tech_scene_li1: "Controles de niebla y atmósfera.",
      tech_scene_li2: "Cambio de proyección para inspección.",
      tech_scene_li3: "GIFs preparados para mostrar interacción y estados visuales.",

      gallery_h2: "Demostraciones Interactivas",
      gallery_caption1:
        "Cambios de iluminación y sombreado mostrados en movimiento para una comparación visual más clara.",
      gallery_caption2:
        "Comparaciones de toon, niebla y estilos de superficie capturadas desde la escena interactiva.",
      controls_h2: "Controles",
      tools_h2: "Herramientas Usadas"
    }
  };

  const DEFAULT_LANGUAGE = "en";
  const STORAGE_KEY = "language";
  let currentLang = localStorage.getItem(STORAGE_KEY) || DEFAULT_LANGUAGE;

  function applyTranslations() {
    const dictionary = translations[currentLang] || translations[DEFAULT_LANGUAGE];
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

    document.title = dictionary.title;

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
