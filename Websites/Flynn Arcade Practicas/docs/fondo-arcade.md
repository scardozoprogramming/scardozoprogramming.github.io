# Fondo arcade compartido

Ilustracion creada para este proyecto con la herramienta integrada de generacion de imagenes de Codex, no con una API o CLI externa. La imagen del nino de la portada no se modifica.

## Archivos

- Original: `docs/assets/arcade-city-original.png`.
- Fondo de escritorio: `flynns-arcade-neon/assets/images/backgrounds/arcade-city.webp`.
- Recorte movil: `flynns-arcade-neon/assets/images/backgrounds/arcade-city-mobile.webp`.

El fondo se incluye localmente en todas las paginas mediante una unica regla compartida. Las capas oscuras controlan el contraste. No hay animacion, dependencias de red ni copias de la imagen por pagina. Las tarjetas de juegos mantienen su fondo opaco y las fotografias conservan sus colores al ampliarse.

Las versiones WebP se optimizan con `tools/prepare-arcade-background.cjs`, pasando como argumento la ruta del PNG original. El recorte movil prioriza las maquinas del lateral derecho y la ciudad.

## Prompt final

Use case: stylized-concept. Asset type: a single wide 16:9 pixel-art environment wallpaper for the background of an independent videogame publisher website, NOT a webpage or mockup. Primary request: an atmospheric neon arcade interior overlooking a futuristic coastal city at night, in the aesthetic of a rich 1990s hand-pixelled adventure game. Scene: several clearly recognizable arcade cabinets in perspective along both far edges, cyan illuminated screens and thin hot-magenta edge lights, a large rear window showing a layered city skyline with cyan windows, pink horizon and a few palm silhouettes. Empty arcade, no people, no child. Composition: wide panoramic eye-level view, cabinets and architectural details strongest at the left and right edges; the middle 60 percent remains naturally dark, calm and low-detail so website text can sit over it. Draw real machines, windows, ceiling and floor, not abstract gradients. Crisp crafted pixel clusters and coherent perspective. Palette: near-black #030811 and #07111e dominate, electric cyan #00deee and magenta #f51ca5 as balanced controlled accents. Mood: inviting late-night arcade, atmospheric but not foggy or blurry. Constraints: no text, no letters, no readable cabinet marquees, no logos, no watermark, no interface, no buttons, no decorative glowing orbs, no characters. Preserve visible recognizable machines and city, avoid an overwhelmingly purple or orange palette. Output only the finished landscape illustration.
