# Banner panoramico de Inicio

Revision del 16 de octubre de 2026. El usuario solicita convertir la imagen del nino en un banner integrado a todo el ancho, en lugar de mostrar una imagen centrada con fondos diferentes a los lados.

## Archivos y presentacion

- Referencia conservada sin cambios: `flynns-arcade-neon/assets/images/hero-arcade-kid.png`.
- PNG maestro de la nueva adaptacion: `docs/assets/hero-arcade-banner-original.png`.
- Banner utilizado por la web: `flynns-arcade-neon/assets/images/hero-arcade-banner.webp`.
- Preparacion del WebP: `tools/prepare-arcade-banner.cjs`, con la ruta del PNG maestro como argumento.

Se ha utilizado la herramienta integrada `image_gen` en modo edicion con el archivo original como referencia. No se ha utilizado la API ni una CLI externa. El prompt solicita un formato 5:1; la herramienta devuelve una adaptacion de 2172 x 724 px (3:1). Se integra este resultado completo, sin recortarlo para forzar otra proporcion. El nuevo archivo es una adaptacion generada, no una copia pixel por pixel del original.

El banner ocupa todo el ancho disponible, sin margenes, esquinas redondeadas ni paneles laterales. La altura es proporcional a la ilustracion; no se usa `object-fit: cover` ni una altura fija que corte la escena. El texto del juego permanece en la seccion inferior. El tema WordPress y las paginas HTML comparten esta misma plantilla.

## Prompt utilizado

EDIT TARGET: the supplied local image of the child in the neon arcade. Use case: stylized-concept / website banner outpainting. Convert this illustration into a finished extra-wide panoramic website banner, 5:1 aspect ratio, ideally 2500 x 500 pixels. Extend the canvas LEFT and RIGHT ONLY. Preserve the COMPLETE existing composition in the central portion, with the same child/backpack silhouette, arcade cabinets, seats, large city window, palm trees, pink sunset and existing neon sign. Do not zoom in, do not cut off any of the original top or bottom, do not stretch the child, and do not put the original image in a framed rectangle. Continue the arcade's architecture, additional machines and subtle wall details naturally into the new lateral space, with a coherent perspective and the same crisp pixel-art cluster size. The result must be ONE continuous seamless scene across the entire canvas, edge to edge; not a collage, not reflected or repeated side panels, no black padding, no vignette, no blur. Palette: near-black #030811, luminous cyan #00deee and hot magenta #f51ca5, matching the supplied artwork. Keep the child and main city view as the focal point; lateral additions should be secondary. No new people, no additional text, no new logos, no website UI, no border, no overlaid headline. Deliver only the complete extra-wide 5:1 landscape banner artwork.
