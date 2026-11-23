# Validación de navegación y enlaces — Natru V4.2

- **Problema resuelto:** al pulsar «Experiencia», el indicador podía seguir marcado en «Sobre mí».
- **Causa 1:** el observador anterior procesaba solo las secciones que cambiaban su estado de intersección; no comparaba todas las secciones con la posición actual.
- **Causa 2:** se sumaban `scroll-padding-top:105px` y `scroll-margin-top:104px`, produciendo un desplazamiento superior duplicado.
- **Solución:** seguimiento mediante posición de desplazamiento, `requestAnimationFrame`, `aria-current=location`, actualización inmediata al pulsar el enlace y un único desplazamiento superior.

## Pruebas en Chromium

| Caso | Resultado |
|---|---|
| Las 6 secciones se activan correctamente en 320, 390, 820, 1024 y 1440 px (30 casos) | Correcto |
| Salto de la portada a «Ver mi experiencia» | Correcto |
| Volver al inicio desde el pie de página | Correcto |
| Desplazarse manualmente a Experiencia y detectar la sección | Correcto |
| Abrir y cerrar el menú móvil; tecla Escape | Correcto |
| Reabrir el menú móvil y conservar «Experiencia» seleccionada con `aria-current` | Correcto |
| Pulsar «Contactar a Natalia» y activar Contacto | Correcto |
| Abrir y cerrar el panel de los tres videos | Correcto |
| Anclas y archivos internos de los 22 enlaces del HTML | No se detectaron destinos locales faltantes |
| Archivo PDF de CV | Se abre; 2 páginas y texto seleccionable |
| Tres videos MP4 | Contienen pista de video H.264 válida |
| Desbordamiento horizontal en los 5 tamaños ensayados | No detectado |
| Errores JavaScript durante las pruebas | No detectados |

Las pruebas de interacción se hicieron con el HTML, CSS y JS en un navegador Chromium con los recursos locales, no sobre una nueva URL publicada. También se comprobó la navegación suave en móvil tras finalizar el desplazamiento. Los servicios externos y la disponibilidad de la publicación en GitHub Pages no están incluidos en estas comprobaciones.
