# Natru la Fisio — Portafolio V4.0 optimizado

**Proyecto estático** en HTML + CSS + JavaScript, preparado para GitHub Pages. La identidad visual se basa en los logotipos y fotografías auténticos de Natalia del ZIP facilitado. No se han generado ni sustituido rostros.

## Cómo ejecutarlo

Descomprime el ZIP y abre `index.html` en Chrome o Edge. Para simular una publicación web puedes iniciar un servidor estático en la carpeta del proyecto con `python -m http.server 8000` y abrir `http://localhost:8000` en tu propio equipo.

## Estructura

- `index.html`: estructura accesible, contenido profesional, enlaces y SEO.
- `css/styles.css`: **una sola hoja** con tokens de marca, componentes y responsive.
- `js/main.js`: menú móvil, indicador de sección activa y progreso de lectura.
- `assets/brand/`: logotipo horizontal, versión principal y monocromática, favicon y vista previa para compartir.
- `assets/images/`: fotografía real recortada de Natalia, variantes WebP y evidencias.
- `assets/docs/CV_Leydy_Natalia_Parra_ATS.pdf`: propuesta actualizada del CV, dos páginas, seleccionable y con enlaces clicables.
- `assets/videos/`: tres videos originales; carga diferida hasta su reproducción.
- `reference/`: PNG exacto de la portada V3.4 y el CV original aportado, para comparar y restaurar si fuera necesario.
- `preview/`: capturas de pruebas de escritorio y móvil, y `audit.json` con verificaciones.
- `sitemap.xml`: ruta prevista para la publicación en la ubicación actual del portafolio.

## Cambios implementados

1. Reemplazada la captura PNG de escritorio y sus enlaces invisibles por **una única portada semántica** compartida entre ordenador, tableta y móvil.
2. Preservado el esquema visual aprobado: logotipo tipográfico, retrato auténtico, fondo rosa, línea curva y ondas de ciruela, rosa y verde salvia.
3. Texto del encabezado y CTAs editables; enlaces reales y foco de teclado visible.
4. Corregida la jerarquía de los datos destacados conforme a la información documentada: titulación 2026, participación con 100 bebés prematuros, Askathon 2025 y cuatro prácticas.
5. Reorganizados los estilos para eliminar capas anteriores y duplicidades. No hay frameworks ni backend innecesario.
6. Imágenes WebP responsivas, dimensiones explícitas, carga diferida de galería y videos con `preload="none"`.
7. Preparado un CV ATS a partir del contenido original, sin atribuir una especialización formal. **Conservar el original y pedir validación de Natalia antes de publicar la propuesta actualizada.**
8. Eliminadas de la web afirmaciones sin respaldo en el CV aportado (tarjeta profesional, ACEFIT y dos cursos antiguos). Se pueden recuperar si se aportan certificados.
9. Se ha retirado de la galería pública inicial la fotografía pediátrica con un menor. Está conservada como recurso para decidir su eventual publicación con autorización.
10. Añadido SEO social y canonical al dominio y ruta actuales previstos en GitHub Pages.

## Validaciones que aún requieren intervención humana

- **Verificar cada dato y fecha** del CV ATS con Natalia; especialmente logros, cursos, niveles lingüísticos y el estado de su acreditación profesional.
- **Autorización de fotografías y videos** donde aparezcan pacientes o terceros, y de otros datos personales. En especial, no publicar imágenes de menores sin autorización.
- Los resultados de las pruebas locales con Chromium confirman maquetación y enlaces, pero **no equivalen a una auditoría Lighthouse real ni a una certificación WCAG 2.2 AA**. Se recomiendan Lighthouse, axe, teclado y lector de pantalla sobre la URL publicada.
- Los botones `download` descargan el CV propuesto. Si Natalia no aprueba el PDF, sustituir por el original o por la versión aprobada, dejando el nombre de archivo correcto.
- Antes de publicar en otra dirección, actualizar `canonical`, `og:image`, `sitemap.xml` y los enlaces del CV.

## Publicación

Sube los archivos del proyecto **sin la carpeta `preview` ni `reference` si deseas reducir la distribución**, conservando la estructura de `assets`, `css`, `js`, `index.html` y `sitemap.xml`, dentro de `Websites/PortafolioFisioNatalia/site/` del repositorio de GitHub Pages. No se ha efectuado ninguna modificación en GitHub remoto.


## Ajuste V4.1

- Se sustituyó el retrato recortado del encabezado por la versión con bordes pulidos aprobada por el usuario.
- Archivos actualizados: `assets/images/natalia-retrato-recortado-480.webp` y `assets/images/natalia-retrato-recortado-800.webp`.
- La estructura del portafolio y el resto del contenido se mantienen sin cambios respecto a la V4.0.


## Actualización V4.2 — navegación y botones

Se corrigió la selección activa del menú. El sistema anterior utilizaba las entradas de `IntersectionObserver` para decidir qué apartado resaltar; en determinados saltos de ancla podía permanecer seleccionado «Sobre mí» aunque la página estuviera situada en «Experiencia».

**Correcciones:**
- El enlace activo se calcula ahora a partir de la posición real de las seis secciones en pantalla, tanto al navegar con botones como al desplazarse manualmente.
- El enlace marcado recibe `aria-current="location"` y los demás pierden ese atributo.
- La selección se actualiza al pulsar enlaces del menú, «Ver mi experiencia», «Contactar a Natalia» o volver al inicio.
- Se eliminó la doble separación del desplazamiento (`scroll-padding-top` más `scroll-margin-top`), para que los encabezados queden correctamente alineados debajo del menú fijo.
- Se mantuvieron la fotografía original, el logo, la paleta, el diseño de la portada y el resto del contenido.

**Pruebas:** 320, 390, 820, 1024 y 1440 px; clic en cada una de las seis secciones en cada tamaño (30 comprobaciones), desplazamiento manual, CTA de experiencia, retorno al inicio, menú móvil con cierre por Escape, reapertura del menú, panel de videos y enlaces/archivos locales. Sin errores JavaScript ni desbordamiento horizontal en estas comprobaciones.

**Límites de las pruebas:** se representó el HTML, CSS y JS con recursos locales en Chromium por restricciones del entorno; los destinos de WhatsApp, Instagram, LinkedIn y correo tienen URLs/formato válido, pero su disponibilidad externa y la descarga a través de GitHub Pages no pudieron comprobarse en una publicación pública nueva.
