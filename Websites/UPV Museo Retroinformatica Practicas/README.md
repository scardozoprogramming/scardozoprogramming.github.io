# Club de Retroinformática UPV — sitio web

Esta es la versión web unificada basada en el diseño oscuro y retro aprobado para continuar el proyecto.

## Cómo abrirlo

1. Descomprime el ZIP conservando la carpeta **`UPV Museo Retroinformatica Practicas`**.
2. Abre **`inicio.html`** (o `index.html`) en un navegador.
3. Recorre Inicio, Actividades, Qué es el club, Máquinas y Únete desde el menú.

No hace falta instalar paquetes ni ejecutar un servidor para ver el contenido básico. Para comprobar la integración con GitHub Pages, publica la carpeta en tu repositorio.

## Organización

- **`*.html`**: cinco páginas del sitio.
- `index.html`: portada completa sin redirección; se conserva también la página inicio.html.
- `assets/css/styles.css`: única hoja de estilos (base + adaptación visual aprobada).
- `assets/js/site.js`: navegación móvil, filtros, fichas informativas, formulario local.
- `assets/js/content.js`: textos de fichas de máquinas y actividades.
- `assets/img/brand/`: logotipo, favicon y monitor de portada.
- `assets/img/machines/`: imágenes compartidas por Inicio, Máquinas y fichas.
- `assets/img/editorial/`: imágenes de los artículos destacados.
- `assets/img/sprites/`: gráficos pixel art auxiliares.
- `assets/img/ui/`: iconos decorativos.
- `docs/GUIA_ACTUALIZACION.md`: instrucciones de mantenimiento y publicación.

## Importante

La revisión del **diseño** está cerrada, pero la información pública debe ser auténtica: las actividades no tienen fechas confirmadas aquí; las fichas son divulgativas y **no equivalen al inventario oficial** del museo. El formulario solo valida en el navegador; **no realiza inscripciones, no envía ni almacena datos**. Los textos legales deben completarse con la información oficial antes de activar la recogida de datos.

## GitHub Pages: actualización de las URL

La carpeta principal se llama igual que la ruta existente `UPV Museo Retroinformatica Practicas` intencionadamente. Los nombres de las cinco páginas han cambiado: actualiza los enlaces externos que apuntaban a las URL anteriores o configura redirecciones en el alojamiento si es posible. Para actualizar la versión publicada, sustituye **su contenido completo** por el de esta entrega. No mezcles las carpetas antiguas `css/`, `js/` o `assets/` con los recursos nuevos: todo está ya referenciado en `assets/`.

## Navegadores

Diseño responsive para escritorio y móvil con navegación accesible y preferencias de movimiento reducido. Las tipografías se obtienen de Google Fonts; en ausencia de red se emplean alternativas locales.

## Versión 1.2
Componentes unificados, imágenes WebP optimizadas, navegación sin JavaScript en móvil y mejoras de legibilidad. Comprobación estática de enlaces locales y sintaxis JavaScript superada. Pruebas visuales en Chromium bloqueadas por la política del entorno. Mantener la carpeta publicada existente al actualizar GitHub Pages.

## Versión 1.3
Tarjetas de máquinas idénticas en Inicio y Máquinas; controles visibles, fichas y navegación corregidas; jerarquía y estilo compartidos; recursos con dimensiones reales; CSS depurado. El formulario no tiene campos serializables y permanece deshabilitado sin JavaScript. Al editar la portada, mantener sincronizados `index.html` y `inicio.html`. Consulta `docs/REVISION_V1_3.md` para la validación y las limitaciones.

## Versión 1.5
Ajuste de encuadre de imágenes, navegación y preparación de una entrega separada para WordPress. Consultar `docs/WORDPRESS_V1_5.md`.


## Actualización 1.6
Seis ilustraciones extraídas de la referencia aprobada, con transparencia real y lienzo uniforme de 384 × 240 px. Recursos WebP sin pérdida; reutilizados en tarjetas, cabeceras y fichas. Sin cambios en rutas, textos o funcionalidades.
