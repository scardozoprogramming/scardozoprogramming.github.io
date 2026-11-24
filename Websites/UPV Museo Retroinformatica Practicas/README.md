# Club de Retroinformática UPV — sitio web

Esta es la versión web unificada basada en el diseño oscuro y retro aprobado para continuar el proyecto.

## Cómo abrirlo

1. Descomprime el ZIP conservando la carpeta **`retroinformatica_prototipo_v1_1`**.
2. Abre **`retroinformatica_Inicio.html`** (o `index.html`) en un navegador.
3. Recorre Inicio, Actividades, Qué es el club, Máquinas y Únete desde el menú.

No hace falta instalar paquetes ni ejecutar un servidor para ver el contenido básico. Para comprobar la integración con GitHub Pages, publica la carpeta en tu repositorio.

## Organización

- **`retroinformatica_*.html`**: cinco páginas del sitio.
- `index.html`: acceso automático al Inicio cuando se visita la carpeta raíz.
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

## GitHub Pages: conserva las URL actuales

La carpeta principal se llama igual que la ruta existente `retroinformatica_prototipo_v1_1` intencionadamente. Para actualizar la versión publicada, sustituye **su contenido completo** por el de esta entrega. No mezcles las carpetas antiguas `css/`, `js/` o `assets/` con los recursos nuevos: todo está ya referenciado en `assets/`.

## Navegadores

Diseño responsive para escritorio y móvil con navegación accesible y preferencias de movimiento reducido. Las tipografías se obtienen de Google Fonts; en ausencia de red se emplean alternativas locales.
