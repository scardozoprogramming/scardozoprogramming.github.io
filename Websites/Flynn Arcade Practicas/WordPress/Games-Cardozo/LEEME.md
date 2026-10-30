# Games Cardozo

Página de WordPress **6953**, guardada como **borrador**. Reproduce el catálogo de Games de la web de GitHub con el mismo diseño y los recursos optimizados utilizados en Inicio Cardozo.

## Edición en Elementor

Abre [Games Cardozo](https://www.flynnsarcades.com/wp-admin/post.php?post=6953&action=elementor). En Estructura aparecen siete secciones:

1. Design & interactions: estilos, fuentes e interacciones de esta página.
2. Games header: logo, navegación y redes editables.
3. Games heading: título nativo.
4. Games: 32 tarjetas editables. Cada juego contiene imagen, título, géneros, plataformas y enlace.
5. Find us everywhere: redes sociales.
6. Better news & Send prayers: textos y contacto.
7. Footer credits.

Los textos, imágenes, botones e iconos son widgets de Elementor. Solo el buscador y el soporte de estilos e interacciones necesitan pequeños bloques HTML. El catálogo muestra seis juegos inicialmente y añade seis con Load more. Los filtros y la búsqueda se pueden combinar; el botón de limpiar recupera el catálogo completo.

## Animaciones y rendimiento

Las 32 portadas y las 32 animaciones WebP ya están en la biblioteca de medios de WordPress. Se reutilizan las mismas direcciones que en Home; no se duplicaron archivos en el servidor ni se necesitan nuevas subidas. Cada tarjeta conserva su GIF de respaldo. Las animaciones se preparan cerca de la vista, se reproducen al pasar el ratón o enfocar la tarjeta y restauran la portada al salir. El cargador limita las descargas simultáneas, comparte caché y respeta movimiento reducido y ahorro de datos.

## Archivos

- `Elementor/Games-Cardozo-bloques.json`: plantilla completa importable.
- `Elementor/Games-Cardozo-portapapeles.json`: formato Pegar desde otro sitio.
- `Elementor/generar.cjs`: generador a partir de los bloques y recursos finales de Inicio Cardozo. Solo genera archivos locales de Games.
- `Elementor/referencia-juegos.json`: 32 títulos, géneros y plataformas contrastados con GitHub.
- `Elementor/comprobacion-web.json`: resultados de las comprobaciones en WordPress.
- `HTML/Games-Cardozo-soporte.html`: soporte integrado en el primer bloque HTML.
- `CSS/games-cardozo.css`: estilos completos de esta página, sin las reglas de las secciones exclusivas de Home.
- `CSS/fuentes-optimizadas.css`: fuentes WOFF comprimidas.
- `CSS/catalogo.css` y `CSS/compartido.css`: fuentes de los estilos.
- `JavaScript/catalogo.js`, `navegacion.js` y `preview-loader.js`: buscador, filtros, navegación y animaciones.

Los estilos están limitados a la página 6953. Su cabecera y pie propios sustituyen visualmente las plantillas compartidas solo dentro de esta página. No se cambiaron Inicio Cardozo, el tema, los plugins ni las plantillas globales.

## Comprobaciones

Verificados los siete filtros, la búsqueda por título, la combinación sin resultados, el botón de limpiar y la carga progresiva hasta 32. Verificadas 32 animaciones distintas cargadas correctamente, incluida Lambs on the Road. Sin errores o avisos de consola en la vista previa. Revisados escritorio, dos columnas en tableta y una en móvil; el pie aparece completo. Los pequeños controles de redimensionado visibles en el editor no pertenecen a la página publicada.

Referencia: [Games en GitHub](https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Games/HTML/index.html).
