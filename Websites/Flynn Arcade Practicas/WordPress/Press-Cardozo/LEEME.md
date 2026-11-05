# Press Cardozo

Página de WordPress **7098**, guardada como **borrador**. Reproduce [Press en GitHub](https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Press/HTML/index.html) con contenidos editables en Elementor y la misma identidad visual de Home, Games y About.

Abre [Press Cardozo en Elementor](https://www.flynnsarcades.com/wp-admin/post.php?post=7098&action=elementor).

## Contenidos editables

- Cabecera con logo, menú y redes.
- Título Press Media y accesos a Coverage, Presskits y Contact.
- Quince artículos: cada imagen, fecha, medio, título, descripción y enlace es un widget editable.
- Buscador por título o medio, contador y More coverage. Se muestran seis artículos inicialmente; después doce y quince. Clear search restaura la vista inicial.
- Cuatro press kits: What Comes After, Lambs on the Road, Kolumno y Flynn's Arcade. Cada tarjeta tiene imagen, título, descripción y botón nativo de descarga.
- Carrusel con los tres juegos desarrollados indicados por el usuario: **Eternum Ex, Lambs on the Road y Saga**, en ese orden. Reutiliza las animaciones WebP optimizadas de Home, con controles manuales, pausa durante la interacción y respeto del movimiento reducido.
- Redes, Better news, Send prayers y créditos, con el diseño de Home.

Hay **157 widgets en 27 secciones**. Solo el buscador y el soporte de estilos e interacciones usan bloques HTML; el contenido visible restante usa widgets nativos. En el editor se muestran todos los artículos y destacados para permitir su edición. El contenido oculto mediante búsqueda o paginación se gestiona en la vista web.

## Recursos

Las imágenes de los artículos y press kits y sus ZIP enlazan a los archivos públicos existentes en el GitHub del usuario, como en la referencia. Los tres destacados reutilizan los medios de WordPress y las animaciones WebP optimizadas de Home. Las imágenes usan carga diferida y dimensiones estables. Las fuentes WOFF comprimidas se reutilizan de Home. No se requieren nuevas subidas a la biblioteca de medios para usar esta versión.

Los estilos se limitan a la página 7098. La cabecera y el pie propios sustituyen visualmente las plantillas compartidas únicamente dentro de esta página. No se cambiaron otras páginas, el tema ni los plugins.

## Archivos

- `Elementor/Press-Cardozo-bloques.json`: plantilla completa para esta página.
- `Elementor/Press-Cardozo-portapapeles.json`: formato Pegar desde otro sitio.
- `Elementor/generar.cjs`: generador local a partir de la referencia y los componentes finales de Home.
- `Elementor/referencia.json`: contenidos contrastados con la página pública.
- `Elementor/comprobacion-web.json`: resultados de las comprobaciones en WordPress.
- `HTML/Press-Cardozo-soporte.html`: contenido del primer bloque de soporte.
- `CSS/press-cardozo.css`: estilos completos y limitados a esta página.
- `CSS/press.css`: estilos propios del diseño de Press.
- `CSS/fuentes-optimizadas.css`: fuentes WOFF.
- `JavaScript/press.js`, `navegacion.js` y `preview-loader.js`: buscador, paginación, carrusel, navegación móvil y animaciones.

## Verificación

Se comprobaron el buscador, la búsqueda sin resultados, la limpieza de la búsqueda y la carga hasta quince artículos. Se cargaron correctamente las quince imágenes de artículos y se verificaron los tres destacados. Se descargaron los cuatro ZIP desde sus botones en WordPress y se validó la integridad de sus archivos. Se revisaron escritorio, tableta y móvil, incluidos los enlaces internos a Coverage, Presskits y Contact. Los pequeños controles de redimensionado visibles en el editor no forman parte de la vista web.
