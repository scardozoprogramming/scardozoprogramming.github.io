# About Cardozo

Página de WordPress **6925**, guardada como **borrador**, con el diseño de [About en GitHub](https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/About/HTML/index.html).

## Editar en Elementor

Abre [About Cardozo](https://www.flynnsarcades.com/wp-admin/post.php?post=6925&action=elementor). En Estructura encontrarás nueve secciones:

1. Design & interactions: estilos, fuentes e interacciones de esta página.
2. About header: logo, menú y redes editables.
3. Studio logo: logo central y título accesible de la página.
4. Studio photos: seis imágenes independientes, con filtro naranja y galería ampliada.
5. Description / Early history / After that: tres encabezados y tres editores de texto, que conservan los seis párrafos de la referencia.
6. Projects: Eternum Ex, Lambs on the Road y Saga, en ese orden. Cada tarjeta tiene portada, título, descripción, géneros, plataformas y enlace editables.
7. Find us everywhere: redes sociales.
8. Better news & Send prayers: textos y contacto.
9. Footer credits.

Todos los contenidos visibles usan widgets de Elementor. El primer bloque HTML reúne únicamente los estilos, las fuentes y las interacciones. La galería ampliada usa el visor nativo de Elementor, con sus controles en inglés dentro de esta página. Se puede recorrer con los botones y cerrar con Escape.

## Recursos y rendimiento

Las seis fotos reutilizan las imágenes existentes en la biblioteca de medios, con miniaturas y versiones adaptadas al tamaño de pantalla. Los originales se usan al ampliar una foto. No se duplicaron archivos en WordPress ni hace falta subir más material.

Los proyectos reutilizan las portadas y animaciones WebP optimizadas de Home. Las animaciones se preparan cerca de la vista, se reproducen con el ratón o el foco y vuelven a la portada al salir. El cargador limita las descargas simultáneas, reutiliza caché y respeta movimiento reducido y ahorro de datos. Saga utiliza el fragmento del vídeo proporcionado por el usuario y enlaza a su ficha de Steam en inglés.

## Archivos

- `Elementor/About-Cardozo-bloques.json`: plantilla completa de la página.
- `Elementor/About-Cardozo-portapapeles.json`: formato Pegar desde otro sitio.
- `Elementor/generar.cjs`: generador local de los archivos de About, que lee los recursos finales de Home y Games.
- `Elementor/referencia.json`: textos y fotografías contrastados con la referencia y los medios existentes.
- `Elementor/comprobacion-web.json`: resultados de las comprobaciones en WordPress.
- `HTML/About-Cardozo-soporte.html`: contenido del bloque de soporte.
- `CSS/about-cardozo.css`: estilos completos, limitados a la página 6925.
- `CSS/about.css`: fuente de los estilos propios de About.
- `CSS/fuentes-optimizadas.css`: fuentes WOFF comprimidas.
- `JavaScript/about.js`, `navegacion.js` y `preview-loader.js`: accesibilidad, navegación y animaciones.

Los archivos están preparados para la página **6925**. Su cabecera y pie propios sustituyen visualmente las plantillas compartidas solo dentro de esta página. No se modificaron Home, Games, otras páginas, las plantillas globales, el tema ni los plugins.

## Verificación

Se contrastaron los seis párrafos completos, las seis fotos y el orden de los tres proyectos. Se probaron la ampliación de fotos, la navegación anterior y siguiente, la última imagen y el cierre con Escape. Se comprobaron las tres animaciones y el enlace de Saga a Steam. Se revisaron escritorio, tableta y móvil, con tres, dos y una columnas de proyectos respectivamente. Los controles de redimensionado del editor pueden sobresalir unos píxeles; esos controles no aparecen en la vista web.
