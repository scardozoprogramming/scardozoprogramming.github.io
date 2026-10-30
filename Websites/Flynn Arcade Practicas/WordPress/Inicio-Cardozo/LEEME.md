# Inicio Cardozo: edición visual en Elementor

La portada de la página **Inicio Cardozo**, ID 6876, está reconstruida con widgets nativos y guardada como **borrador**. Los contenidos ya no están reunidos en un único widget HTML. Este trabajo corresponde únicamente a esta página.

## Cómo editarla

Abre «Estructura» en Elementor. Las secciones tienen estos nombres:

1. **00 · Design & interactions**: estilos y funciones comunes. Esta sección solo se muestra en el editor.
2. **00A · Home header · GitHub design**: logo, menú y enlaces sociales propios de Home.
2. **01 · Arcade banner**: la imagen panorámica del principio.
3. **02 · Featured game · The Last Cat**: nombre, descripción, plataforma, botón e imagen del juego principal.
4. **03 · Highlights · Eternum / Lambs / Saga**: los tres destacados, cada uno con su texto, imagen y botón.
5. **04 · Games · 32 editable cards**: buscador, categorías y tarjetas individuales.
6. **05 · Find us everywhere**: introducción y nueve enlaces sociales.
7. **06 · Better news & Send prayers**: textos, aviso de newsletter, Steam y correo de contacto.
8. **07 · Footer credits**: nombre del estudio, lema y enlace About.

Para cambiar un texto, selecciona su widget «Encabezado» o «Editor de texto». Para sustituir una portada, selecciona «Imagen». Los géneros y las plataformas se editan en «Listado de iconos»; sus elementos y enlaces tienen controles independientes. Los botones también son widgets normales.

Cada tarjeta se llama **Game · nombre del juego**. Puedes moverla en «Estructura» para cambiar el orden o duplicarla para preparar otra ficha. En el editor se muestran todas las tarjetas y los tres destacados para facilitar su edición; en la vista previa funciona la presentación normal con seis juegos iniciales y un destacado cada vez.

## Animaciones y categorías

La imagen de cada juego contiene el enlace a su ficha. En las opciones de ese enlace, el atributo personalizado `data-fc-preview|URL` indica el GIF que se activa al pasar el ratón o navegar con el teclado. Cambiar la portada no elimina esa animación. Para sustituirla, actualiza la URL de ese atributo.

Las categorías del filtro se asignan a cada tarjeta desde «Avanzado → Clases CSS»: `fc-category-narrative`, `fc-category-platform`, `fc-category-arcade`, `fc-category-point-click`, `fc-category-puzzle` o `fc-category-rpg`. Mantén la clase `fc-card`. Los rótulos visibles de los géneros se editan por separado en «Genres».

El estilo neón y las funciones especiales conservan código en la sección 00. El campo del buscador también usa un pequeño widget HTML. No hace falta editar estos bloques para cambiar los contenidos habituales.

## Archivos

- `Elementor/Inicio-Cardozo-bloques.json`: plantilla nativa de Elementor para esta página; incluye los estilos y las funciones.
- `Elementor/Inicio-Cardozo-portapapeles.json`: la misma estructura preparada para «Pegar desde otro sitio».
- `CSS/bloques.css` y `JavaScript/bloques.js`: fuentes del diseño y los comportamientos.
- `CSS/home-github.css` y `JavaScript/home-github.js`: ajustes finales para reproducir la Home de GitHub, fondo fijo continuo y menú móvil.
- `Elementor/Home-partes-nativas.json` y los dos archivos `Home-*-portapapeles.json`: cabecera y pie propios de Inicio Cardozo como widgets nativos. Ya están incluidos en la plantilla completa.
- `HTML/Inicio-Cardozo.html` y `HTML/Inicio-Cardozo.js`: respaldo de la anterior presentación en un solo widget. No se usan en la página actual ni deben pegarse encima de los bloques.

Los archivos y estilos están limitados a la página 6876. Las imágenes y los GIF enlazan a los materiales ya disponibles en WordPress; no se requiere instalar un plugin ni subir más archivos.

## Contenido y comprobaciones

- Banner panorámico completo y presentación de The Last Cat in the Universe.
- Destacados en este orden: Eternum Ex, Lambs on the Road y Saga.
- Catálogo de 32 juegos, con seis visibles inicialmente, búsqueda, categorías y Load more.
- Animaciones asignadas a las 32 tarjetas; se activan al pasar el ratón o navegar con el teclado y respetan la preferencia de reducir movimiento.
- Enlaces a las fichas actuales de la web original.
- Comprobados el borrador guardado, los filtros, la búsqueda, el estado sin resultados, la carga de más juegos, los recursos de imagen, la animación de una tarjeta y la adaptación del contenido a móvil.

## Referencia de diseño y alcance

La referencia es https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Home/HTML/index.html . La página contiene nueve secciones y 236 widgets nativos o de soporte, incluidas las 32 tarjetas de juegos.

Inicio Cardozo tiene ahora una cabecera y un pie propios, editables desde esta misma página. Sus estilos ocultan las plantillas compartidas únicamente dentro de la página 6876; las plantillas globales no se han modificado. El logo de Publishing usa el recurso público de esa referencia de GitHub. Las demás imágenes y GIF de WordPress mantienen sus enlaces actuales.

El fondo utiliza una sola capa fija del tamaño de la pantalla, con el mismo filtro y opacidad que GitHub, y continúa detrás del pie. El menú e iconos son cian/blanco con rosa en hover; los degradados están en bordes y subrayados. El pie reproduce el aviso «Newsletter coming soon» y el enlace a Steam de GitHub; el formulario del pie global permanece en su plantilla original para las otras páginas.

Lambs on the Road utiliza su WebP optimizado de gameplay tanto en la tarjeta como en el destacado; conserva `gif_lambs_correr.gif` de la web original como alternativa. Sus atributos se editan en los widgets Imagen, igual que las demás animaciones.

Los cambios se han guardado con «Guardar borrador». No se ha usado «Publicar» ni se han editado otras páginas.

## Carga optimizada

Las 32 tarjetas utilizan portadas WebP y animaciones WebP propias, ya subidas a la biblioteca de WordPress. Las animaciones conservan el gameplay como un GIF en bucle. Se han preparado clips de hasta cuatro segundos y un tamaño apropiado para las tarjetas: las versiones locales largas sumaban 49.816.650 bytes y los nuevos clips suman 10.575.766 bytes (un 79 % menos). Las 32 portadas suman 1.129.224 bytes.

El motor anticipa las animaciones próximas a la pantalla con dos descargas simultáneas como máximo. La tarjeta y el destacado del mismo juego comparten su recurso; al retirar el ratón o el foco se restaura la portada. El ahorro de datos y la preferencia de reducir movimiento evitan las descargas especulativas. Los enlaces conservan el GIF original como alternativa ante un fallo del recurso nuevo.

Las fuentes originales se han comprimido sin alterar sus tablas a WOFF. No hace falta subir archivos de fuentes: siguen incluidas en el bloque de soporte, cuyo tamaño baja de aproximadamente 462 KB a 226 KB. El banner dispone de una indicación de carga prioritaria.

- `JavaScript/preview-loader.js`: cola, caché compartida y precarga limitada de animaciones.
- `HTML/Inicio-Cardozo-bloques-soporte.html`: contenido actualizado del widget de soporte común.
- `CSS/fuentes-optimizadas.css`: fuentes comprimidas incluidas en el bloque.
- `Materiales-optimizados/manifest.json`: peso y dimensiones de cada recurso.
- `Materiales-optimizados/wordpress-urls.json`: direcciones e identificadores confirmados en la biblioteca.
- `Materiales-optimizados/comprobacion-web.json`: comprobación de las 32 animaciones distintas desde la vista previa guardada.
- `Elementor/optimizar-materiales.py` y `comprimir-fuentes.py`: procesos reproducibles que conservan intactos los archivos originales.
- `Elementor/preview-loader.test.cjs`: pruebas de concurrencia, reutilización, restauración, ahorro de datos, movimiento reducido, tarjetas ocultas y recuperación con el GIF original.

La compresión de las fuentes sigue el contenedor WOFF 1.0: https://www.w3.org/TR/WOFF/ . La reducción de peso no equivale a un tiempo de carga garantizado: el primer acceso sigue dependiendo de la conexión y del servidor.

Saga añade una animación propia en su destacado, creada a partir del vídeo proporcionado por el usuario «Saga Level 0 - Open Field Close Combat.mp4». El clip conserva cuatro segundos de gameplay, pesa 454.552 bytes y está subido a WordPress como `fc-home-saga-hover.webp`. El vídeo original de Descargas no se modifica. El catálogo mantiene sus 32 juegos y Saga conserva su posición como tercer destacado, después de Eternum Ex y Lambs on the Road.
