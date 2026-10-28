# Inicio Cardozo: edición visual en Elementor

La portada de la página **Inicio Cardozo**, ID 6876, está reconstruida con widgets nativos y guardada como **borrador**. Los contenidos ya no están reunidos en un único widget HTML. Este trabajo corresponde únicamente a esta página.

## Cómo editarla

Abre «Estructura» en Elementor. Las secciones tienen estos nombres:

1. **00 · Design & interactions**: estilos y funciones comunes. Esta sección solo se muestra en el editor.
2. **01 · Arcade banner**: la imagen panorámica del principio.
3. **02 · Featured game · The Last Cat**: nombre, descripción, plataforma, botón e imagen del juego principal.
4. **03 · Highlights · Eternum / Lambs / Saga**: los tres destacados, cada uno con su texto, imagen y botón.
5. **04 · Games · 32 editable cards**: buscador, categorías y tarjetas individuales.

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
- `HTML/Inicio-Cardozo.html` y `HTML/Inicio-Cardozo.js`: respaldo de la anterior presentación en un solo widget. No se usan en la página actual ni deben pegarse encima de los bloques.

Los archivos y estilos están limitados a la página 6876. Las imágenes y los GIF enlazan a los materiales ya disponibles en WordPress; no se requiere instalar un plugin ni subir más archivos.

## Contenido y comprobaciones

- Banner panorámico completo y presentación de The Last Cat in the Universe.
- Destacados en este orden: Eternum Ex, Lambs on the Road y Saga.
- Catálogo de 32 juegos, con seis visibles inicialmente, búsqueda, categorías y Load more.
- Animaciones asignadas a las 32 tarjetas; se activan al pasar el ratón o navegar con el teclado y respetan la preferencia de reducir movimiento.
- Enlaces a las fichas actuales de la web original.
- Comprobados el borrador guardado, los filtros, la búsqueda, el estado sin resultados, la carga de más juegos, los recursos de imagen, la animación de una tarjeta y la adaptación del contenido a móvil.

La cabecera y el pie compartidos conservan su configuración actual. Este trabajo solo corresponde al contenido de Home en Inicio Cardozo.
