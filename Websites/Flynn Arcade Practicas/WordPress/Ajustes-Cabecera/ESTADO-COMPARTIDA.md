# Cabecera compartida Cardozo

Estado: aplicada y verificada en WordPress. Las cinco páginas utilizan la misma plantilla de sección **7123 · Cardozo · Shared header**.

La cabecera tiene widgets nativos de Elementor. Las cinco páginas referencian esa misma plantilla mediante `[elementor-template id="7123"]`, en lugar de insertar cinco copias independientes. Para cambiar el logo, los enlaces o las redes una sola vez, edita la plantilla desde https://www.flynnsarcades.com/wp-admin/post.php?post=7123&action=elementor.

## Destinos

| Elemento | Enlace |
| --- | --- |
| Logo y Home | https://www.flynnsarcades.com/inicio-cardozo/ |
| Games | https://www.flynnsarcades.com/games-cardozo/ |
| Indies | https://www.flynnsarcades.com/indies-cardozo/ |
| About | https://www.flynnsarcades.com/about-cardozo/ |
| Press | https://www.flynnsarcades.com/press-cardozo/ |

## Archivos

- `Cabecera-compartida-plantilla.json`: plantilla nativa importable.
- `Cabecera-compartida-portapapeles.json`: la misma sección para el portapapeles de Elementor.
- `cabecera-compartida.css` y `cabecera-compartida.js`: estilos y navegación incluidos dentro de la plantilla.
- `compartida.cjs`: generador de los archivos anteriores, sin acceso a WordPress.

La selección activa depende de la página actual. El menú móvil conserva apertura, cierre y atributos de accesibilidad. El soporte se limita a la cabecera compartida. Se mantuvo el contenido de las páginas; Inicio, Games, About e Indies se guardaron como borradores. Press ya estaba publicada al abrir el editor y se guardó su actualización.

## Verificación en WordPress

- Las cinco páginas muestran una sola cabecera y una referencia a la plantilla compartida.
- Los 25 enlaces del menú y los cinco enlaces del logo coinciden con la tabla anterior.
- La página activa se marca correctamente en las cinco cabeceras.
- No hay desbordamiento horizontal en escritorio.
- En el modo móvil de Elementor (360 px), el menú abre y cierra con su botón y la cabecera no tiene desbordamiento horizontal.
- La prueba de Escape dentro del editor no resultó concluyente, ya que Elementor gestiona también esa tecla; no se presenta como una comprobación completada.

Los archivos locales de bloques y portapapeles también utilizan la plantilla 7123. `actualizar-referencias.cjs` sincroniza esas referencias sin acceder a WordPress. La antigua cabecera global del sitio permanece independiente; esta plantilla se inserta solo en las páginas Cardozo.
