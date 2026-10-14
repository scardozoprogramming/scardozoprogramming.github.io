# Flynn's Arcade Neon

Tema WordPress en PHP, HTML, CSS y JavaScript. La portada sigue la estructura de la imagen facilitada: cabecera, portada arcade, seis juegos, bloque indie junto a newsletter y pie de pagina.

## Vista previa

Abrir `preview.html` directamente en el navegador. Todos los recursos son locales; no requiere servidor, instalacion de paquetes ni conexion a la web de referencia.

La vista previa se genera a partir de las mismas plantillas PHP del tema, evitando mantener dos versiones del contenido.

## Instalar en WordPress

1. Ir a Apariencia > Temas > Anadir nuevo > Subir tema.
2. Subir `flynns-arcade-neon-wordpress-theme.zip` y activar.
3. La plantilla `front-page.php` muestra la portada.

## Contenido y estructura

- `inc/catalog.json`: catalogo real de 32 juegos, conservando el orden de la web original. Los seis primeros aparecen en la portada; todos aparecen en el catalogo completo.
- `inc/editorial.json`: informacion de la empresa, 16 creadores y estudios, 15 menciones de prensa, textos del spotlight y contacto.
- `inc/content.php`: lectura de los datos y categorias del buscador.
- `inc/template-tags.php`: componentes compartidos, logos, iconos y tarjetas.
- `front-page.php`, `header.php`, `footer.php`: estructura de la pagina.
- `style.css`: estilos y adaptacion a ordenador, tableta y movil.
- `assets/js/theme.js`: navegacion movil, ventanas de contenido, filtros, buscador y controles del spotlight.

Los enlaces Games e Indies llevan a sus bloques de portada. About y Press muestran informacion local. View All Games abre el catalogo completo; cada tarjeta permite consultar su descripcion.

## Recursos visuales

`assets/images/reference-art.png` es la imagen facilitada por el usuario, conservada sin modificar. La escena arcade, el bloque indie y las marcas mantienen su aspecto. Los textos, botones, tarjetas y formularios de la pagina son elementos HTML reales.

`assets/images/games/` contiene las imagenes reales de los 32 juegos, descargadas desde la web original. Las tarjetas ya no utilizan ilustraciones de concepto como si fueran arte de los juegos. Todos los archivos se sirven localmente.

Las tipografias Press Start 2P y Barlow se distribuyen localmente con sus licencias OFL. Los iconos de herramientas proceden de Lucide, con licencia incluida. Las siluetas Nintendo Switch, PlayStation y Xbox proceden de Simple Icons 11.14.0; su licencia se incluye junto a los recursos. PC utiliza una silueta de monitor.

## Suscripcion

El formulario conserva el diseno de la referencia. La integracion con un proveedor de newsletter queda pendiente. La vista previa comunica que las suscripciones aun no estan abiertas; no guarda ni envia direcciones de correo. Los simbolos de redes no contienen enlaces ficticios. El icono de correo abre la informacion de prensa local.

## Herramientas de desarrollo

En la carpeta superior se incluyen:

- `tools/render-preview.php`: ejecutar `php tools/render-preview.php` desde la carpeta del proyecto para regenerar `preview.html` tras editar las plantillas o el catalogo.
- `tools/check-preview.cjs`: comprobacion de la presentacion y las interacciones con Playwright. Las capturas se guardan en `checks/`.
- `tools/build-icons.cjs`: exportacion de los iconos utilizados desde Lucide.
- `docs/content-source.md`: procedencia y criterios de adaptacion del contenido.

Las herramientas JavaScript usan las dependencias del entorno local de Codex indicado en sus importaciones. No son necesarias para utilizar la vista previa ni instalar el tema en WordPress.
