# Procedencia del contenido

Revision inicial el 5 de octubre de 2026. Estructura y recursos revisados de nuevo el 15 de octubre de 2026.

## Fuentes publicas de la empresa

- Catalogo: https://www.flynnsarcades.com/
- Creadores y estudios: https://www.flynnsarcades.com/indies/
- Descripcion e historia: https://www.flynnsarcades.com/about-3/
- Prensa: https://www.flynnsarcades.com/press/

Se consultaron tambien las fichas de What Comes After, Donut Dodo, Looking Up I See Only A Ceiling, Dexter Stardust, Eternum Ex, The Last Cat in the Universe y Viral Reload EX.

## Adaptacion

- Los 32 juegos proceden de la galeria completa de la portada, incluyendo los elementos que aparecen al pulsar Load more. Se conserva su orden.
- Los primeros seis juegos coinciden con los de la portada original: What Comes After, Donut Dodo, Looking Up I See Only A Ceiling, Dexter Stardust, Eternum Ex y The Last Cat in the Universe.
- Las imagenes de juegos son recursos originales descargados a `assets/images/games/`. No se mantienen referencias remotas en el HTML publicado.
- Las plataformas se toman de los iconos del catalogo y de las fichas de producto. Las etiquetas de las tarjetas resumen las categorias y el genero descrito en esas fichas. Point & Click y Shooter se incluyen como filtros utiles.
- Los 16 nombres de creadores y estudios, y sus juegos asociados, proceden de Indies.
- About conserva literalmente la descripcion y las secciones Early history y After that, con sus seis parrafos y su orden. Se incorporan las seis fotografias originales de la galeria y las ilustraciones de Eternum Ex y Lambs on the road con sus frases originales. No se anade una sede actual que esa pagina no confirme.
- Press muestra las 15 menciones y medios de la pagina original. Las fechas se muestran sin anadir anos que la fuente no indique. No se copian opiniones ni puntuaciones de resenas como si fueran afirmaciones propias.
- El contacto de prensa utiliza un enlace mailto, solicitado por el usuario. La navegacion permanece dentro de esta version local.

## Correcciones de la maqueta anterior

La sexta tarjeta deja de ser Viral Reload EX y pasa a The Last Cat in the Universe, conforme a la portada actual. Viral Reload EX permanece en su posicion original dentro del catalogo completo.

Donut Dodo muestra Switch y PC, no las plataformas PlayStation y Xbox que aparecian por defecto en la maqueta. Dexter Stardust refleja Switch y Xbox en el catalogo consultado. Eternum Ex muestra las cuatro plataformas indicadas por la web.

El bloque indie presenta creadores reales, y About incluye los origenes del proyecto en la universidad y el prototipo OUTSIDERS. Los bloques adicionales de marketing de la version anterior no se reintroducen en la portada.

La version 2.0 sustituye las ventanas por paginas independientes y sigue la estructura del original. Se anaden el logo autentico, las imagenes de los creadores, los destacados y los bloques de presskits. La nueva imagen de portada es el archivo del nino adjunto por el usuario el 15 de octubre, conservado sin alterar. La suscripcion sigue pendiente de integracion. Ver `estructura-y-entrega.md` para el alcance actual.

La version 2.2 corrige About contrastando `about-3/` con `checks/about-reference.json` y `checks/about-original-desktop.png`. Las seis fotos se guardan sin alterar. Las dos ilustraciones conservan su contenido en WebP sin perdida; los PNG fuente se guardan en `checks/`, fuera del tema, para no incluir sus metadatos pesados en WordPress. Ninguna imagen se carga desde el sitio de referencia.

En la version 2.3, por peticion posterior del usuario, Projects cambia a tarjetas de Eternum Ex y Lambs on the Road con las portadas, generos y plataformas del catalogo, siguiendo su ultima captura. La galeria cambia a un filtro cyan y los encabezados de historia pasan a rosa con esquinas rectas.
