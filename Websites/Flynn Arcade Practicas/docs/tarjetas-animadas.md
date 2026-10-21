# Tarjetas animadas

Revision 2.8.0, 21 de octubre de 2026. Se aplica al catalogo compartido de Inicio y Games, sin cambiar las fichas individuales ni la distribucion de About.

## Recursos originales

`tools/import-card-previews.cjs` inspecciona las referencias de las 32 fichas y selecciona GIF del contenido propio, priorizando las imagenes de su descripcion frente a fondos o carruseles reutilizados. No ejecuta codigo del sitio original ni incorpora sus estilos.

Se preparan 31 animaciones. Eternum Ex permanece estatico: no se encontro GIF en su ficha ni en los resultados publicos de su biblioteca, y el usuario confirmo que todavia no tiene uno. No se inventa una animacion ni se asigna la de otro juego.

En The Last Cat in the Universe y Boardwalk Builders se utiliza el GIF de su descripcion, no el de Viral Reload EX que aparecia copiado en el bloque superior. Satryn utiliza su GIF de marketing, no el recurso externo de Steam incrustado en otro apartado.

Los originales se conservan en `checks/card-preview-originals/`. Las versiones de uso web, en `flynns-arcade-neon/assets/images/previews/`, mantienen la animacion y su duracion en WebP, con anchura maxima de 640 px y sin recortar. El codificador puede combinar fotogramas identicos conservando sus tiempos; no se transforma una galeria de fotos en una animacion inventada.

`checks/card-preview-import.json` registra la URL original, el archivo local, dimensiones, fotogramas de entrada/salida, duracion y pesos. El tema solo necesita `inc/card-previews.json`, con rutas locales y dimensiones; no requiere los archivos originales ni acceso al sitio de referencia.

Los originales suman 213.401.617 bytes; las versiones optimizadas, 49.438.518 bytes, aproximadamente un 77% menos. Estos recursos no forman parte de la descarga inicial de las paginas: cada tarjeta solicita su animacion solo al activarse.

## Comportamiento

- La portada fija es la imagen inicial y no se precarga toda la coleccion de GIF.
- La animacion se descarga y decodifica al activar su tarjeta con raton o foco visible de teclado.
- Al salir, perder el foco, ocultar la tarjeta con filtros o activar movimiento reducido, se recupera la portada.
- Si el puntero sale antes de terminar la descarga, la animacion no sustituye la portada al llegar tarde.
- Un error de carga conserva la imagen inicial.
- En movil, un toque sigue abriendo la ficha sin obligar a tocar dos veces.
- La imagen y la animacion comparten el mismo espacio, sin cambiar el tamano de la tarjeta ni cortar su contenido.

El componente de tarjetas recibe una opcion explicita de animacion, utilizada solo por el catalogo de Inicio y Games. Los proyectos de About y los juegos relacionados no se modifican.

## Comprobaciones

`tools/check-card-previews.cjs` revisa las 64 tarjetas entre Inicio y Games, la duracion de todos los recursos, muestras de movimiento real en capturas, carga bajo demanda, restauracion de portadas, medidas estables, teclado, movimiento reducido, navegacion tactil, carga fallida y apertura directa como archivo HTML.

La suite general comprueba tambien filtros, buscador, Load more, navegacion y distribucion de escritorio/movil. Los resultados y capturas de esta revision se guardan en carpetas `checks/card-previews-*` y `checks/preview-*`.

Verificacion final: 64 tarjetas comprobadas entre las dos paginas, seis muestras de movimiento real con capturas de pantalla, 42 distribuciones generales y 85 distribuciones de fichas, sin errores JavaScript. Se comprobaron tambien la salida del puntero antes de terminar la descarga y el fallo de un recurso, conservando la portada en ambos casos.
