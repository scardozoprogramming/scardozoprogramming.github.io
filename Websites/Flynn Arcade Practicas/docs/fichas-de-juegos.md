# Fichas de juegos

Revision del 21 de octubre de 2026, version 2.7.0. Los 32 elementos del catalogo tienen fichas completas y comparten ahora la distribucion aprobada por el usuario en What Comes After, manteniendo sus contenidos originales y el estilo actual.

## Estructura

1. Presentacion: nombre propio, lema original, plataformas y video del juego.
2. Galeria de cinco capturas ampliables. Galacticon traslada sus capturas originales a este bloque para conservarlas al sustituir el carrusel superior por el video.
3. Purchase: enlaces a los productos correctos en sus tiendas, con los logos de plataformas magenta del resto de la pagina.
4. Portada, tres encabezados de genero, descripcion completa, caracteristicas y trailer.
5. Resenas pertinentes cuando existen, con extractos breves, autor, medio y enlace.
6. Other Games: los juegos relacionados publicados en cada ficha, con navegacion local y sin enlazar a la propia ficha.
7. Pie compartido de redes, newsletter y contacto.

No se repiten las versiones duplicadas para escritorio/movil del codigo original ni sus feeds vacios. Se conserva una sola estructura responsive y los bloques con contenido real. La galeria tiene cinco columnas en escritorio, dos en tableta y una en movil, siguiendo la referencia.

## Datos y recursos

- `flynns-arcade-neon/inc/catalog.json`: catalogo y tarjetas existentes, sin reordenar.
- `flynns-arcade-neon/inc/game-details.json`: datos propios de las 32 fichas.
- `flynns-arcade-neon/assets/images/game-details/`: 191 recursos WebP locales, aproximadamente 15 MB.
- `checks/game-reference/manifest.json`: correspondencia de cada juego con su fuente original y los enlaces de plataforma del catalogo.
- `checks/game-reference/details-extracted.json`: contenido y orden extraidos de cada pagina.
- `checks/game-reference/corrections.json`: referencias cruzadas detectadas y decisiones aplicadas.
- `checks/game-reference/asset-manifest.json`: procedencia y dimensiones de cada imagen.

Se priorizan las capturas de la galeria especifica de cada titulo: algunas paginas nuevas conservaban un carrusel de otro juego. Se descartan las resenas que identifican otro titulo, sin sustituirlas por citas inventadas. Los enlaces de tienda del catalogo se utilizan para corregir enlaces de compra copiados de otra ficha. Los enlaces oficiales de Steam corregidos son Donut Dodo (app 1779560) y Colored Effects (app 1892570).

## Funcionamiento independiente

Las fichas se abren directamente como HTML y se generan desde la misma plantilla PHP que utiliza WordPress. No cargan imagenes, estilos, scripts ni datos desde flynnsarcades.com. Los textos se renderizan escapados, sin importar HTML, scripts o estilos del sitio de referencia.

Los trailers conservan un enlace publico a YouTube y solo insertan su reproductor tras una accion del usuario en HTTP/HTTPS. En archivos file:// abren directamente YouTube para evitar el error 153 por falta de Referer. El acceso directo se mantiene tambien despues de insertar el reproductor. La reproduccion efectiva necesita conexion y puede depender de restricciones o disponibilidad del video. Las pruebas sin red verifican los enlaces y este comportamiento alternativo; una prueba HTTP comprueba aparte los reproductores y registra la reproduccion real.

En What Comes After, el video superior es `ZBIj7KCL5LU` (What Comes After Streamers Reaction) y el de Features es `zk9ebZ-LRFU` (What Comes After Trailer), con inicio en el segundo 4. Los dos pertenecen al canal de Flynn's Arcade y aparecen en esas posiciones en la fuente; no se reutiliza el primero como trailer. Las cinco capturas de la galeria inferior no se eliminan. Ver `videos-y-vista-previa.md` para la incidencia de disponibilidad del servicio externo.

La misma regla se aplica al resto: se prioriza el widget superior para la presentacion y el del cuerpo para Features. Cuando no existe otro video en el cuerpo, se utiliza el video propio del carrusel original o el mismo video superior. Eternum Ex conserva gameplay y trailer separados; Galacticon mantiene sus dos videos; Papertris excluye el video copiado de Legend of Gwen. Los segundos iniciales se extraen y conservan. No se presentan como dos videos diferentes cuando solo existe uno.

## Verificacion

`tools/check-game-pages.cjs` comprueba las 32 fichas a 1440 y 390 px y siete ejemplos adicionales a 1191, 768 y 320 px: 85 distribuciones. Comprueba titulos, palabras originales, capturas, orden, plataformas, enlaces propios, visores, teclado, enlaces de video y ausencia de dependencias remotas antes de interactuar. El resultado y las capturas quedan en una carpeta unica de `checks/game-pages-*`.

`tools/check-video-players.cjs` verifica la activacion HTTP de los dos reproductores en cada ficha, el Referer real, los segundos iniciales, la estabilidad del diseno y la procedencia de todos los videos. La reproduccion real se muestrea por separado y se registra sin confundir disponibilidad de metadatos con reproduccion efectiva.

El informe `revision-fichas.md` se genera con la matriz de los 32 juegos tras superar las comprobaciones. La suite general `check-preview.cjs` revisa ademas Inicio, Games, Indies, About y Press para evitar regresiones.
