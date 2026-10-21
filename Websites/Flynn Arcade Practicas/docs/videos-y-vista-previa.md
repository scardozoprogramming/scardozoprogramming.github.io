# Videos de los 32 juegos

Revision del 21 de octubre de 2026, tema 2.7.0. La distribucion aprobada en What Comes After se aplica al catalogo completo. Se conservan el estilo actual, la portada y las fotos de About.

## Correspondencia con la fuente

La copia de referencia de What Comes After contiene dos widgets diferentes:

- `f708aa9`: video superior `ZBIj7KCL5LU`, What Comes After Streamers Reaction.
- `e01560f`: trailer junto a Features `zk9ebZ-LRFU`, What Comes After Trailer, con `t=4s`.

Ambos se comprobaron con el servicio publico oEmbed de YouTube: devuelve HTTP 200, los titulos anteriores y el canal Flynn's Arcade. La asignacion explicita se guarda en `inc/game-overrides.json`, para conservarla al regenerar los datos. No se cambia silenciosamente un video por otro.

Todas las fichas muestran video superior y trailer junto a las caracteristicas, ademas de sus cinco capturas ampliables. Las de Galacticon proceden de su antiguo carrusel y se trasladan a la galeria para no perderlas.

## Asignacion del catalogo

`tools/parse-game-pages.cjs` distingue widgets superiores, widgets del cuerpo y videos de los carruseles originales. Tambien conserva los segundos iniciales. `tools/import-game-assets.cjs` aplica esa correspondencia, las excepciones verificadas y las mismas plantillas a los 32 juegos.

- Eternum Ex: gameplay `4cKBn3aDJX0` arriba, desde el segundo 142 como en la fuente; trailer final `MSRTJvFvM2E` junto a Features.
- Galacticon: video del desarrollador `1qq2xSTWAoA` arriba y trailer del editor `rwUEnjfGp-A` en el cuerpo.
- Papertris: `fRrG1LPihvg` en ambos espacios; se excluye `OkY-OcmmmU8`, que pertenece a Legend of Gwen y estaba copiado en su carrusel original.
- Super Spy: se conserva el inicio en el segundo 3; Sentry City, en el segundo 1; A Winding Path, en el segundo 2 en el video superior.
- En los juegos que solo publican un video propio, se reutiliza en ambos espacios para seguir la distribucion solicitada sin atribuirles un segundo trailer inexistente.

Las dos posiciones se guardan como `hero_video` y `trailers` en los datos locales. Se elimina el antiguo campo `hero_slides` y el codigo/estilos del carrusel de capturas. No se duplican las plantillas de WordPress y HTML ni se consulta la web de origen para mostrar las fichas.

## Error 153 y vista local

YouTube identifica el error 153 como la ausencia del HTTP Referer o una identificacion equivalente. Su documentacion recomienda `strict-origin-when-cross-origin`:

- [Referencia oficial del reproductor](https://developers.google.com/youtube/iframe_api_reference#onError)
- [Identificacion del cliente](https://developers.google.com/youtube/terms/required-minimum-functionality#api-client-identity-and-credentials)

La vista `http://127.0.0.1:4173/game-what-comes-after.html` se sirve desde el ordenador del usuario, sin publicar nada en Internet. Usa los archivos de su carpeta. En HTTP/HTTPS, el navegador envia su Referer real; no se inventa un dominio ni se utiliza el sitio de referencia como origen. El iframe declara esa politica y utiliza el origen real de la pagina.

En `file://`, el boton mantiene su enlace normal y abre YouTube directamente. Asi no se inserta un reproductor que inevitablemente careceria de Referer. Sin JavaScript funciona el mismo enlace. Todos los reproductores mantienen tambien un acceso directo visible a YouTube cuando ya estan insertados.

## Comprobacion real

`tools/check-video-players.cjs` verifica ambos reproductores de cada juego a 1440 y 390 px, mas cinco ejemplos a 1191, 768 y 320 px: 79 distribuciones y 158 activaciones. Contrasta todos los IDs y segundos iniciales con la fuente guardada y consulta los titulos/canales mediante oEmbed oficial. Comprueba ausencia de conexiones externas antes de pulsar, Referer real, area util minima de 200 x 200 px y estabilidad al activar el reproductor.

La comprobacion online se registra por separado. El video superior se reprodujo y avanzo sin errores. El trailer devolvio "Este video no esta disponible" dentro de los reproductores oficiales con y sin cookies; no fue un error 153. Al abrir su pagina normal en YouTube, el titulo, el canal y la duracion de 1:12 aparecen correctamente, y su estado publico indica OK y playableInEmbed. Por tanto, no se afirma que se haya eliminado ni se sustituye por otro trailer: se conserva el original y su enlace directo. La disponibilidad dentro del reproductor debe volver a comprobarse desde el navegador habitual o el dominio final.

La muestra online incluye What Comes After, Donut Dodo, Eternum Ex, Papertris y Galacticon. Sus resultados se registran por separado: los metadatos HTTP 200 no garantizan que el servicio externo permita insertar o reproducir un video desde cualquier navegador. No se eliminan ni sustituyen los videos originales por avisos temporales.

Las comprobaciones de las 32 fichas sin red y de las paginas generales se mantienen separadas de esta disponibilidad externa. Los informes de esta revision se guardan en `checks/video-players-*`, `checks/game-pages-*` y `checks/preview-*`.
