# Flynn's Arcade Neon 2.8.0

Adaptacion independiente de la estructura de la web original de Flynn's Arcade, con la paleta oscura, cyan y magenta de la referencia visual. Se utiliza el logo autentico del estudio y una adaptacion panoramica de la imagen del nino facilitada por el usuario.

## Abrir la entrega

Abrir `preview.html` o `index.html` directamente en el navegador para ver los contenidos locales. Para reproducir videos dentro de las paginas, utilizar una vista HTTP local o el tema publicado en WordPress: los archivos `file://` no proporcionan a YouTube el Referer que exige. Al abrir archivos directamente, los botones de video abren YouTube en otra pestana. No se necesita conexion a la web original. Los menus llevan a paginas reales:

- Inicio: banner panoramico del nino a todo el ancho, proporcion 3:1 y escena completa sin recortes ni deformacion; informacion de The Last Cat in the Universe en una seccion independiente, destacados y catalogo.
- Games (`games.html`): pagina independiente dedicada al catalogo, con filtros, buscador y Load more, sin portada ni destacados.
- Indies: los 16 creadores y estudios con sus logos, imagenes y juegos.
- About: logo centrado, las seis fotos originales ampliables, Description, Early history y After that en tres columnas, y Projects con tarjetas de Eternum Ex y Lambs on the Road como en la ultima referencia del usuario. Los textos de historia se conservan completos, sin resumir.
- Press: 15 menciones de prensa, cuatro presskits con botones de descarga visibles y destacados.
- Los 32 juegos tienen fichas propias con presentacion, capturas, compra, descripcion completa, caracteristicas, trailer, resenas pertinentes y juegos relacionados, respetando el orden de las fichas originales y la identidad visual actual.

El pie conserva el orden del original: Find us everywhere, Better news y Send prayers. Las redes y resenas pueden abrir paginas externas, pero no son necesarias para que la web cargue o funcione. No se enlaza al sitio original. El correo de contacto abre el programa de correo configurado mediante un enlace mailto.

## WordPress

Subir `flynns-arcade-neon-wordpress-theme.zip` desde Apariencia > Temas > Anadir nuevo > Subir tema y activar.

La misma estructura PHP genera la vista previa y el tema. La navegacion WordPress utiliza parametros propios de consulta, compatibles tambien con instalaciones sin enlaces permanentes personalizados. No crea ni modifica paginas, entradas o datos existentes al activar el tema.

## Archivos principales

- `front-page.php`: seleccion de pagina.
- `templates/`: Inicio, Games, catalogo compartido, Indies, About, Press y fichas de juegos.
- `inc/catalog.json`: los 32 juegos en el orden de la web original.
- `inc/game-details.json`: textos completos, capturas locales, trailers, enlaces de producto y juegos relacionados de las 32 fichas.
- `inc/card-previews.json`: animaciones locales de las tarjetas de Inicio y Games.
- `inc/game-overrides.json`: correcciones verificadas de tiendas y videos, incluida la exclusion del video de Gwen copiado en Papertris.
- `inc/editorial.json`: estudios, historia, prensa y contacto.
- `inc/site.json`: portada, destacados, redes y textos comunes.
- `inc/structure.json`: correspondencia de imagenes locales y referencias a resenas.
- `inc/template-tags.php`: cabecera visual, navegacion, tarjetas, plataformas y destacados compartidos.
- `style.css`: presentacion responsive; tres columnas en ordenador, dos en tableta y una en movil.
- `assets/js/theme.js`: menu movil, filtros, buscador, Load more, destacados y animaciones de tarjetas bajo demanda.

## Imagenes y fuentes

Todos los recursos visuales y tipograficos se incluyen localmente. El nuevo logo pixel de Publishing, proporcionado por el usuario, se conserva sin modificar en `assets/images/brand/flynns-publishing-pixel.png`. La cabecera y About comparten el mismo componente: recorta visualmente los margenes negros del PNG con CSS, sin deformar ni regenerar sus letras. La cabecera conserva su espacio de 170 x 90 px (150 x 80 px en movil). Los cuatro presskits incluyen tambien este PNG. `assets/images/hero-arcade-kid.png` es una copia sin alterar del archivo proporcionado por el usuario. Los retratos y banners usan, cuando estan disponibles, los tamanos web publicados por el propio sitio.

Todas las paginas, incluidas las fichas de juegos, comparten un fondo pixel-art de maquinas arcade y ciudad nocturna en `assets/images/backgrounds/`. La imagen es propia de esta propuesta y se ha creado con la herramienta integrada de generacion de imagenes. Se sirve en WebP, con un recorte mas ligero para movil. Las capas oscuras protegen la lectura y las tarjetas siguen siendo opacas. No hay animaciones ni dependencias remotas. El PNG original, el prompt y la procedencia se conservan en `docs/fondo-arcade.md` y `docs/assets/`, fuera del tema instalable.

La portada utiliza `assets/images/hero-arcade-banner.webp` (2172 x 724 px), una version panoramica creada con la herramienta integrada a partir de la imagen del nino. No tiene limite de 742 px ni paneles laterales: ocupa todo el ancho y mantiene su altura proporcional, tambien en movil. La escena se muestra completa sin usar `cover` ni estirarla. El archivo adjunto inicial sigue intacto; el nuevo PNG maestro y el prompt se conservan en `docs/assets/hero-arcade-banner-original.png` y `docs/banner-arcade.md`. `tools/prepare-arcade-banner.cjs` prepara el WebP sin recortar la imagen.

Las seis fotos de About se incluyen en su orden original en `assets/images/about/`. La galeria admite ampliar fotos, avanzar, retroceder y cerrar con Escape; no utiliza servicios externos. Las ilustraciones anteriores de Projects permanecen guardadas en WebP; las tarjetas actuales utilizan las mismas portadas locales que el catalogo.

La galeria utiliza un filtro naranja neon inspirado en el nuevo logo. Al pasar el cursor o enfocar una foto se recupera su color original; las ampliaciones se muestran sin filtro. Los tres encabezados de historia tienen texto y borde rosa con esquinas rectas. Los proyectos muestran portada, titulo, generos y plataformas en dos columnas en escritorio y una en movil, reutilizando el componente del catalogo.

Projects utiliza un bloque centrado de hasta 820 px, con tarjetas medianas de unos 400 px de ancho en escritorio y un interior mas compacto. No se altera el tamano de las tarjetas de Games.

Los iconos de herramientas proceden de Lucide; los de plataformas y redes, de Simple Icons. Switch, Xbox, PlayStation y PC comparten el mismo magenta. Los circulos de redes miden 44 px en la cabecera y 56 px en el pie. Las tipografias locales son Barlow y Press Start 2P. Se incluyen sus licencias.

La franja de redes del pie tiene una capa negra al 65% que mejora la lectura sin ocultar por completo la ilustracion del fondo.

## Presskits y suscripcion

Los cuatro ZIP de `downloads/` son kits locales basicos preparados con las imagenes publicas, el logo y la informacion de contacto. No son una copia de las carpetas de Google Drive del sitio original y no dependen de ellas.

La newsletter conserva el formulario y la presentacion. Su proveedor de correo sigue pendiente de configurar. La vista previa no guarda ni envia direcciones, y comunica que las suscripciones no estan abiertas.

## Regenerar y comprobar

En la carpeta superior del proyecto:

- `php tools/render-preview.php`: regenera las cinco paginas y las 32 fichas a partir de las plantillas WordPress.
- `tools/build-presskits.ps1`: reconstruye los cuatro ZIP locales.
- `node tools/check-preview.cjs`: revisa navegacion, recursos, filtros, paginacion y distribucion en varias pantallas, bloqueando las conexiones HTTP externas.
- `node tools/check-background-contrast.cjs`: comprueba el contraste del texto sobre los fondos renderizados de escritorio y movil.
- `node tools/capture-about-reference.cjs`: consulta la pagina de referencia y guarda las fotos e ilustraciones y una copia de verificacion en `checks/`; solo es necesario para revisar de nuevo la fuente, no para utilizar la web.
- `docs/estructura-y-entrega.md`: comparacion de estructura para la entrega al instructor.
- `node tools/check-game-pages.cjs`: comprueba los 32 juegos en escritorio y movil, otras resoluciones representativas y los controles de capturas y trailers. Genera `docs/revision-fichas.md`.
- `node tools/import-card-previews.cjs`: guarda los GIF propios de las fichas de referencia y prepara versiones WebP animadas locales.
- `node tools/check-card-previews.cjs`: comprueba hover, teclado, movimiento real, restauracion de portadas, carga bajo demanda, movimiento reducido, enlaces tactiles y errores de carga.

## Tarjetas animadas

En Inicio y Games, las tarjetas de 31 juegos muestran su GIF original al pasar el raton y recuperan su portada al salir. Eternum Ex conserva la portada fija: el usuario confirmo que todavia no tiene GIF. Las animaciones se incluyen en `assets/images/previews/` como WebP animados optimizados, con la duracion original; no consultan la web de referencia ni se descargan al entrar en la pagina.

Las tarjetas mantienen las mismas dimensiones y enlaces. El foco visible de teclado activa la misma vista previa; al perderlo se restaura la portada. En pantallas tactiles no se introduce una pulsacion adicional: se abre el juego directamente. Se respeta la preferencia de reducir movimiento. Si una animacion falla al cargar, permanece la portada, sin imagen rota. About y las tarjetas de juegos relacionados conservan su comportamiento anterior.

Los GIF originales y el informe de procedencia se guardan fuera del tema, en `checks/card-preview-originals/` y `checks/card-preview-import.json`. Ver `docs/tarjetas-animadas.md`.

## Fichas de juegos

La revision 2.7 aplica a los 32 juegos la distribucion aprobada en What Comes After: presentacion con video superior, cinco capturas ampliables, compra, descripcion y caracteristicas con trailer, resenas y relacionados. Los textos originales se mantienen completos. Las imagenes se guardan como WebP locales en `assets/images/game-details/` y se muestran sin cortar. Las capturas de Galacticon pasan de su antiguo carrusel a la galeria inferior, sin perder ninguna.

Se elimina la rama y los estilos del antiguo carrusel superior: todas las fichas utilizan el mismo componente de video y la misma galeria. El carrusel de resenas comparte el controlador de los destacados. La galeria utiliza el visor accesible de About, con controles y teclado. Las resenas son extractos breves enlazados a sus medios originales; no se muestran citas que corresponden a otro juego. Los enlaces de compra se contrastan con los del catalogo, evitando enlaces copiados de otra ficha. Steam para Donut Dodo y Colored Effects se corrige con sus paginas oficiales.

La asignacion de videos se extrae de su posicion en la fuente, no solo del primer enlace encontrado. What Comes After mantiene reacciones arriba y trailer junto a Features; Eternum Ex, gameplay y trailer final; Galacticon, sus dos videos propios. Se respetan los segundos iniciales publicados. Si la ficha solo tiene un video propio, se reutiliza en ambos espacios para mantener la estructura solicitada sin inventar otro. Papertris utiliza su video correcto en ambos y excluye el enlace ajeno de Legend of Gwen que aparecia en el original.

Las paginas y las capturas funcionan sin la web original. Los videos se cargan desde YouTube solo tras pulsar reproducir en una pagina HTTP/HTTPS, con la politica de Referer recomendada y el origen real de la pagina. Sin JavaScript o al abrir un archivo directamente, el enlace abre YouTube. Cada reproductor conserva tambien un acceso directo a YouTube si el servicio rechaza la reproduccion insertada. La reproduccion real necesita conexion y depende de la disponibilidad del video externo. Las tiendas y las resenas son enlaces de salida opcionales. No se importa ni se ejecuta codigo del sitio de referencia.

`node tools/check-video-players.cjs http://127.0.0.1:4173/` comprueba los dos reproductores de los 32 juegos en escritorio y movil, mas resoluciones representativas, el Referer real, los segundos iniciales y la estabilidad de la pagina. Contrasta los videos con su fuente y sus titulos/canales mediante oEmbed de YouTube. Requiere una vista HTTP en esa direccion. Registra por separado una muestra de reproduccion real: crear un iframe correctamente no demuestra que YouTube permita reproducir un video.

`docs/fichas-de-juegos.md` recoge las decisiones de estructura y la procedencia. `checks/game-reference/` conserva las referencias, los recursos importados y las incidencias detectadas. Para repetir la importacion: `capture-game-pages.cjs`, `parse-game-pages.cjs`, `import-game-assets.cjs` y `render-preview.php`, en ese orden.

Las herramientas JavaScript utilizan las dependencias del entorno local de Codex indicadas en sus importaciones. No hacen falta para abrir las paginas o instalar el tema.
