# Flynn's Arcade Neon 2.4

Adaptacion independiente de la estructura de la web original de Flynn's Arcade, con la paleta oscura, cyan y magenta de la referencia visual. Se utiliza el logo autentico del estudio y la imagen del nino facilitada por el usuario.

## Abrir la entrega

Abrir `preview.html` o `index.html` directamente en el navegador. No requiere servidor ni conexion a la web original. Los menus llevan a paginas reales:

- Inicio: portada con la imagen del nino, destacados y catalogo.
- Games (`games.html`): pagina independiente dedicada al catalogo, con filtros, buscador y Load more, sin portada ni destacados.
- Indies: los 16 creadores y estudios con sus logos, imagenes y juegos.
- About: logo centrado, las seis fotos originales ampliables, Description, Early history y After that en tres columnas, y Projects con tarjetas de Eternum Ex y Lambs on the Road como en la ultima referencia del usuario. Los textos de historia se conservan completos, sin resumir.
- Press: 15 menciones de prensa, cuatro presskits descargables y destacados.
- Cada juego tiene una ficha local propia con descripcion y plataformas.

El pie conserva el orden del original: Find us everywhere, Better news y Send prayers. Las redes y resenas pueden abrir paginas externas, pero no son necesarias para que la web cargue o funcione. No se enlaza al sitio original. El correo de contacto abre el programa de correo configurado mediante un enlace mailto.

## WordPress

Subir `flynns-arcade-neon-wordpress-theme.zip` desde Apariencia > Temas > Anadir nuevo > Subir tema y activar.

La misma estructura PHP genera la vista previa y el tema. La navegacion WordPress utiliza parametros propios de consulta, compatibles tambien con instalaciones sin enlaces permanentes personalizados. No crea ni modifica paginas, entradas o datos existentes al activar el tema.

## Archivos principales

- `front-page.php`: seleccion de pagina.
- `templates/`: Inicio, Games, catalogo compartido, Indies, About, Press y fichas de juegos.
- `inc/catalog.json`: los 32 juegos en el orden de la web original.
- `inc/editorial.json`: estudios, historia, prensa y contacto.
- `inc/site.json`: portada, destacados, redes y textos comunes.
- `inc/structure.json`: correspondencia de imagenes locales y referencias a resenas.
- `inc/template-tags.php`: cabecera visual, navegacion, tarjetas, plataformas y destacados compartidos.
- `style.css`: presentacion responsive; tres columnas en ordenador, dos en tableta y una en movil.
- `assets/js/theme.js`: menu movil, filtros, buscador, Load more y destacados.

## Imagenes y fuentes

Todos los recursos visuales y tipograficos se incluyen localmente. El logo proviene del recurso publico de la web original. `assets/images/hero-arcade-kid.png` es una copia sin alterar del archivo proporcionado por el usuario. Los retratos y banners usan, cuando estan disponibles, los tamanos web publicados por el propio sitio.

Todas las paginas, incluidas las fichas de juegos, comparten un fondo pixel-art de maquinas arcade y ciudad nocturna en `assets/images/backgrounds/`. La imagen es propia de esta propuesta y se ha creado con la herramienta integrada de generacion de imagenes. Se sirve en WebP, con un recorte mas ligero para movil. Las capas oscuras protegen la lectura y las tarjetas siguen siendo opacas. No hay animaciones ni dependencias remotas. El PNG original, el prompt y la procedencia se conservan en `docs/fondo-arcade.md` y `docs/assets/`, fuera del tema instalable.

Las seis fotos de About se incluyen en su orden original en `assets/images/about/`. La galeria admite ampliar fotos, avanzar, retroceder y cerrar con Escape; no utiliza servicios externos. Las ilustraciones anteriores de Projects permanecen guardadas en WebP; las tarjetas actuales utilizan las mismas portadas locales que el catalogo.

La galeria utiliza el azul cyan de la paleta. Al pasar el cursor o enfocar una foto se recupera su color original; las ampliaciones se muestran sin filtro. Los tres encabezados de historia tienen texto y borde rosa con esquinas rectas. Los proyectos muestran portada, titulo, generos y plataformas en dos columnas en escritorio y una en movil, reutilizando el componente del catalogo.

Projects utiliza un bloque centrado de hasta 820 px, con tarjetas medianas de unos 400 px de ancho en escritorio y un interior mas compacto. No se altera el tamano de las tarjetas de Games.

Los iconos de herramientas proceden de Lucide; los de plataformas y redes, de Simple Icons. Switch, Xbox, PlayStation y PC comparten el mismo magenta. Los circulos de redes miden 44 px en la cabecera y 56 px en el pie. Las tipografias locales son Barlow y Press Start 2P. Se incluyen sus licencias.

## Presskits y suscripcion

Los cuatro ZIP de `downloads/` son kits locales basicos preparados con las imagenes publicas, el logo y la informacion de contacto. No son una copia de las carpetas de Google Drive del sitio original y no dependen de ellas.

La newsletter conserva el formulario y la presentacion. Su proveedor de correo sigue pendiente de configurar. La vista previa no guarda ni envia direcciones, y comunica que las suscripciones no estan abiertas.

## Regenerar y comprobar

En la carpeta superior del proyecto:

- `php tools/render-preview.php`: regenera las cinco paginas y las 32 fichas a partir de las plantillas WordPress.
- `tools/build-presskits.ps1`: reconstruye los cuatro ZIP locales.
- `node tools/check-preview.cjs`: revisa navegacion, recursos, filtros, paginacion y distribucion en varias pantallas, bloqueando las conexiones HTTP externas.
- `node tools/capture-about-reference.cjs`: consulta la pagina de referencia y guarda las fotos e ilustraciones y una copia de verificacion en `checks/`; solo es necesario para revisar de nuevo la fuente, no para utilizar la web.
- `docs/estructura-y-entrega.md`: comparacion de estructura para la entrega al instructor.

Las herramientas JavaScript utilizan las dependencias del entorno local de Codex indicadas en sus importaciones. No hacen falta para abrir las paginas o instalar el tema.
