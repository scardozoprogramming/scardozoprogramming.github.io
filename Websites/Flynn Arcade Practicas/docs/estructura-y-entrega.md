# Estructura y entrega

Revision del 15 de octubre de 2026.

| Sitio original | Version independiente |
| --- | --- |
| Logo propio, Games, Indies, About y Press | Logo autentico, Inicio separado de Games y las paginas Indies, About y Press |
| Portada destacada de The Last Cat in the Universe | Mismo juego destacado con la imagen del nino solicitada por el usuario |
| Destacados de Eternum Ex, Dexter Stardust y What Comes After | Mismos tres destacados, con controles manuales y recursos locales |
| Filtros sobre el catalogo | All games, Narrative, Platform, Arcade, Point & Click, Puzzle y RPG |
| Galeria de tres columnas con seis juegos iniciales | Tres columnas en ordenador, dos en tableta y una en movil; seis juegos iniciales |
| Load more | Revela los siguientes seis juegos hasta llegar a los 32 |
| Indies con creadores, logos, retratos y biografias | Los mismos 16 nombres y juegos, recursos propios y descripciones resumidas |
| About con logo centrado, seis fotos, tres columnas de historia y Projects | Mismo orden, las seis fotos locales ampliables y textos completos; Projects utiliza tarjetas segun la ultima imagen del usuario |
| Press Media y presskits | Las mismas 15 menciones y cuatro bloques de kits locales |
| Find us everywhere | Redes oficiales verificadas, con iconos guardados localmente |
| Better news y Send prayers | Newsletter y contacto en el mismo orden |

## Identidad visual

Se conserva el uniforme oscuro, cyan y magenta de la imagen de referencia. El logo mantiene la identidad autentica de Flynn's Arcade. La imagen del nino es el archivo adjunto por el usuario, sin modificaciones.

La version 2.4 incorpora un fondo compartido de maquinas arcade y ciudad nocturna en todas las paginas y fichas. Se ha generado una ilustracion nueva coherente con la portada, guardada localmente y optimizada para escritorio y movil. Su intensidad se reduce detras del texto, sin cambiar el orden de las secciones ni el tamano mediano de Projects. Ver `fondo-arcade.md` para el original y el prompt de generacion.

Por peticion posterior del usuario, Games muestra solo el catalogo y sus controles; Inicio conserva la portada y los destacados. Todos los logos de plataformas usan el magenta de Switch. Los circulos de redes se amplian a 44 px en la cabecera y 56 px en el pie, tambien en movil.

About recupera la galeria fotografica que faltaba y sustituye los resumenes por los textos originales completos. En escritorio conserva seis fotos en una fila y las tres columnas de historia. Projects utiliza dos tarjetas con portada, titulo, generos y plataformas, segun la ultima referencia visual del usuario. En movil los proyectos se apilan. La galeria pasa a dos columnas en tableta y una en movil. Las fotos se pueden ampliar con controles locales y los proyectos enlazan a sus fichas propias.

Las fotos sin seleccionar llevan un filtro azul cyan de la paleta. El filtro desaparece con el cursor, el foco de teclado y al ampliar la imagen. Los encabezados Description, Early history y After that tienen letras y borde rosa, con esquinas rectas. Las portadas de los proyectos se muestran completas, con el texto y los logos de plataformas debajo.

## Independencia

La version local funciona al abrir un archivo HTML. Las imagenes, fuentes, estilos, scripts y datos estan incluidos en el proyecto. El catalogo y su paginacion no consultan WordPress ni la web de origen para cargar contenido.

Las redes oficiales y los articulos de prensa son enlaces de salida opcionales; no son dependencias de carga. Ningun enlace de navegacion devuelve al sitio original. El correo de contacto utiliza mailto para abrir la aplicacion de correo del visitante. Los presskits son ZIP locales basicos, ensamblados con el material publico disponible. La newsletter requiere conectar un proveedor de correo antes de poder recibir suscripciones reales.

## Formato de entrega

- Carpeta `flynns-arcade-neon/`: codigo, paginas estaticas y recursos.
- Archivo `flynns-arcade-neon-wordpress-theme.zip`: tema instalable.
- Carpeta `checks/`: capturas de comprobacion.

La vista previa y WordPress comparten las mismas plantillas; no se mantiene una segunda estructura de pagina independiente.
