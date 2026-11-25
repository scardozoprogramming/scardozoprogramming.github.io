# Guía de mantenimiento y publicación

## 1. Reemplazar la carpeta anterior en GitHub

1. Haz una copia de seguridad del proyecto publicado.
2. Accede a `Websites/UPV Museo Retroinformatica Practicas/` dentro de tu repositorio.
3. Mantén la carpeta `retroinformatica_prototipo_v1_1` para no romper los enlaces públicos ya existentes.
4. Elimina de la carpeta publicada los antiguos directorios `css/`, `js/`, los archivos `CAMBIOS_V1_1.md`, `PUNTOS_PARA_REVISION.md` y los recursos viejos no usados.
5. Copia sobre esa carpeta los **cinco HTML**, `index.html`, `README.md`, `docs/` y `assets/` de este paquete. Comprueba que no quede otra carpeta anidada de nombre idéntico.
6. Confirma los cambios en el repositorio y, una vez desplegado GitHub Pages, recarga la web (Ctrl+F5 en escritorio).

## 2. Editar el proyecto

- **Colores, espaciados y tamaño de tarjetas:** `assets/css/styles.css`.
- **Textos y orden de bloques:** archivos `*.html`.
- **Contenido de ventanas de ficha:** `assets/js/content.js`.
- **Filtros, menú y validación de campos:** `assets/js/site.js`.
- **Máquinas:** `assets/img/machines/`. Arcade, Amstrad, Commodore y Spectrum comparten las mismas imágenes en Inicio, Máquinas y sus fichas emergentes. Si sustituyes una, usa la misma ruta en todas sus referencias.

## 3. Datos que requieren información real

- Fechas, plazas y disponibilidad de actividades: incorporar solo información confirmada por el club.
- Equipos expuestos: comprobar inventario, autorización y descripción institucional.
- Artículos: vincularlos a publicaciones auténticas cuando estén disponibles.
- Avisos legales, contacto, consentimiento y envíos de formulario: incorporar únicamente los datos y el servicio aprobados antes de activar inscripciones.

La web conserva intencionalmente los elementos retro aprobados: fondo oscuro, tipografía y amarillos discretos. No hay necesidad de generar nuevas ilustraciones para mantener la consistencia.
