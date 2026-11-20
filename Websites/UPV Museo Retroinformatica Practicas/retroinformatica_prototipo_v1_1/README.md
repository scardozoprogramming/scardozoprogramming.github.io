# Club de Retroinformática UPV — Prototipo navegable v1.1

## Qué cambia respecto a v1.0

Se ha revisado **todo el proyecto** para acercar la página de Inicio a la referencia visual validada provisionalmente por el tutor (archivo `referencia_visual_acordada.png`). La versión anterior se separaba de la captura en la cabecera, tipografía, paleta, monitor, proporciones de las tarjetas y distribución general.

**La nueva portada se reconstruyó con HTML y CSS reales**: el monitor, los sprites y las imágenes de muestra son recursos independientes; no se ha usado una captura completa como fondo de página. Se conserva la navegación y se extiende el mismo sistema gráfico a las demás páginas.

## Cómo probarlo

1. Descarga y descomprime **todo** el ZIP, manteniendo las carpetas `assets`, `css` y `js` junto a los cinco HTML.
2. Abre `retroinformatica_Inicio.html` en Chrome, Edge, Firefox o Safari.
3. Visita cada apartado desde el menú. El prototipo está preparado para escritorio y móvil.

**No es necesario instalar dependencias, Node, PHP ni levantar un servidor.** Puede abrirse directamente desde disco (`file://`). Las fuentes retro de Google Fonts son externas; si estás sin internet, se aplica una tipografía de respaldo, por lo que el aspecto de los títulos puede variar ligeramente. El contenido y la navegación seguirán funcionando.

## Páginas

- `retroinformatica_Inicio.html` — portada fiel a la referencia, máquinas, actividades y noticias.
- `retroinformatica_Actividades.html` — filtros, fichas de ejemplo y propuesta de actividad.
- `retroinformatica_Que_es_el_club.html` — presentación, Jugar / Preservar / Aprender, datos prácticos y acceso al formulario.
- `retroinformatica_Maquinas.html` — catálogo filtrable con fichas informativas.
- `retroinformatica_Unete.html` — formulario demostrativo accesible y preguntas frecuentes.

## Qué se puede probar

- Navegación completa; menú de móvil; enlaces entre secciones.
- Filtros de actividades y máquinas; diálogos informativos.
- Formulario con validación de campos, **sin envío ni almacenamiento** de datos personales.
- Teclado y foco visible, enlace para saltar al contenido, compatibilidad con reducción de movimiento.

## Dónde modificar el diseño

- `css/styles.css`: estilos heredados de v1.0, necesarios para los componentes y páginas internas.
- **`css/styles-v1-1.css`**: ajustes visuales definitivos de esta revisión. Se carga después y prevalece sobre los estilos anteriores.
- Cada `retroinformatica_*.html`: estructura y textos de su página.
- `js/site.js`: interacción de filtros, menú, fichas y formulario.
- `js/modal-data.js` y `js/data.json`: contenido demostrativo de los diálogos.
- `assets/logo-club.webp`: versión ligera del logo original entregado por el usuario.
- `assets/monitor-referencia.png`, `assets/referencia-*.png`: recortes de elementos gráficos del mockup aprobado, usados únicamente dentro de componentes web reales.
- `assets/sprite-*.svg`: ilustraciones SVG del prototipo previo, reutilizadas en páginas internas.

## Nota sobre contenido y publicación

Las actividades, fechas, programación, noticias, inscripciones y la presencia de máquinas concretas en el museo **son ejemplos sin validar**, no datos oficiales. Antes de su publicación será necesario: aprobación del director, disponibilidad y procedencia de las imágenes, revisión de la nomenclatura del club, contenido real, aviso legal y protección de datos, accesibilidad y conexión del formulario a un canal institucional.

Consulta `PUNTOS_PARA_REVISION.md` y `CAMBIOS_V1_1.md` para preparar la revisión.
