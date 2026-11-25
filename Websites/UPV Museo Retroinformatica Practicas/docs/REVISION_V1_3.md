# Revisión V1.3

## Cambios
- Identidad retro aprobada conservada. Tarjetas de máquinas compartidas entre portada y catálogo, cabeceras, controles, tipografía, colores, espaciado y estados de foco armonizados.
- Enlace a catálogo desde Inicio; el torneo de arcade abre su propia ficha y aparece como propuesta en Actividades. No se asignan fechas no confirmadas.
- Filtros con ocultación explícita de tarjetas; estados activos, etiquetas accesibles y navegación adaptada a pantallas estrechas. Fichas alternativas sin JavaScript.
- Formulario exclusivamente demostrativo, sin campos con atributo name ni envío de datos. Deshabilitado sin JavaScript.
- Portada disponible directamente en index.html; se conserva la URL histórica. Dimensiones reales de imágenes rasterizadas, carga diferida de las imágenes secundarias, tipografías con display=swap y CSS depurado de selectores que no corresponden a los componentes del proyecto.

## Comprobaciones realizadas
- 6 documentos HTML (5 secciones más portada raíz): un H1, un enlace de navegación activo y ausencia de identificadores duplicados.
- 135 referencias locales y anclas comprobadas; 40 imágenes con texto alternativo y dimensiones; 33 controles de fichas con datos correspondientes.
- Sintaxis de los dos archivos JavaScript comprobada con Node.js. CSS analizado con PostCSS y tinycss2; se retiraron 134 reglas sin componentes correspondientes.

## Pendientes de validación
- Chromium del entorno bloquea tanto file:// como HTTP local con ERR_BLOCKED_BY_ADMINISTRATOR. No se han podido comprobar visualmente las páginas ni ejecutar las interacciones en navegador o Lighthouse. Antes de publicación, revisar en Chrome/Firefox/Safari y móviles, medir Lighthouse en GitHub Pages y confirmar contraste y teclado con pruebas reales.
- Las tipografías externas siguen dependiendo de Google Fonts; existe alternativa local. Los contenidos, inventario, calendario, datos legales y canal de contacto oficial requieren validación del club; no se ha inventado un servicio de inscripción.
