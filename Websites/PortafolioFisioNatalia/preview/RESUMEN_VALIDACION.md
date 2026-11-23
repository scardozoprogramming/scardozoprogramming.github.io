# Validación técnica local — V4.0

- Anchos comprobados en Chromium: **320, 360, 390, 768, 820, 1024, 1280 y 1440 píxeles**.
- Sin desbordamiento horizontal en esas anchuras.
- La portada y fotografía original se cargaron correctamente en todas.
- Un único encabezado H1 visible y portada semántica para escritorio y móvil.
- Menú móvil: apertura, atributo `aria-expanded` y cierre al navegar comprobados.
- Sin excepciones JavaScript detectadas en la prueba.
- Recursos internos del HTML y anclas comprobados: sin rutas faltantes.
- CV ATS: dos páginas PDF, texto seleccionable y enlaces internos clicables.
- Código fuente: un archivo CSS y un archivo JS, sin librerías de frameworks.

**Limitaciones:** pruebas realizadas con representación local de HTML y recursos incrustados en Chromium debido a restricciones de navegación en el entorno. No constituyen medición PageSpeed Lighthouse en una URL pública, prueba de red real ni certificación WCAG 2.2 AA. Verificar permisos de uso para materiales clínicos antes de publicar.
