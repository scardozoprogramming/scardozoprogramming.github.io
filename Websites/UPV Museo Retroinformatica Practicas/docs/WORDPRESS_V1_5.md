# Versión 1.5 — preparación para WordPress

## Sitio HTML
Conservar todos los archivos y carpetas; abrir index.html. La portada inicio.html también está disponible. Los seis sistemas comparten SVG transparente con encuadre ajustado y estilos comunes.

## WordPress
El ZIP de tema es **independiente**, no un plugin ni un archivo para pegar en Gutenberg. No reemplazar el tema institucional de la UPV sin autorización. Probar primero en staging.
1. En una instalación de pruebas que permita temas personalizados, Apariencia > Temas > Añadir nuevo > Subir tema. Seleccionar el ZIP `UPV_Retroinformatica_WordPress_V1_5.zip`. Activar solo en pruebas.
2. Crear y publicar cinco páginas con slugs exactos: `inicio`, `actividades`, `que-es-el-club`, `maquinas`, `unete`. Seleccionar Inicio como página estática en Ajustes > Lectura. El tema carga las cinco plantillas correspondientes por slug; usar los slugs indicados incluso si los enlaces permanentes son simples. El menú usa enlaces permanentes de páginas publicadas.
3. Verificar cada ruta y filtros, fichas, menú móvil y FAQ. Las plantillas del prototipo contienen HTML en `template-parts`, no bloques Gutenberg editables. Las demás páginas y entradas usan el editor habitual. Si se exige edición de esas cinco páginas desde Gutenberg o integración con un tema existente, deberá hacerse una adaptación a bloques o tema hijo según las reglas de la UPV.
4. El formulario es exclusivamente demostrativo: no envía ni guarda información. Actividades y fichas son contenidos de muestra. Validar inventario, fechas, contactos, privacidad y aviso legal antes de publicación. No activar un formulario real sin autorización y política de datos. Las fuentes Google Fonts son externas; comprobar política de privacidad, o servir fuentes localmente con licencias correspondientes.
5. El tema clásico es para pruebas privadas. No está certificado ni probado contra la instalación real de WordPress/UPV. PHP se valida sintácticamente y HTML se ensaya con recursos incorporados en Chromium. No hay auditoría Lighthouse real.

## Nombres definitivos
`inicio.html`, `actividades.html`, `que-es-el-club.html`, `maquinas.html`, `unete.html` e `index.html` en la versión estática. Los enlaces antiguos externos deberán actualizarse. En WordPress se usan slugs sin prefijo ni extensión.
