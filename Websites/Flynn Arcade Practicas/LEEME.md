# Flynn's Arcade — versión independiente para GitHub Pages

Actualización del 19 de noviembre de 2026. Esta versión reproduce el contenido público actual de Flynn's Arcade con HTML, CSS y JavaScript locales. No necesita WordPress, Elementor, PHP, una base de datos ni conexión con flynnsarcades.com para funcionar.

## Organización

- `Home`, `Games`, `Indies`, `About`, `Press` y `Privacy`: una carpeta por página, organizada por HTML, CSS, JavaScript y JSON.
- `Games/Juegos`: 38 fichas individuales. El catálogo público actual contiene 35 tarjetas.
- `Indies/Estudios`: materiales y datos de cada estudio.
- `Compartido/HTML`: cabecera, catálogo, proyectos y pie compartidos.
- `Compartido/CSS`, `Compartido/JavaScript`, `Compartido/Images` y `Compartido/Fonts`: recursos comunes locales.
- `Compartido/JSON/Sincronizacion`: inventario, procedencia y resultados de las comprobaciones.

Hay 44 páginas en total. Las tiendas, redes sociales, vídeos de YouTube y servicios de suscripción mantienen sus enlaces externos propios. Los enlaces internos y los recursos visuales se sirven desde este proyecto.

## Ver la web en el ordenador

Desde esta carpeta, con Node.js instalado:

```powershell
node Compartido/JavaScript/server.cjs
```

El servidor muestra su dirección al arrancar y abre Home desde la raíz. Para elegir otro puerto:

```powershell
$env:PORT='4184'
node Compartido/JavaScript/server.cjs
```

La página principal está en `Home/HTML/index.html`. GitHub Pages sirve directamente estos archivos; no necesita ejecutar Node.js.

## Editar páginas y bloques compartidos

Edita el HTML, CSS o JavaScript de la página correspondiente. Se han conservado las clases del diseño de Elementor para reproducir su apariencia, pero no hace falta cargar su motor de WordPress.

Para cambiar un elemento común, edita **una sola vez** el archivo correspondiente:

| Bloque | Archivo |
|---|---|
| Cabecera y navegación | `Compartido/HTML/header.html` |
| Catálogo y filtros | `Compartido/HTML/catalogo.html` |
| Eternum, Lambs y Saga | `Compartido/HTML/proyectos.html` |
| Pie de página | `Compartido/HTML/footer.html` |

Después ejecuta:

```powershell
node Compartido/JavaScript/compilar-compartidos.mjs
```

Esto actualiza sus apariciones en las 44 páginas y conserva las modificaciones del contenido propio de cada página. No borres los comentarios `shared:…:start/end` que delimitan estos bloques. En los archivos compartidos, `@root/` representa la raíz del proyecto; el compilador lo convierte en la ruta relativa adecuada para cada página.

Los estilos y las interacciones comunes están en `Compartido/CSS/Sincronizado` y `Compartido/JavaScript/Sincronizado`. `wordpress-estatico.js` incorpora las funciones locales de navegación activa, acordeones y galería de imágenes.

## Comprobaciones

```powershell
node Compartido/JavaScript/verificar-estatico.mjs
```

Comprueba que los recursos locales existan y que no queden dependencias de la web original. El informe señala también archivos que no están referenciados; no los elimina automáticamente.

## Importación opcional de contenido

`sincronizar-wordpress.mjs` es una herramienta de mantenimiento para importar otra vez el contenido público original. **No se ejecuta al visitar la web ni es necesaria para editarla o publicarla.** Una nueva importación sustituye el contenido de las páginas locales y sus bloques; guarda antes tus cambios si quieres conservarlos.

```powershell
node Compartido/JavaScript/sincronizar-wordpress.mjs
```

## Limpieza y compatibilidad

Los archivos de versiones anteriores y duplicados retirados tienen un respaldo verificado fuera del repositorio. No se publica ese respaldo en GitHub.

Se conservan algunos recursos en sus rutas históricas porque la web pública actual todavía los utiliza desde GitHub. En particular, la carpeta `WordPress` contiene únicamente tres fuentes WOFF necesarias para esa compatibilidad. No contiene una segunda web ni una instalación de WordPress. Eliminar esas rutas rompería recursos de la web original.

Los datos de Privacy reflejan el documento público de origen. Su copia no constituye una nueva revisión jurídica.
