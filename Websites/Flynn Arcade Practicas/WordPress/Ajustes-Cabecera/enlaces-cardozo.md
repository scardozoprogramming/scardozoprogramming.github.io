# Normalizador de enlaces internos Cardozo

Preparación local, sin automatización del navegador ni escrituras en WordPress. Solo se crean estos cuatro archivos: `enlaces-cardozo.cjs`, `enlaces-cardozo.js`, `rutas-fuentes-publicas.json` y esta explicación. No se modifican cabeceras existentes, exportaciones ni registros de progreso.

El alcance explícito es **41 destinos: cinco páginas principales y 36 juegos**. Point & Click queda excluido: destino 6984 y fuente 3436. El constructor rechaza destinos fuera de esos 41 IDs; nunca añade esa categoría ni publica páginas.

La captura pública actual contiene 44 fuentes originales y 31 destinos Cardozo publicados. Las diez fichas restantes (7294, 7296–7302, 7310 y 7311) necesitan registros verificados del padre. El JavaScript generado inicialmente contiene **solo los 31 destinos publicados comprobados**; debe regenerarse con los 41 registros antes de aplicar la cobertura completa.

La API se descubre en el `<link rel="https://api.w.org/">` de [la portada pública](https://www.flynnsarcades.com/). Se consulta [la colección pública de páginas](https://www.flynnsarcades.com/wp-json/wp/v2/pages), con paginación, `context=view`, `status=publish` y campos limitados. La captura guarda fecha, descubrimiento, IDs, títulos, slugs y enlaces; no inventa estados ni slugs de borradores. También documenta las redirecciones públicas que confirman HTTP y el dominio sin `www` como alias del mismo sitio.

## Invocación del padre

`verifiedTargets` debe ser un array de registros `{id, title, slug, status}` con **slug exacto** y estado WordPress (`publish`, `draft`, `pending`, `future` o `private`). Los IDs pueden ser números o `post-123`. Los inventarios de juegos aportan únicamente asociaciones y alias; sus slugs de juego no se toman como slugs de destino WordPress.

```js
const fs = require('node:fs');
const { build, normalizeHref } = require('./WordPress/Ajustes-Cabecera/enlaces-cardozo.cjs');
const snapshot = require('./WordPress/Ajustes-Cabecera/rutas-fuentes-publicas.json');

// Combinar por ID los destinos públicos y los registros exactos verificados en la UI.
// Los registros verificados más recientes prevalecen sobre la captura pública.
const byId = new Map(snapshot.publicTargets.map(row => [String(row.id), row]));
for (const row of verifiedTargets) byId.set(String(row.id).replace(/^post-/, ''), row);
const { config, script, diagnostics } = build([...byId.values()], snapshot.sources);
console.log(diagnostics);
fs.writeFileSync('./WordPress/Ajustes-Cabecera/enlaces-cardozo.js', script);

// Para futuras exportaciones nativas: normalizar cada URL de página conocida.
const canonical = normalizeHref(oldUrl, config, {
  baseUrl: snapshot.origin + '/', previewDrafts: false
});
```

La función `build` (también `buildNormalizer`) devuelve `{config, script, diagnostics}` y no escribe archivos ni accede a la red. `script` es JavaScript sin etiquetas `<script>`, listo para que el padre lo integre como soporte del widget HTML de la cabecera común **7123**, conservando su navegación existente. Esa integración no se realiza aquí.

El tercer argumento opcional admite `{metadata: snapshot, origin, origins, previewDrafts}`. `metadata` permite otra captura explícita con `sources`, `expectedTargets` y `localGameAliases`; `origins` añade orígenes internos expresamente autorizados. El alcance de 41 IDs permanece cerrado.

`diagnostics.matchedSources` explica cada fuente y destino; `unmatchedSources` muestra fuentes sin correspondencia; `targetsWithoutSource` identifica destinos sin fuente pública asociada; `missingInventoryTargets` muestra los IDs del alcance que faltan; `excludedSources` registra Point & Click. Las ambigüedades de título, ruta o ID provocan un error, nunca una elección silenciosa. Con los 41 destinos, las únicas fuentes sin correspondencia deberían ser `Press_Plantilla` (4100) y `Privacy policy` (6326); se conservan sus enlaces.

También se puede generar el archivo desde un JSON que ya contenga todos los destinos verificados:

```powershell
node WordPress/Ajustes-Cabecera/enlaces-cardozo.cjs --build ruta/a/destinos-verificados.json
node WordPress/Ajustes-Cabecera/enlaces-cardozo.cjs --check
```

`--refresh-public` actualiza únicamente la captura mediante GET públicos sin cookies ni credenciales; `--public` regenera únicamente el JavaScript con los destinos publicados de esa captura. El JSON de entrada de `--build` debe ser un array; no se fusiona automáticamente desde la CLI.

## Reglas y compatibilidad

- La portada `/` y `/inicio/` se asocian por la fuente pública 3231; `/about-3/` por 4032. No se añaden `/home/` ni `/about/` sin evidencia.
- Los títulos se comparan sin «Cardozo», diferencias de mayúsculas, signos o entidades HTML. `8-Colors Star Guardians + Cardozo` funciona sin alterar el slug exacto de entrada. La captura conserva literalmente el título que devuelve la API, aunque la UI ya muestre otra revisión.
- `/dexter/` y el alias local `/dexter-stardust/` llevan al slug exacto del destino 6939. `/spaceducks/` y `/space-ducks-the-great-escape/` se asocian al 7299 cuando esté verificado. BoardWalk/Boardwalk se compara sin distinguir mayúsculas. También se cubren `/sentrycity/` y el alias local `/sentry-city/`, además de `/flora-fang/` y su alias largo.
- Se registran los permalinks de los propios destinos. Un permalink de destino verificado prevalece sobre parámetros de enrutamiento antiguos. Los enlaces por `page_id`, `p`, `pagename`, `name` o `elementor-preview` se resuelven por la tabla conocida; un ID desconocido no se convierte accidentalmente en Inicio.
- Los publicados quedan en su URL canónica, sin `page_id` ni parámetros de preview antiguos. Se conserva el fragmento y, literalmente, el orden, codificación y repetición de parámetros ajenos al enrutamiento.
- Para borradores, el navegador usa `/?page_id=ID&preview=true` únicamente si `body.logged-in` está presente. Visitantes conservan el permalink exacto; eso no hace público el borrador. `previewDrafts:true/false` permite fijar el modo al construir; `normalizeHref` acepta el booleano equivalente en su tercer argumento. El formulario por ID facilita la revisión de borradores sin depender de que WordPress resuelva su slug.
- Se dejan intactos dominios externos, tiendas, redes, medios, descargas, `download`, `mailto:`, `tel:`, `javascript:`, fragmentos solos, rutas de administración, login, contenidos y REST. No se reescriben `src`, ni se sustituyen enlaces por manejadores de clic.

El script espera a DOM ready y solo cambia los `href` reales si existe `[data-elementor-id="7123"]` (marcador observado públicamente) o `#header7123`. No basta una clase genérica de cabecera. Un único observador atiende anclas añadidas y cambios de `href`/`download`; también reconoce una cabecera insertada posteriormente. Agrupa subárboles añadidos, evita escaneos completos repetidos y no vuelve a escribir un enlace que ya está normalizado. Una instancia idéntica no se registra dos veces.

Así, los widgets antiguos siguen siendo editables y sus URLs antiguas continúan funcionando mediante la cabecera compartida. El padre puede además guardar las URLs normalizadas directamente en futuras exportaciones nativas. **La configuración y el script deben regenerarse tras publicar o renombrar destinos**, y el padre debe actualizar el soporte de 7123 con ese nuevo resultado. No hay sincronización automática ni consulta REST desde el navegador.

Las comprobaciones usan solo Node (`assert`, `vm`): slugs deliberadamente distintos a los títulos, todas las asociaciones de las 41 páginas mediante fixtures de borrador solo en memoria, exclusión de Point & Click, título con `+`, queries, hashes, dominios, medios, preview, idempotencia, guardia de cabecera y observación de anclas. No sustituyen una comprobación visual futura del padre; aquí no se abre el navegador.
