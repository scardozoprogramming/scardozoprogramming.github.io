# Diez fichas de juegos Cardozo

## Estado real

Preparadas localmente; **todavía no aplicadas ni verificadas en WordPress**.

La conexión de control con Edge ha dejado de responder. Abrir WordPress y proporcionar el enlace a la pestaña no ha recuperado esa conexión. Está pendiente que el usuario reconecte la extensión al chat. No se ha modificado ninguna de estas diez páginas durante esta preparación.

## Fichas preparadas

- BoardWalk Builders Cardozo → `boardwalk-builders`
- Cash Cow DX Cardozo → `cash-cow-dx`
- Colored Effects Cardozo → `colored-effects`
- Dexter Cardozo → `dexter-stardust`
- Donut Dodo Cardozo → `donut-dodo`
- Eternum Ex Cardozo → `eternum-ex`
- Explosive Dinosaurs Cardozo → `explosive-dinosaurs`
- Flora & Fang: Guardians of the vampire garden Cardozo → `flora-fang-guardians-of-the-vampire-garden`
- Freddy Farmer Cardozo → `freddy-farmer`
- Galacticon Cardozo → `galacticon`

Cada carpeta `Juegos/<juego>/Elementor` incluye:

- `plantilla.json`: exportación nativa de Elementor derivada de **Cardozo · Game page**.
- `portapapeles.json`: los mismos bloques en formato de pegado de Elementor.

El diseño, el soporte y la cabecera compartida 7123 proceden de la plantilla base. Los textos, imágenes, vídeo, plataformas, tiendas y reseñas corresponden a cada juego. Se conserva el orden de los bloques informativos. Las fichas sin reseñas omiten tanto esa sección como su enlace de navegación.

Nueve animaciones utilizan los WebP optimizados previamente subidos a WordPress. Explosive Dinosaurs utiliza el GIF existente en la web original. Sus cinco capturas, portada, vídeo y textos se recuperaron de https://www.flynnsarcades.com/explosive-dinosaurs/. El HTML público original se conserva en su carpeta Fuentes.

## Pendiente al recuperar la conexión

1. Localizar las diez páginas por sus títulos exactos, registrar sus identificadores y conservar su contenido previo para poder recuperarlo.
2. Aplicar los bloques nativos preparados, evitando duplicar la ficha anterior. Establecer Elementor Canvas.
3. Guardar borrador en cada página. No publicar nuevos juegos ni modificar los otros cuatro ya realizados.
4. Revisar cada ficha en su vista previa: título y materiales correctos, una sola cabecera, tiendas, anclas, capturas, animación y vista móvil. Verificar el vídeo sin afirmar reproducción si el reproductor incrustado sigue sin responder.
5. Guardar evidencias y actualizar `preparacion.json` con el estado real de cada página.

El generador está en `WordPress/Plantilla-Juegos/Elementor/generar-lote.cjs`. Regenera las exportaciones desde los JSON de los juegos y la plantilla local. **Ejecutarlo no modifica WordPress.**
