# CSS para la cabecera existente de Elementor

Este ajuste corresponde a la captura con el logo Flynn's Arcade, los enlaces Games, Indies, About y Press, y los iconos de redes.

## Aplicar

1. Abre la cabecera con Elementor y selecciona la seccion o contenedor exterior que contiene el logo, el menu y las redes.
2. En **Avanzado > Clases CSS**, conserva las clases anteriores y anade `flynns-header`, sin punto. No lo introduzcas en el campo ID CSS.
3. Si el logo utiliza un widget Imagen normal, anade `flynns-header-logo` a ese widget para aplicar el limite de tamano. El widget Logo del sitio no necesita esa segunda clase.
4. En el mismo campo CSS de WordPress que utilizaste para el pie, anade el contenido de **cabecera.css** debajo del CSS existente. No sustituyas el bloque del pie ni anadas etiquetas `<style>`.
5. Guarda y revisa la cabecera en escritorio y movil antes de publicar. Si utilizas cache, actualizala despues de guardar.

## Resultado

- Fondo oscuro y linea inferior cian/rosa.
- Menu blanco, con cian al pasar el raton y en los enlaces que Elementor identifica como pagina actual.
- Redes con circulos oscuros, iconos cian y hover rosa.
- Tipografia sencilla y legible, manteniendo el logotipo como imagen.
- No se cambian los enlaces ni se sustituye el JavaScript del menu movil.

Barlow se utiliza si ya esta cargada en tu web; en caso contrario, Arial. No se descargan fuentes externas ni se reemplaza la imagen del logo. Los limites de tamano del logo no realizan recortes.

No hace falta anadir JavaScript para este ajuste visual. Conserva el codigo existente de Elementor y del resto de la web. Si el menu esta construido con enlaces individuales, este CSS no lo convierte automaticamente en un menu hamburguesa: utiliza el comportamiento adaptable del widget o contenedor que ya tengas configurado.

Los estilos solo afectan a descendientes de `flynns-header`. Se han validado sintacticamente, pero no se han aplicado a tu WordPress real. Si usas widgets de terceros con otras clases, podrian necesitar un selector adicional.

Para revertir, retira unicamente este bloque CSS y las nuevas clases.

Referencia oficial: https://elementor.com/help/css-classes-in-elementor/
