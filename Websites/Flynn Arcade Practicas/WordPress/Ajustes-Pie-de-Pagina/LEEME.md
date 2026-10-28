# Ajustes del pie existente de Elementor

Estos estilos son para el pie mostrado en la captura: redes sobre la imagen arcade y una franja inferior con newsletter y contacto. No necesitan el plugin Flynn's Arcade ni sustituyen el contenido HTML de las secciones.

## Preparar las dos secciones

En Elementor, selecciona cada seccion o contenedor exterior del pie y abre **Avanzado > Clases CSS**. Conserva las clases que ya tenga y anade, sin puntos:

- Seccion superior de redes: `flynns-footer flynns-footer-social`
- Seccion inferior de newsletter y contacto: `flynns-footer flynns-footer-contact`

Si el correo es un encabezado y no un enlace mailto, anade tambien `flynns-footer-email` al widget de ese correo.

## Campo CSS de WordPress

Introduce el contenido de **pie-de-pagina.css**, sin etiquetas `<style>`. Si el sistema permite elegir ubicacion, carga el CSS en la cabecera para evitar que aparezca primero el estilo antiguo.

El CSS utiliza las clases estandar de los widgets de redes, encabezados y botones de Elementor. Afecta solo a las secciones a las que anadas las clases anteriores. No cambia el resto de la pagina.

Mantiene la imagen existente, oscurece la franja social con negro transparente, cambia los iconos a cian con hover rosa y utiliza azul oscuro para la franja inferior. Barlow se utilizara si ya esta disponible en tu web; en caso contrario se utiliza Arial, sin descargar nuevas fuentes.

## Campo JS de WordPress

**Dejalo vacio para este ajuste.** El diseno y los estados hover/foco se resuelven con CSS. Los enlaces siguen funcionando con su HTML y la newsletter conserva la validacion y el envio de su proveedor actual.

Si ya hay JavaScript de tu formulario, analitica u otras funciones en ese campo, no lo borres. Simplemente no anadas un script adicional para estos cambios visuales. No se simulan altas ni mensajes de suscripcion correcta.

## Comprobar antes de publicar

Guarda primero una copia del CSS que ya utilizas. Prueba el nuevo ajuste en la vista previa y revisa escritorio y movil. No cambies el destino ni el metodo del formulario; prueba la suscripcion con su proveedor. Si hay cache de WordPress o Elementor, actualizala tras guardar.

Para revertir este ajuste, retira solo este bloque CSS y las nuevas clases. No es necesario desinstalar Elementor ni cambiar de tema.

Los estilos se han validado sintacticamente. No se han aplicado a tu WordPress real; pueden necesitar un ajuste si tu pie utiliza widgets de un complemento distinto a los estandar de Elementor.

Referencia oficial: https://elementor.com/help/css-classes-in-elementor/
