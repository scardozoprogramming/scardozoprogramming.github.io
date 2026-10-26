# Flynn's Arcade Neon para WordPress

Esta carpeta es un tema completo de WordPress, independiente de la web HTML original. Incluye las cinco secciones, las 32 fichas de juegos, los 18 estudios, las animaciones, las fotos, las fuentes, los iconos y los materiales de prensa de la version actual.

## Organizacion

```text
WordPress/
  HOME/           PHP, CSS, JavaScript, JSON e Images
  GAMES/          PHP, CSS, JavaScript y JSON del catalogo
    Juegos/       Una carpeta por juego: PHP, JSON e Images
  INDIES/         PHP, CSS, JavaScript y JSON del directorio
    Estudios/     Una carpeta por estudio: PHP, JSON e Images
  ABOUT/          PHP, CSS, JavaScript, JSON e Images
  PRESS/          PHP, CSS, JavaScript, JSON y materiales de prensa
  COMPARTIDO/     PHP, CSS, JavaScript, JSON, Images, Fonts e Icons comunes
  functions.php   Entrada de las funciones del tema
  header.php      Cabecera y navegacion compartidas
  footer.php      Pie compartido
  index.php       Plantilla de respaldo de WordPress
  404.php         Pagina no encontrada
  style.css       Identificacion del tema y ajustes de WordPress
  theme.json      Paleta del editor
  screenshot.png  Vista previa del tema en Apariencia > Temas
```

El HTML esta dentro de las plantillas PHP, que es como WordPress lo sirve. No hacen falta versiones en Python, Java ni SQL, ni un servidor Node para esta carpeta.

## Instalar

1. Utiliza una instalacion de WordPress 6.3 o posterior, con PHP 7.4 o posterior. Es preferible una version con soporte vigente.
2. Sube esta carpeta completa a `wp-content/themes/WordPress/`. Tambien puedes comprimir la carpeta `WordPress` y subir ese ZIP desde Apariencia > Temas > Anadir nuevo > Subir tema. Si el hosting limita el tamano de subida, utiliza su gestor de archivos.
3. Activa **Flynn's Arcade Neon**. La portada y las cinco secciones ya funcionan, incluso antes de crear paginas: los enlaces alternativos utilizan parametros propios del tema.
4. Para tener paginas normales, crea Home, Games, Indies, About y Press. Asigna a cada una su plantilla **Flynn's Arcade: Home**, **Games**, **Indies**, **About** o **Press**. Las cinco aparecen en el selector de plantillas del editor.
5. En Ajustes > Lectura, selecciona Home como pagina de inicio. En Ajustes > Generales, utiliza el nombre Flynn's Arcade y el idioma English para mantener el contenido publico en ingles.
6. En Ajustes > Enlaces permanentes, selecciona una estructura como Nombre de la entrada y guarda. Las fichas usaran `/games/nombre-del-juego/`. Con enlaces simples tambien funcionan, usando un parametro del tema.

El menu detecta automaticamente las paginas publicadas que tienen asignada cada plantilla. No se crean, eliminan ni sobrescriben paginas existentes al activar el tema. El prefijo `/games/` se utiliza para las fichas de los juegos.

## Que editar

- El contenido y la maquetacion visibles estan en los archivos PHP de cada seccion. Los datos JSON conservan el inventario y la informacion de referencia; el catalogo JSON tambien identifica las fichas de juegos y sus titulos.
- Las tarjetas de Home y Games comparten `GAMES/PHP/catalog.php`, para evitar mantenerlas por duplicado.
- Cada estudio tiene su propia plantilla dentro de `INDIES/Estudios/` y cada juego su ficha dentro de `GAMES/Juegos/`.
- La paleta publica esta en `COMPARTIDO/CSS/site.css`; `theme.json` ofrece los mismos colores al editor.
- La integracion con WordPress y las rutas estan en `COMPARTIDO/PHP/wordpress.php`.

Las cinco secciones conservan la maquetacion en PHP y HTML: no son una conversion automatica a bloques Gutenberg ni a Elementor. El editor normal sigue disponible para otras paginas de WordPress mediante la plantilla de respaldo.

## Conservado

- Diseno neon, fondos arcade, logotipo, menu movil, accesibilidad y textos en ingles.
- Animaciones al pasar por las tarjetas de los 32 juegos, incluida la nueva muestra de Eternum.
- Un unico video superior por ficha; animacion inferior con pausa y respeto al movimiento reducido.
- Eternum, Lambs y Saga en los bloques de juegos desarrollados por Flynn's Arcade.
- Catcade despues de Pablo Navarro y siete estudios visibles inicialmente.
- Galerias, filtros, busquedas, carga progresiva, enlaces a tiendas, resenas y presskits.

No se incluye el nucleo de WordPress, una base de datos, usuarios, contrasenas ni un servicio de newsletter. El aviso de newsletter proxima se conserva: para suscripciones reales habra que conectar un servicio. El codigo del tema se distribuye bajo GPL-2.0-or-later; las imagenes, juegos, logotipos y fuentes mantienen los derechos y licencias de sus respectivos titulares, incluidos los avisos dentro de COMPARTIDO.

## Comprobaciones realizadas

- Instalacion local aislada con WordPress 7.1.3 y PHP 8.3.36.
- Las 37 paginas publicas conservan el texto completo de la version HTML; 343 recursos enlazados responden correctamente.
- Rutas con parametros y enlaces permanentes comprobados, incluidas respuestas 404 para juegos inexistentes o parametros invalidos.
- Busqueda del catalogo, animacion de tarjetas con teclado, pausa de animaciones, galeria ampliada, carga de estudios y menu movil verificados.
- Revision visual de las cinco secciones y una ficha en movil, sin desbordamiento horizontal.
- Los 464 archivos de la web original permanecen sin cambios.

Estas pruebas no publican ni migran una web existente. Antes de activarlo en un sitio real, comprueba tambien el alojamiento y los plugins que vayas a utilizar.
