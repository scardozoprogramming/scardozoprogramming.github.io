'use strict';
window.FlynnsArcadeElementor.register('home', (root, listen) => {
const FlynnsArcade = window.FlynnsArcadeElementor.behaviors;
FlynnsArcade.navigation(root, listen);
FlynnsArcade.carousels(root, listen);
FlynnsArcade.catalog(root, listen);

});
