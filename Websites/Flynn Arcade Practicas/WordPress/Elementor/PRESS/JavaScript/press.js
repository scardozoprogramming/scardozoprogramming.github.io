'use strict';
window.FlynnsArcadeElementor.register('press', (root, listen) => {
const FlynnsArcade = window.FlynnsArcadeElementor.behaviors;
FlynnsArcade.navigation(root, listen);
FlynnsArcade.directory(root, listen);
FlynnsArcade.carousels(root, listen);

});
