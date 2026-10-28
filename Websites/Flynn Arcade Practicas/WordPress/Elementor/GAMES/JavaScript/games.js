'use strict';
window.FlynnsArcadeElementor.register('games', (root, listen) => {
const FlynnsArcade = window.FlynnsArcadeElementor.behaviors;
FlynnsArcade.navigation(root, listen);
FlynnsArcade.catalog(root, listen);

});
