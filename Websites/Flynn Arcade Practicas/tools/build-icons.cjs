const fs = require('node:fs');
const path = require('node:path');
const lucide = require('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/lucide');
const names = ['ArrowRight', 'ChevronLeft', 'ChevronRight', 'X', 'Menu', 'MapPin', 'Mail', 'Search', 'Gamepad2'];
const symbols = names.map((name) => {
  const id = name.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase();
  const nodes = lucide[name].map(([tag, attributes]) => {
    const attrs = Object.entries(attributes).map(([key, value]) => `${key}="${value}"`).join(' ');
    return `<${tag} ${attrs}/>`;
  }).join('');
  return `<symbol id="icon-${id}" viewBox="0 0 24 24">${nodes}</symbol>`;
});
const directory = path.resolve(__dirname, '../flynns-arcade-neon/assets/icons');
fs.writeFileSync(path.join(directory, 'ui.svg'), `<svg xmlns="http://www.w3.org/2000/svg" class="icon-sprite" aria-hidden="true">${symbols.join('')}</svg>\n`);
fs.copyFileSync('C:/Users/santi/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/lucide/LICENSE', path.join(directory, 'LICENSE'));
