// Build only the editable draft Games Cardozo (6953), using the finished Home assets.
const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
const home = path.resolve(base, '../Inicio-Cardozo');
for (const dir of ['CSS', 'JavaScript', 'HTML', 'Elementor']) fs.mkdirSync(path.join(base, dir), { recursive: true });
const read = (dir, file) => fs.readFileSync(path.join(dir, file), 'utf8');
const write = (file, value) => fs.writeFileSync(path.join(base, file), value);
const source = JSON.parse(read(home, 'Elementor/Inicio-Cardozo-bloques.json'));
const reference = JSON.parse(read(base, 'Elementor/referencia-juegos.json'));
const clone = value => JSON.parse(JSON.stringify(value));
const walk = node => [node, ...(node.elements || []).flatMap(walk)];
let seed = 0xfc60000;
const id = () => (++seed).toString(16);
const zero = { unit: 'px', top: '0', right: '0', bottom: '0', left: '0', isLinked: true };
const setup = clone(source.content[0]);
const header = clone(source.content[1]);
const catalog = clone(source.content[5]);
const footer = clone(source.content.slice(6));
const title = {
  id: id(), elType: 'section', isInner: false,
  settings: { _title: '02 · Games heading', layout: 'boxed', content_width: { unit: 'px', size: 1260 }, gap: 'no', padding: zero, css_classes: 'fc-home fc-page-heading' },
  elements: [{ id: id(), elType: 'column', isInner: false, settings: { _title: 'Page title', _column_size: 100, padding: zero }, elements: [
    { id: id(), elType: 'widget', widgetType: 'heading', settings: { _title: 'Games · Page title', title: 'Games', header_size: 'h1', _css_classes: 'fc-page-title', _margin: zero }, elements: [] }
  ] }]
};
const content = [setup, header, title, catalog, ...footer];
const names = ['00 · Design & interactions', '01 · Games header', '02 · Games heading', '03 · Games · 32 editable cards', '04 · Find us everywhere', '05 · Better news & Send prayers', '06 · Footer credits'];
content.forEach((node, i) => { node.settings._title = names[i]; });
for (const node of content.flatMap(walk)) {
  node.id = id();
  delete node.settings._element_id;
  if (node.elType === 'section' && !node.isInner) node.settings.padding = zero;
}
for (const item of walk(header).filter(n => n.settings._css_classes?.includes('fc-home-menu-item'))) {
  const active = item.settings.text === 'Games';
  item.settings._css_classes = 'fc-home-menu-item' + (active ? ' fc-active' : '');
  item.settings.link.custom_attributes = active ? 'aria-current|page' : '';
  if (active) item.settings.link.url = '#games';
}
const items = catalog.elements[0].elements;
catalog.elements[0].elements = items.filter(n => !n.settings._css_classes?.includes('fc-catalog-title'));
const search = items.find(n => n.settings._css_classes === 'fc-search');
search.settings.html = '<label for="fc-games-search"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m16 16 5 5"/></svg><span class="fc-visually-hidden">Search games</span><input id="fc-games-search" data-fc-search type="search" placeholder="Search games" autocomplete="off" maxlength="100"></label><button type="button" data-fc-reset aria-label="Clear search and filters" title="Clear search and filters" hidden><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><path d="m6 6 12 12M18 6 6 18"/></svg></button>';
const cards = items.filter(n => n.settings.css_classes?.includes('fc-card'));
if (cards.length !== reference.length) throw new Error('Catalog count mismatch');
cards.forEach((card, i) => {
  const widgets = walk(card);
  const titleWidget = widgets.find(n => n.settings._css_classes === 'fc-card-title');
  if (titleWidget.settings.title !== reference[i].title) throw new Error('Catalog order mismatch: ' + i);
  card.settings.css_classes = 'fc-card ' + reference[i].categories.map(c => 'fc-category-' + c).join(' ');
  const tags = widgets.find(n => n.settings._css_classes === 'fc-tags');
  if (JSON.stringify(tags.settings.icon_list.map(t => t.text)) !== JSON.stringify(reference[i].tags)) throw new Error('Genre mismatch: ' + i);
  const platforms = widgets.find(n => n.settings._css_classes?.trim() === 'fc-platforms');
  const actualPlatforms = platforms.settings.icon_list.map(p => p.text.replace('Nintendo ', ''));
  if (JSON.stringify(actualPlatforms) !== JSON.stringify(reference[i].platforms)) throw new Error('Platform mismatch: ' + i);
});
// Omit rules for Home-only sections while preserving media queries and shared styles.
function pruneStyles(css) {
  css = css.replace(/\/\*[\s\S]*?\*\//g, '');
  let output = '', pos = 0;
  while (pos < css.length) {
    const open = css.indexOf('{', pos);
    if (open < 0) break;
    let depth = 1, quote = '', end = open + 1;
    for (; end < css.length && depth; end++) {
      const ch = css[end];
      if (quote) { if (ch === '\\') end++; else if (ch === quote) quote = ''; }
      else if (ch === '"' || ch === "'") quote = ch;
      else if (ch === '{') depth++;
      else if (ch === '}') depth--;
    }
    const selector = css.slice(pos, open).trim();
    const body = css.slice(open + 1, end - 1);
    if (selector.startsWith('@media')) {
      const nested = pruneStyles(body);
      if (nested.trim()) output += selector + ' {\n' + nested + '}\n';
    } else if (!/fc-(?:featured|promo|banner|copy|description|eyebrow)/.test(selector)) {
      output += selector + ' {' + body + '}\n';
    }
    pos = end;
  }
  return output;
}
const scope = value => value.replaceAll('6876', '6953').replaceAll('HOME · DESIGN', 'GAMES · DESIGN');
const sharedCSS = scope(pruneStyles(read(home, 'CSS/bloques.css') + '\n' + read(home, 'CSS/home-github.css')));
const fonts = read(home, 'CSS/fuentes-optimizadas.css');
const loader = read(home, 'JavaScript/preview-loader.js');
const navigation = scope(read(home, 'JavaScript/home-github.js')).replaceAll('fc-home-navigation', 'fc-games-navigation');
const app = read(base, 'JavaScript/catalogo.js');
const css = (sharedCSS + '\n' + read(base, 'CSS/catalogo.css')).replaceAll('[data-elementor-id="6953"]', 'body.page-id-6953 [data-elementor-id="6953"]').replaceAll('body.page-id-6953 body.page-id-6953', 'body.page-id-6953').replaceAll('.elementor-editor-active body.page-id-6953', 'body.elementor-editor-active.page-id-6953');
write('CSS/games-cardozo.css', css);
write('CSS/compartido.css', sharedCSS);
write('CSS/fuentes-optimizadas.css', fonts);
write('JavaScript/preview-loader.js', loader);
write('JavaScript/navegacion.js', navigation);
const support = '<style>\n' + fonts + '\n' + css + '</style>\n<script>\n' + loader + '\n' + app + '\n' + navigation + '</script>';
setup.elements[0].elements = [setup.elements[0].elements[0]];
setup.elements[0].elements[0].settings._title = 'Games · Styles, fonts & interactions';
setup.elements[0].elements[0].settings.html = support;
write('HTML/Games-Cardozo-soporte.html', support);
const template = { version: '0.4', title: 'Games Cardozo · Editable neon catalog', type: 'page', content, page_settings: [] };
write('Elementor/Games-Cardozo-bloques.json', JSON.stringify(template, null, 2));
write('Elementor/Games-Cardozo-portapapeles.json', JSON.stringify({ type: 'elementor', siteurl: 'https://www.flynnsarcades.com/wp-json/', elements: content }));
const report = { pageId: 6953, sections: content.length, widgets: content.flatMap(walk).filter(n => n.elType === 'widget').length, cards: cards.length, optimizedCovers: cards.length, animatedPreviews: cards.length, supportBytes: Buffer.byteLength(support), title: 'Games', source: 'https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Games/HTML/index.html' };
write('Elementor/validacion-bloques.json', JSON.stringify(report, null, 2));
console.log(report);
