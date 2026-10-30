// Build the editable About Cardozo draft (6925). Does not change other pages.
const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
const home = path.resolve(base, '../Inicio-Cardozo');
const games = path.resolve(base, '../Games-Cardozo');
for (const dir of ['CSS', 'JavaScript', 'HTML', 'Elementor']) fs.mkdirSync(path.join(base, dir), { recursive: true });
const read = (dir, file) => fs.readFileSync(path.join(dir, file), 'utf8');
const write = (file, value) => fs.writeFileSync(path.join(base, file), value);
const source = JSON.parse(read(home, 'Elementor/Inicio-Cardozo-bloques.json'));
const reference = JSON.parse(read(base, 'Elementor/referencia.json'));
const clone = value => JSON.parse(JSON.stringify(value));
const walk = node => [node, ...(node.elements || []).flatMap(walk)];
const zero = { unit:'px', top:'0', right:'0', bottom:'0', left:'0', isLinked:true };
let seed = 0xfc70000;
const id = () => (++seed).toString(16);
const link = (url, attrs = '') => ({ url, is_external:'', nofollow:'', custom_attributes:attrs });
const widget = (type, title, settings = {}, classes = '') => ({ id:id(), elType:'widget', widgetType:type, settings:{ _title:title, _css_classes:classes, _margin:zero, ...settings }, elements:[] });
const column = (title, elements, size = 100) => ({ id:id(), elType:'column', isInner:false, settings:{ _title:title, _column_size:size, padding:zero }, elements });
const section = (title, elements, classes, full = false) => ({ id:id(), elType:'section', isInner:false, settings:{ _title:title, layout:full ? 'full_width' : 'boxed', content_width:{ unit:'px', size:1260 }, gap:'no', padding:zero, css_classes:'fc-home ' + classes }, elements });
const setup = clone(source.content[0]);
setup.settings._title = '00 · Design & interactions';
setup.settings.padding = zero;
const header = clone(source.content[1]);
header.settings._title = '01 · About header';
for (const item of walk(header).filter(n => n.settings._css_classes?.includes('fc-home-menu-item'))) {
  const active = item.settings.text === 'About';
  item.settings._css_classes = 'fc-home-menu-item' + (active ? ' fc-active' : '');
  item.settings.link.custom_attributes = active ? 'aria-current|page' : '';
  if (active) item.settings.link.url = '#about-studio';
}
const logo = clone(walk(header).find(n => n.settings._css_classes === 'fc-home-logo'));
logo.settings._title = 'Publishing logo · Studio';
logo.settings._css_classes = 'fc-about-logo';
logo.settings.link_to = 'none';
delete logo.settings.link;
const brand = section('02 · Studio logo', [column('Studio identity', [
  widget('heading', 'About · Accessible page title', { title:'About', header_size:'h1' }, 'fc-about-title'), logo
])], 'fc-about-brand', true);
const photos = reference.photos.map((photo, i) => widget('image', `Photo ${i + 1} · ${photo.alt}`, {
  image:{ id:photo.id, url:photo.url }, image_size:'medium', caption_source:'none',
  link_to:'custom', link:link(photo.url, 'aria-label|' + photo.alt + ',data-elementor-lightbox-slideshow|fc-about-photos,data-elementor-lightbox-title|' + photo.alt), open_lightbox:'yes'
}, 'fc-about-photo'));
const gallery = section('03 · Studio photos · 6 editable images', [column('Studio gallery', photos)], 'fc-about-gallery', true);
const escape = text => text.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const story = section('04 · Description / Early history / After that', reference.story.map(item => column(item.title, [
  widget('heading', item.title, { title:item.title, header_size:'h2' }, 'fc-story-heading'),
  widget('text-editor', item.title + ' · Story', { editor:item.paragraphs.map(p => '<p>' + escape(p) + '</p>').join('') }, 'fc-about-copy')
], 33)), 'fc-about-story', true);
const catalogCards = source.content[5].elements[0].elements.filter(n => n.settings.css_classes?.includes('fc-card'));
const projectCard = title => clone(catalogCards.find(n => walk(n).some(w => w.settings._css_classes === 'fc-card-title' && w.settings.title === title)));
const eternum = projectCard('Eternum Ex');
const lambs = projectCard('Lambs on the Road');
const saga = clone(lambs);
const sagaUrl = 'https://store.steampowered.com/app/2590820/Saga/?l=english';
const sagaImage = walk(source.content[4]).find(n => n.widgetType === 'image' && n.settings.image?.url.includes('project-saga'));
saga.settings._title = 'Project · Saga';
saga.settings.css_classes = 'fc-card fc-project-saga';
for (const node of walk(saga)) {
  if (node.settings._css_classes === 'fc-cover') {
    node.settings._title = 'Saga · Cover';
    node.settings.image = clone(sagaImage.settings.image);
    node.settings.link = link(sagaUrl, 'aria-label|View Saga on Steam,data-fc-preview|https://www.flynnsarcades.com/wp-content/uploads/2026/10/fc-home-saga-hover.webp');
  }
  if (node.settings._css_classes === 'fc-card-title') { node.settings.title = 'Saga'; node.settings.link = link(sagaUrl); }
  if (node.settings._css_classes === 'fc-tags') {
    node.settings.icon_list.forEach((item, i) => { item.text = i ? 'Action' : 'Platform'; });
  }
  if (node.settings._css_classes?.trim() === 'fc-platforms') {
    node.settings.icon_list = node.settings.icon_list.filter(item => item.text === 'PC');
    node.settings.icon_list.forEach(item => { item.link = link(sagaUrl); });
  }
  if (node.settings._css_classes === 'fc-card-arrow') { node.settings.text = 'View Saga on Steam'; node.settings._title = 'View Saga on Steam'; node.settings.link = link(sagaUrl); }
  if (node.settings.link?.url === sagaUrl) { node.settings.link.is_external = 'on'; node.settings.link.custom_attributes += (node.settings.link.custom_attributes ? ',' : '') + 'rel|noopener noreferrer'; }
}
[eternum, lambs, saga].forEach((card, i) => {
  const summaries = ['Hell is yours, the Arcade is ours', 'When hope is the last thing to be lost', "A Viking girl's quest for revenge."];
  card.elements[0].elements.splice(2, 0, widget('text-editor', 'Project summary', { editor:'<p>' + escape(summaries[i]) + '</p>' }, 'fc-project-summary'));
});
const projects = section('05 · Projects · Eternum / Lambs / Saga', [column('Projects', [
  widget('heading', 'Projects title', { title:'Projects', header_size:'h2' }, 'fc-projects-title'), eternum, lambs, saga
])], 'fc-about-projects');
const footer = clone(source.content.slice(6));
footer.forEach((s, i) => { s.settings._title = ['06 · Find us everywhere', '07 · Better news & Send prayers', '08 · Footer credits'][i]; });
for (const node of footer.flatMap(walk)) if (node.settings.text === 'About the studio') node.settings.link.url = '#about-studio';
const content = [setup, header, brand, gallery, story, projects, ...footer];
for (const node of content.flatMap(walk)) { node.id = id(); delete node.settings._element_id; }
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
    const selector = css.slice(pos, open).trim(), body = css.slice(open + 1, end - 1);
    if (selector.startsWith('@media')) { const nested = pruneStyles(body); if (nested.trim()) output += selector + ' {\n' + nested + '}\n'; }
    else if (!/fc-(?:catalog|filter|search|empty|loadmore)/.test(selector)) output += selector + ' {' + body + '}\n';
    pos = end;
  }
  return output;
}
const shared = pruneStyles(read(games, 'CSS/compartido.css')).replaceAll('6953', '6925').replaceAll('GAMES · DESIGN', 'ABOUT · DESIGN');
const fonts = read(home, 'CSS/fuentes-optimizadas.css');
const css = (shared + '\n' + read(base, 'CSS/about.css')).replaceAll('[data-elementor-id="6925"]', 'body.page-id-6925 [data-elementor-id="6925"]').replaceAll('body.page-id-6925 body.page-id-6925', 'body.page-id-6925').replaceAll('.elementor-editor-active body.page-id-6925', 'body.elementor-editor-active.page-id-6925');
const loader = read(home, 'JavaScript/preview-loader.js');
const navigation = read(home, 'JavaScript/home-github.js').replaceAll('6876', '6925').replaceAll('fc-home-navigation', 'fc-about-navigation').replace(/    const banner[^\n]*\n    if \(banner[^\n]*\n/, '');
const app = read(base, 'JavaScript/about.js');
const support = '<style>\n' + fonts + '\n' + css + '</style>\n<script>\n' + loader + '\n' + app + '\n' + navigation + '</script>';
setup.elements[0].elements = [setup.elements[0].elements[0]];
setup.elements[0].elements[0].settings._title = 'About · Styles, fonts & interactions';
setup.elements[0].elements[0].settings.html = support;
write('CSS/about-cardozo.css', css);
write('CSS/fuentes-optimizadas.css', fonts);
write('JavaScript/preview-loader.js', loader);
write('JavaScript/navegacion.js', navigation);
write('HTML/About-Cardozo-soporte.html', support);
const template = { version:'0.4', title:'About Cardozo · Editable neon studio page', type:'page', content, page_settings:[] };
write('Elementor/About-Cardozo-bloques.json', JSON.stringify(template, null, 2));
write('Elementor/About-Cardozo-portapapeles.json', JSON.stringify({ type:'elementor', siteurl:'https://www.flynnsarcades.com/wp-json/', elements:content }));
const report = { pageId:6925, sections:content.length, widgets:content.flatMap(walk).filter(n => n.elType === 'widget').length, photos:photos.length, storyParagraphs:reference.story.reduce((n, s) => n + s.paragraphs.length, 0), projects:['Eternum Ex', 'Lambs on the Road', 'Saga'], supportBytes:Buffer.byteLength(support) };
write('Elementor/validacion-bloques.json', JSON.stringify(report, null, 2));
console.log(report);
