// Editable Indies Cardozo draft (6961). Other pages and shared templates are read-only.
const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
const home = path.resolve(base, '../Inicio-Cardozo');
const games = path.resolve(base, '../Games-Cardozo');
for (const dir of ['CSS', 'JavaScript', 'HTML', 'Elementor']) fs.mkdirSync(path.join(base, dir), { recursive:true });
const read = (dir, file) => fs.readFileSync(path.join(dir, file), 'utf8');
const write = (file, value) => fs.writeFileSync(path.join(base, file), value);
const source = JSON.parse(read(home, 'Elementor/Inicio-Cardozo-bloques.json'));
const reference = JSON.parse(read(base, 'Elementor/referencia.json'));
const media = JSON.parse(read(base, 'Elementor/medios-wordpress.json'));
const clone = value => JSON.parse(JSON.stringify(value));
const walk = node => [node, ...(node.elements || []).flatMap(walk)];
const zero = { unit:'px', top:'0', right:'0', bottom:'0', left:'0', isLinked:true };
let seed = 0xfc80000;
const id = () => (++seed).toString(16);
const link = (url, attrs = '') => ({ url, is_external:'', nofollow:'', custom_attributes:attrs });
const widget = (type, title, settings = {}, classes = '') => ({ id:id(), elType:'widget', widgetType:type, settings:{ _title:title, _css_classes:classes, _margin:zero, ...settings }, elements:[] });
const column = (title, elements, size = 100, classes = '') => ({ id:id(), elType:'column', isInner:false, settings:{ _title:title, _column_size:size, padding:zero, css_classes:classes }, elements });
const section = (title, elements, classes, tag = 'section') => ({ id:id(), elType:'section', isInner:false, settings:{ _title:title, layout:'boxed', content_width:{ unit:'px', size:1260 }, gap:'no', padding:zero, html_tag:tag, css_classes:'fc-home ' + classes }, elements });
const escape = value => value.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const para = (title, text, classes) => widget('text-editor', title, { editor:'<p>' + escape(text) + '</p>' }, classes);
const setup = clone(source.content[0]);
setup.settings._title = '00 · Design & interactions';
setup.settings.padding = zero;
const header = clone(source.content[1]);
header.settings._title = '01 · Indies header';
for (const item of walk(header).filter(n => n.settings._css_classes?.includes('fc-home-menu-item'))) {
  const active = item.settings.text === 'Indies';
  item.settings._css_classes = 'fc-home-menu-item' + (active ? ' fc-active' : '');
  item.settings.link.custom_attributes = active ? 'aria-current|page' : '';
  if (active) item.settings.link.url = '#independent-creators';
}
const heading = section('02 · We Love Indies · Page heading', [column('Page introduction', [
  para('Independent creators', 'Independent creators', 'fc-directory-eyebrow'),
  widget('heading', 'We Love Indies.', { title:'We Love <span>Indies.</span>', header_size:'h1' }, 'fc-directory-title')
])], 'fc-directory-heading');
const searchHTML = '<label for="fc-studio-search"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m16 16 5 5"/></svg><span class="fc-visually-hidden">Search studios or games</span><input id="fc-studio-search" data-fc-studio-search type="search" placeholder="Search studios or games" autocomplete="off" maxlength="100"></label><button type="button" data-fc-studio-clear aria-label="Clear search" title="Clear search" hidden><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><path d="m6 6 12 12M18 6 6 18"/></svg></button>';
const tools = section('03 · Search studios & games', [column('Directory tools', [
  widget('html', 'Search studios or games · input', { html:searchHTML }, 'fc-directory-search'),
  widget('heading', 'Studio counter', { title:'7 / 18 studios', header_size:'div' }, 'fc-directory-count')
])], 'fc-directory-tools');
const github = 'https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Indies/HTML/index.html';
const image = (studio, kind) => {
  const found = media.find(m => m.name === studio.name)?.[kind];
  const attachment = found?.class.match(/wp-image-(\d+)/)?.[1];
  return { id:attachment ? Number(attachment) : '', url:found?.src || new URL(studio[kind], github).href };
};
const rows = reference.map((studio, index) => {
  const slug = studio.logo.split('/')[2];
  const websites = studio.links.map(item => {
    const setting = link(item.url, 'rel|noopener noreferrer');
    setting.is_external = item.external ? 'on' : '';
    return widget('button', item.text, { text:item.text, link:setting, selected_icon:{ value:'fas fa-external-link-alt', library:'fa-solid' }, icon_align:'right' }, 'fc-studio-website');
  });
  return section(String(index + 4).padStart(2, '0') + ' · Studio · ' + studio.name, [
    column('Identity · ' + studio.name, [
      para('Games / location', studio.games, 'fc-studio-games'),
      widget('heading', studio.name, { title:studio.name, header_size:'h2' }, 'fc-studio-name'),
      widget('image', studio.name + ' · Logo', { image:image(studio, 'logo'), image_size:'medium', caption_source:'none', link_to:'none', open_lightbox:'no', align:'left' }, 'fc-studio-logo')
    ], 33, 'fc-studio-identity'),
    column('Portrait · ' + studio.name, [
      widget('image', studio.name + ' · Portrait', { image:image(studio, 'portrait'), image_size:'medium', caption_source:'none', link_to:'none', open_lightbox:'no' }, 'fc-studio-portrait')
    ], 33),
    column('Story · ' + studio.name, [
      para('Summary · ' + studio.name, studio.summary, 'fc-studio-summary'),
      widget('toggle', 'Full story · ' + studio.name, {
        tabs:[{ _id:id(), tab_title:'Full story', tab_content:studio.bio.map(p => '<p>' + escape(p) + '</p>').join('') }],
        selected_icon:{ value:'fas fa-caret-right', library:'fa-solid' }, selected_active_icon:{ value:'fas fa-caret-down', library:'fa-solid' }, title_html_tag:'div'
      }, 'fc-studio-bio'),
      ...websites
    ], 33, 'fc-studio-copy')
  ], 'fc-studio-row fc-studio-key-' + slug, 'article');
});
const more = section('23 · More studios', [column('Directory pagination', [
  widget('button', 'More studios', { text:'More studios', link:link('#more-studios', 'role|button'), selected_icon:{ value:'fas fa-plus', library:'fa-solid' }, icon_align:'right' }, 'fc-cta fc-directory-more-button')
])], 'fc-directory-more');
// Empty results must remain visible even when the pagination section is hidden.
const empty = section('22 · Search · No results', [column('Empty search results', [para('No results found', 'No results found.', '')])], 'fc-directory-empty');
const footer = clone(source.content.slice(6));
footer.forEach((s, i) => { s.settings._title = ['24 · Find us everywhere', '25 · Better news & Send prayers', '26 · Footer credits'][i]; });
const content = [setup, header, heading, tools, ...rows, empty, more, ...footer];
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
    else if (!/fc-(?:catalog|filter|search|empty|loadmore|card|cover|tags|platforms)/.test(selector)) output += selector + ' {' + body + '}\n';
    pos = end;
  }
  return output;
}
const shared = pruneStyles(read(games, 'CSS/compartido.css')).replaceAll('6953', '6961').replaceAll('GAMES · DESIGN', 'INDIES · DESIGN');
const fonts = read(home, 'CSS/fuentes-optimizadas.css').split('\n').filter(line => !line.includes('Flynns Cardozo Pixel')).join('\n');
const css = (shared + '\n' + read(base, 'CSS/indies.css')).replaceAll('[data-elementor-id="6961"]', 'body.page-id-6961 [data-elementor-id="6961"]').replaceAll('body.page-id-6961 body.page-id-6961', 'body.page-id-6961').replaceAll('.elementor-editor-active body.page-id-6961', 'body.elementor-editor-active.page-id-6961').replaceAll('body.elementor-editor-active.page-id-6961 body.page-id-6961', 'body.elementor-editor-active.page-id-6961');
const navigation = read(home, 'JavaScript/home-github.js').replaceAll('6876', '6961').replaceAll('fc-home-navigation', 'fc-indies-navigation').replace(/    const banner[^\n]*\n    if \(banner[^\n]*\n/, '');
const app = read(base, 'JavaScript/indies.js');
const support = '<style>\n' + fonts + '\n' + css + '</style>\n<script>\n' + app + '\n' + navigation + '</script>';
setup.elements[0].elements = [setup.elements[0].elements[0]];
setup.elements[0].elements[0].settings._title = 'Indies · Styles, fonts & interactions';
setup.elements[0].elements[0].settings.html = support;
write('CSS/indies-cardozo.css', css);
write('CSS/fuentes-optimizadas.css', fonts);
write('JavaScript/navegacion.js', navigation);
write('HTML/Indies-Cardozo-soporte.html', support);
const template = { version:'0.4', title:'Indies Cardozo · Editable neon studio directory', type:'page', content, page_settings:[] };
write('Elementor/Indies-Cardozo-bloques.json', JSON.stringify(template, null, 2));
write('Elementor/Indies-Cardozo-portapapeles.json', JSON.stringify({ type:'elementor', siteurl:'https://www.flynnsarcades.com/wp-json/', elements:content }));
const report = { pageId:6961, sections:content.length, widgets:content.flatMap(walk).filter(n => n.elType === 'widget').length, studios:rows.length, initialStudios:7, biographies:reference.length, paragraphCount:reference.reduce((n, s) => n + s.bio.length, 0), wordpressPortraits:media.filter(m => m.portrait).length, publicWebpImages:4, supportBytes:Buffer.byteLength(support) };
write('Elementor/validacion-bloques.json', JSON.stringify(report, null, 2));
console.log(report);
