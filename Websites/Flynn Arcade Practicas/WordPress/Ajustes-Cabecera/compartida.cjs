const fs = require('node:fs');
const path = require('node:path');
const base = __dirname;
const home = path.resolve(base, '../Inicio-Cardozo');
const master = path.join(base, 'Cabecera-compartida-plantilla.json');
const source = fs.existsSync(master) ? {type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:JSON.parse(fs.readFileSync(master)).content} : JSON.parse(fs.readFileSync(path.join(home, 'Elementor/Home-cabecera-portapapeles.json')));
const header = source.elements[0];
const walk = n => [n, ...(n.elements || []).flatMap(walk)];
const slugs = {Home:'inicio', Games:'games', Indies:'indies', About:'about', Press:'press'};
header.settings._title = 'Cardozo · Shared header';
header.settings.css_classes = 'fc-home fc-local-header fc-shared-header';
header.elements[0].elements = header.elements[0].elements.filter(n=>n.settings._css_classes!=='fc-shared-support');
for (const n of walk(header)) {
  if(n.settings._css_classes?.includes('fc-home-menu-item')) {
    n.settings._css_classes = 'fc-home-menu-item';
    n.settings.link.url = 'https://www.flynnsarcades.com/' + slugs[n.settings.text] + '-cardozo/';
    n.settings.link.custom_attributes = '';
  }
}
// Keep the established design while making the saved template independent of a page ID.
function rules(css) {
  css=css.replace(/\/\*[\s\S]*?\*\//g,'');
  let out='', p=0;
  while(p<css.length) {
    const o=css.indexOf('{',p);if(o<0)break;
    let d=1,q='',e=o+1;
    for(;e<css.length&&d;e++){const c=css[e];if(q){if(c==='\\')e++;else if(c===q)q='';}else if(c==='"'||c==="'")q=c;else if(c==='{')d++;else if(c==='}')d--;}
    const sel=css.slice(p,o).trim(), body=css.slice(o+1,e-1);
    if(sel.startsWith('@media')) {const nested=rules(body);if(nested)out+=sel+'{'+nested+'}\n';}
    else {
      let depth=0,start=0;const selectors=[];
      for(let i=0;i<=sel.length;i++){if(sel[i]==='('||sel[i]==='[')depth++;else if(sel[i]===')'||sel[i]===']')depth--;if((sel[i]===','&&depth===0)||i===sel.length){selectors.push(sel.slice(start,i));start=i+1;}}
      const keep=selectors.filter(s=>/fc-(?:local-header|logo-col|home-logo|nav-col|home-menu|header-social|home-social|social-bluesky|home-nav-toggle)/.test(s));
      if(keep.length)out+=keep.join(',').replaceAll('[data-elementor-id="6876"] .fc-local-header','.fc-shared-header').replaceAll('[data-elementor-id="6876"]','.fc-shared-header')+'{'+body+'}\n';
    }
    p=e;
  }
  return out;
}
const css = `.fc-shared-instance{--fc-cyan:#00deee;--fc-pink:#f51ca5;--fc-muted:#a6bfd3;--fc-line:#183348;--fc-white:#f1f6fc;color:var(--fc-white);font:16px/1.6 "Flynns Cardozo Barlow",Arial,sans-serif}.fc-shared-header .elementor-widget{margin-bottom:0}.fc-shared-header a{text-decoration:none}.fc-shared-header a:focus-visible{outline:2px solid var(--fc-cyan);outline-offset:4px}.fc-shared-support{display:none!important}\n` + rules(fs.readFileSync(path.join(home,'CSS/home-github.css'),'utf8'));
const js = `(()=>{function prepare(){document.querySelectorAll('.fc-shared-header').forEach(header=>{const page=document.body.className.match(/(?:^|\\s)page-id-(\\d+)/)?.[1]||new URLSearchParams(location.search).get('elementor-preview');const pages={6876:'inicio',6953:'games',6925:'about',6961:'indies',7098:'press'};header.querySelectorAll('.fc-home-menu-item').forEach(item=>{const a=item.querySelector('a');const active=a&&new URL(a.href).pathname==='/'+(pages[page]||(document.querySelector('.fc-game-page')?'games':''))+'-cardozo/';item.classList.toggle('fc-active',!!active);if(active)a.setAttribute('aria-current','page');else a?.removeAttribute('aria-current')});const logo=header.querySelector('.fc-home-logo img');if(logo){logo.alt="Flynn's Arcade Publishing";logo.loading='eager'}const nav=header.querySelector('.fc-nav-col'),toggle=header.querySelector('.fc-home-nav-toggle a');if(!nav||!toggle||header.dataset.fcNavReady)return;header.dataset.fcNavReady='true';nav.id='fc-cardozo-navigation';nav.setAttribute('role','navigation');nav.setAttribute('aria-label','Primary');toggle.setAttribute('aria-controls',nav.id);const setOpen=open=>{header.classList.toggle('fc-nav-open',open);toggle.setAttribute('aria-expanded',String(open));toggle.setAttribute('aria-label',open?'Close navigation':'Open navigation')};toggle.addEventListener('click',e=>{e.preventDefault();setOpen(!header.classList.contains('fc-nav-open'))});toggle.addEventListener('keydown',e=>{if(e.key===' '){e.preventDefault();toggle.click()}});nav.addEventListener('click',e=>{if(e.target.closest('a'))setOpen(false)});header.addEventListener('keydown',e=>{if(e.key==='Escape'){setOpen(false);toggle.focus()}});setOpen(false)})}if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',prepare,{once:true});else prepare()})();`;
header.settings.css_classes += ' fc-shared-instance';
header.elements[0].elements.push({id:'fc52001',elType:'widget',widgetType:'html',settings:{_title:'Shared header · Styles & navigation',_css_classes:'fc-shared-support',html:'<style>'+css+'</style><script>'+js+'</script>'},elements:[]});
fs.writeFileSync(path.join(base,'Cabecera-compartida-portapapeles.json'),JSON.stringify(source));
fs.writeFileSync(path.join(base,'Cabecera-compartida-plantilla.json'),JSON.stringify({version:'0.4',title:'Cardozo · Shared header',type:'section',content:[header],page_settings:[]},null,2));
fs.writeFileSync(path.join(base,'cabecera-compartida.js'),js);
fs.writeFileSync(path.join(base,'cabecera-compartida.css'),css);
console.log(JSON.stringify({widgets:walk(header).filter(n=>n.widgetType).length,links:walk(header).filter(n=>n.settings._css_classes==='fc-home-menu-item').map(n=>[n.settings.text,n.settings.link.url])}));
