const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
const home = path.resolve(base, '../Inicio-Cardozo');
for (const dir of ['CSS','JavaScript','HTML','Elementor']) fs.mkdirSync(path.join(base, dir), {recursive:true});
const read = (dir, file) => fs.readFileSync(path.join(dir, file), 'utf8');
const write = (file, value) => fs.writeFileSync(path.join(base, file), value);
const source = JSON.parse(read(home, 'Elementor/Inicio-Cardozo-bloques.json'));
const reference = JSON.parse(read(base, 'Elementor/referencia.json'));
const clone = value => JSON.parse(JSON.stringify(value));
const walk = node => [node, ...(node.elements || []).flatMap(walk)];
const zero = {unit:'px',top:'0',right:'0',bottom:'0',left:'0',isLinked:true};
let seed = 0xfca0000;
const id = () => (++seed).toString(16);
const link = (url, attrs = '') => ({url,is_external:'',nofollow:'',custom_attributes:attrs});
const widget = (type,title,settings={},classes='') => ({id:id(),elType:'widget',widgetType:type,settings:{_title:title,_css_classes:classes,_margin:zero,...settings},elements:[]});
const col = (title,elements,size=100) => ({id:id(),elType:'column',isInner:false,settings:{_title:title,_column_size:size,padding:zero},elements});
const section = (title,elements,classes,inner=false,tag='section') => ({id:id(),elType:'section',isInner:inner,settings:{_title:title,layout:inner?'full_width':'boxed',content_width:{unit:'px',size:1260},gap:'no',padding:zero,css_classes:(inner?'':'fc-home ')+classes,html_tag:tag},elements});
const esc = text => text.replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;');
const para = (title,text,classes) => widget('text-editor',title,{editor:'<p>'+esc(text)+'</p>'},classes);
const heading = (title,text,tag,classes) => widget('heading',title,{title:text,header_size:tag},classes);
const button = (title,url,classes,icon,attrs='') => widget('button',title,{text:title,link:link(url,attrs),...(icon?{selected_icon:{value:icon,library:'fa-solid'},icon_align:'right'}:{})},classes);
const refUrl = 'https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Press/HTML/index.html';
const asset = value => new URL(value,refUrl).href;
const image = (title,url,classes) => widget('image',title,{image:{id:'',url},image_size:'full',caption_source:'none',link_to:'none',open_lightbox:'no'},classes);
const setup = clone(source.content[0]); setup.settings._title='00 · Design & interactions'; setup.settings.padding=zero;
const header = clone(source.content[1]); header.settings._title='01 · Shared header · Template 7123';
for (const item of walk(header).filter(n=>n.settings._css_classes?.includes('fc-home-menu-item'))) {
  const active=item.settings.text==='Press'; item.settings._css_classes='fc-home-menu-item'+(active?' fc-active':''); item.settings.link.custom_attributes=active?'aria-current|page':''; if(active)item.settings.link.url='#press-media';
}
const intro=section('02 · Press Media · Page heading',[col('Introduction',[
 para('Company news highlights and presskits','Company news highlights and presskits','fc-press-eyebrow'),heading('Press Media.','Press <span>Media.</span>','h1','fc-press-title')
])],'fc-press-heading');
const nav=section('03 · Coverage / Presskits / Contact',[col('Press sections',[
 button('Coverage','#coverage','',null),button('Presskits','#presskits','','fas fa-download'),button('Contact','#contact','',null)
])],'fc-press-sections',false,'nav');
const searchHtml='<label for="fc-press-search"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m16 16 5 5"/></svg><span class="fc-visually-hidden">Search coverage</span><input id="fc-press-search" data-fc-press-search type="search" placeholder="Search coverage" autocomplete="off" maxlength="100"></label><button type="button" data-fc-press-clear aria-label="Clear search" title="Clear search" hidden><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><path d="m6 6 12 12M18 6 6 18"/></svg></button>';
const tools=section('04 · Search coverage & article count',[col('Coverage tools',[widget('html','Search coverage · input',{html:searchHtml},'fc-press-search'),heading('Article counter','6 / 15 articles','div','fc-press-count')])],'fc-press-tools');
const articles=reference.articles.map((a,i)=>{
 const articleLink=button('Read article',a.link,'fc-article-link','fas fa-external-link-alt','rel|noopener noreferrer'); articleLink.settings.link.is_external='on';
 return section(String(i+5).padStart(2,'0')+' · '+a.heading,[col('Coverage image',[image(a.title,asset(a.image),'fc-press-image')],33),col('Article information',[para('Date & outlet',a.eyebrow,'fc-article-eyebrow'),heading('Article title',a.heading,'h2','fc-article-title'),para('Article summary',a.summary,'fc-article-summary'),articleLink],66)],'fc-press-row',false,'article');
});
const empty=section('20 · Coverage · No results',[col('No results',[para('No results found','No results found.','')])],'fc-press-empty');
const more=section('21 · More coverage',[col('Coverage pagination',[button('More coverage','#more-coverage','fc-cta','fas fa-plus','role|button')])],'fc-press-pagination');
const kitRows=reference.kits.map(k=>section('Presskit · '+k.title,[col(k.title,[image(k.title+' · Media pack',asset(k.image),'fc-kit-image'),heading('Presskit title',k.title,'h3','fc-kit-title'),para('Pack description',k.caption,'fc-kit-caption'),button('Download presskit',asset(k.link),'fc-cta fc-kit-download','fas fa-download','download|,aria-label|Download '+k.title+' presskit')])],'fc-presskit',true,'article'));
const kits=section('22 · Presskits · 4 downloadable media packs',[col('Presskits',[heading('Presskits title','Presskits','h2','fc-presskits-title'),...kitRows])],'fc-presskits');
const promotions=clone(source.content[4]); promotions.settings._title='23 · Game highlights'; promotions.settings.padding=zero;
const slides=walk(promotions).filter(n=>n.settings.css_classes?.includes('fc-promo-slide'));
// User-selected developed games take precedence over the older Press reference carousel.
const highlightNames=['Eternum Ex','Lambs on the Road','Saga'];
const highlightPreviews=['fc-home-eternum-ex-hover.webp','fc-home-lambs-on-the-road-hover.webp','fc-home-saga-hover.webp'];
slides.forEach((slide,i)=>{
 slide.settings._title='Highlight · '+highlightNames[i];
 for(const w of walk(slide)){
  if(w.widgetType==='image'){
   w.settings._title=highlightNames[i]+' · Artwork';
   w.settings.link.custom_attributes='data-fc-preview|https://www.flynnsarcades.com/wp-content/uploads/2026/10/'+highlightPreviews[i]+',aria-label|Discover '+highlightNames[i];
  }
 }
});
const dots=walk(promotions).filter(n=>n.settings._css_classes?.includes('fc-promo-dot'));
dots.forEach((d,i)=>{d.settings.text=highlightNames[i];d.settings._title=highlightNames[i];});
const footer=clone(source.content.slice(6));footer.forEach((s,i)=>s.settings._title=['24 · Find us everywhere','25 · Better news & Send prayers','26 · Footer credits'][i]);
const content=[setup,header,intro,nav,tools,...articles,empty,more,kits,promotions,...footer];
for(const n of content.flatMap(walk)){n.id=id();delete n.settings._element_id;}
function prune(css){css=css.replace(/\/\*[\s\S]*?\*\//g,'');let out='',pos=0;while(pos<css.length){const open=css.indexOf('{',pos);if(open<0)break;let depth=1,quote='',end=open+1;for(;end<css.length&&depth;end++){const ch=css[end];if(quote){if(ch==='\\')end++;else if(ch===quote)quote='';}else if(ch==='"'||ch==="'")quote=ch;else if(ch==='{')depth++;else if(ch==='}')depth--;}const selector=css.slice(pos,open).trim(),body=css.slice(open+1,end-1);if(selector.startsWith('@media')){const nested=prune(body);if(nested.trim())out+=selector+' {\n'+nested+'}\n';}else if(!/fc-(?:catalog|filter|search|empty|loadmore|card|cover|tags|platforms|featured|banner)/.test(selector))out+=selector+' {'+body+'}\n';pos=end;}return out;}
const shared=prune(read(home,'CSS/bloques.css')+'\n'+read(home,'CSS/home-github.css')).replaceAll('6876','7098').replaceAll('HOME · DESIGN','PRESS · DESIGN');
const fonts=read(home,'CSS/fuentes-optimizadas.css');
const css=(shared+'\n'+read(base,'CSS/press.css')).replaceAll('[data-elementor-id="7098"]','body.page-id-7098 [data-elementor-id="7098"]').replaceAll('body.page-id-7098 body.page-id-7098','body.page-id-7098').replaceAll('.elementor-editor-active body.page-id-7098','body.elementor-editor-active.page-id-7098');
const navigation=read(home,'JavaScript/home-github.js').replaceAll('6876','7098').replaceAll('fc-home-navigation','fc-press-navigation').replace(/    const banner[^\n]*\n    if \(banner[^\n]*\n/,'');
const previewLoader=read(home,'JavaScript/preview-loader.js');
const support='<style>\n'+fonts+'\n'+css+'</style>\n<script>\n'+previewLoader+'\n'+read(base,'JavaScript/press.js')+'\n'+navigation+'</script>';
setup.elements[0].elements=[setup.elements[0].elements[0]];setup.elements[0].elements[0].settings._title='Press · Styles, fonts & interactions';setup.elements[0].elements[0].settings.html=support;
write('CSS/press-cardozo.css',css);write('CSS/fuentes-optimizadas.css',fonts);write('JavaScript/navegacion.js',navigation);write('HTML/Press-Cardozo-soporte.html',support);
write('JavaScript/preview-loader.js',previewLoader);
write('Elementor/Game-highlights-portapapeles.json',JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:[promotions]}));
write('Elementor/Press-Cardozo-bloques.json',JSON.stringify({version:'0.4',title:'Press Cardozo · Editable neon press room',type:'page',content,page_settings:[]},null,2));
write('Elementor/Press-Cardozo-portapapeles.json',JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:content}));
const report={pageId:7098,sections:content.length,widgets:content.flatMap(walk).filter(n=>n.elType==='widget').length,articles:articles.length,kits:kitRows.length,highlights:highlightNames,supportBytes:Buffer.byteLength(support)};write('Elementor/validacion-bloques.json',JSON.stringify(report,null,2));console.log(report);
