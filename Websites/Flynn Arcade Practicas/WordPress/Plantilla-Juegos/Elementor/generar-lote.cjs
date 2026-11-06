const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
const wp = path.resolve(base, '..');
const root = path.resolve(wp, '..');
const clone = value => JSON.parse(JSON.stringify(value));
const read = file => JSON.parse(fs.readFileSync(file, 'utf8'));
const original = read(path.join(__dirname, 'Game-plantilla.json'));
const github = 'https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/';
const asset = value => /^https?:/.test(value) ? value : github + value;
const walk = node => [node, ...(node.elements || []).flatMap(walk)];
const all = nodes => nodes.flatMap(walk);
const find = (nodes, cls) => all(nodes).find(n => (n.settings._css_classes || n.settings.css_classes || '').split(/\s+/).includes(cls));
const escape = value => String(value).replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
const link = url => ({url, is_external: '', nofollow: '', custom_attributes: 'target|_blank,rel|noopener noreferrer'});
const icons = {Switch:['fab fa-nintendo-switch','fa-brands'], Xbox:['fab fa-xbox','fa-brands'], PlayStation:['fab fa-playstation','fa-brands'], PC:['fas fa-desktop','fa-solid']};
const slugs = ['boardwalk-builders','cash-cow-dx','colored-effects','dexter-stardust','donut-dodo','eternum-ex','explosive-dinosaurs','flora-fang-guardians-of-the-vampire-garden','freddy-farmer','galacticon'];
const report = [];
for (const [index, slug] of slugs.entries()) {
  const local = path.join(root, 'Games/Juegos', slug, 'JSON/game.json');
  const supplemental = path.join(wp, 'Juegos-Cardozo/Juegos', slug, 'JSON/game.json');
  if (!fs.existsSync(local) && !fs.existsSync(supplemental)) { report.push({slug,status:'missing-source'}); continue; }
  const data = read(fs.existsSync(local) ? local : supplemental);
  const content = clone(original.content);
  find(content, 'fc-game-title').settings.title = data.title;
  find(content, 'fc-game-tagline').settings.editor = '<p>' + escape(data.tagline) + '</p>';
  const platformWidget = find(content, 'fc-game-platforms');
  const platforms = [...new Set([...data.platforms, ...data.stores.map(s => s.platform)])];
  platformWidget.settings.icon_list = platforms.map((p,i) => ({_id:'platform'+i,text:p==='Switch'?'Nintendo Switch':p,selected_icon:{value:icons[p][0],library:icons[p][1]},link:{url:''}}));
  const video = find(content, 'fc-game-video');
  video.settings.youtube_url = data.hero_video.url;
  video.settings.image_overlay = {id:'',url:asset(data.gallery[0].image)};
  if (data.hero_video.start) video.settings.start = data.hero_video.start;
  find(content,'fc-game-youtube').settings.link = link(data.hero_video.url);
  const shot = find(content, 'fc-game-shot');
  content[4].settings._title = '04 · Screenshots · '+data.gallery.length+' editable images';
  content[4].elements[0].elements = data.gallery.map((g,i) => {
    const n=clone(shot); n.settings._title='Screenshot '+(i+1); n.settings.image={id:'',url:asset(g.image)};
    n.settings.link={...link(asset(g.image)),custom_attributes:'aria-label|View '+g.alt}; return n;
  });
  const purchase = content[5].elements[0].elements;
  const storeButton = clone(purchase[1]);
  content[5].elements[0].elements = [purchase[0], ...data.stores.map(s => {
    const n=clone(storeButton); n.settings._title=s.label; n.settings.text=s.label; n.settings.link=link(s.url);
    n.settings.selected_icon={value:icons[s.platform][0],library:icons[s.platform][1]};return n;
  })];
  find(content, 'fc-game-artwork').settings.image = {id:'',url:asset(data.poster.image)};
  const story = content[6].elements[1];
  const headingBase = clone(find(content,'fc-game-features-heading'));
  const paragraphBase = clone(find(content,'fc-game-description'));
  const listBase = clone(find(content,'fc-game-features'));
  function block(b,i) {
    const n=clone(b.type==='heading'?headingBase:b.type==='list'?listBase:paragraphBase);
    n.settings._title='Content · '+(i+1)+' · '+b.type;
    if(b.type==='heading') {n.settings.title=b.text;n.settings.header_size='h3';}
    else n.settings.editor=b.type==='list'?'<ul>'+b.items.map(t=>'<li>'+escape(t)+'</li>').join('')+'</ul>':'<p>'+escape(b.text)+'</p>';
    return n;
  }
  const aboutHeading=clone(find(content,'fc-game-about-title'));
  aboutHeading.settings.title='About '+data.title;
  const genres=clone(find(content,'fc-game-genres'));
  const genreBase=clone(find(content,'fc-game-genre'));
  genres.elements[0].elements=data.genres.map(g=>{const n=clone(genreBase);n.settings.title=g;n.settings._title='Genre · '+g;return n;});
  const features=clone(find(content,'fc-game-features-media'));
  let split=data.blocks.findIndex(b=>b.type==='list');
  if(split<0) {split=Math.max(1,data.blocks.length-2);}
  else if(split>0 && data.blocks[split-1].type==='heading') split--;
  features.elements[0].elements=data.blocks.slice(split).map(block);
  const animation=find([features],'fc-game-animation');
  animation.settings.image={id:'',url:data.animation_url || 'https://www.flynnsarcades.com/wp-content/uploads/2026/10/fc-home-'+slug+'-hover.webp'};
  const reviews=clone(find(content,'fc-game-reviews'));
  const reviewToolbar=clone(find([reviews],'fc-game-review-toolbar'));
  const reviewSlideBase=clone(find([reviews],'fc-game-review-slide'));
  const reviewColBase=clone(reviewSlideBase.elements[0]);
  const slides=[];
  for(let i=0;i<data.reviews.length;i+=2) {
    const slide=clone(reviewSlideBase);slide.settings._title='Reviews · '+(i+1)+'–'+Math.min(i+2,data.reviews.length);
    slide.elements=data.reviews.slice(i,i+2).map(r=>{
      const col=clone(reviewColBase);col.settings._title=r.outlet;
      find([col],'fc-game-review-quote').settings.editor='<p>'+escape(r.quote)+'</p>';
      find([col],'fc-game-review-author').settings.editor='<p>'+escape(r.author)+'</p>';
      const outlet=find([col],'fc-game-review-outlet');outlet.settings.text=r.outlet;outlet.settings._title=r.outlet;outlet.settings.link=link(r.url);
      col.elements=col.elements.filter(n=>!(n.settings._css_classes||'').includes('fc-game-review-translation'));
      if(r.translated_from) {const n=clone(paragraphBase);n.settings._css_classes='fc-game-review-translation';n.settings.editor='<p>Translated from '+escape(r.translated_from)+'</p>';col.elements.push(n);}
      return col;
    });slides.push(slide);
  }
  find([reviewToolbar],'fc-game-review-count').settings.title='1 / '+slides.length;
  reviews.elements[0].elements=[reviewToolbar,...slides];
  if(slides.length===1) reviewToolbar.elements[0].elements=reviewToolbar.elements[0].elements.filter(n=>!['fc-game-review-prev','fc-game-review-next','fc-game-review-count'].includes(n.settings._css_classes));
  story.elements=[aboutHeading,genres,...data.blocks.slice(0,split).map(block),features,...(slides.length?[reviews]:[])];
  if(!slides.length) content[3].elements[0].elements=content[3].elements[0].elements.filter(n=>n.settings.link.url!=='#reviews');
  let seed=0xfd00000+index*0x1000;for(const n of all(content)) n.id=(++seed).toString(16);
  const out=path.join(wp,'Juegos-Cardozo/Juegos',slug,'Elementor');fs.mkdirSync(out,{recursive:true});
  const page={...original,title:data.title+' Cardozo',content,page_settings:{template:'elementor_canvas'}};
  fs.writeFileSync(path.join(out,'plantilla.json'),JSON.stringify(page,null,2));
  fs.writeFileSync(path.join(out,'portapapeles.json'),JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:content}));
  const sourceText=data.blocks.flatMap(b=>b.items||[b.text]);
  const generatedText=all(content[6].elements).map(n=>n.settings.editor||n.settings.title||'').join(' ');
  const missing=sourceText.filter(t=>!generatedText.includes(escape(t))&&!generatedText.includes(t));
  if(missing.length) throw Error('Content lost: '+slug+' '+missing.join(';'));
  if(slug!=='what-comes-after' && JSON.stringify(content.slice(2,7)).includes('Games/Juegos/what-comes-after/')) throw Error('Old media in '+slug);
  const result={slug,title:data.title,sections:content.length,widgets:all(content).filter(n=>n.widgetType).length,stores:data.stores.length,captures:data.gallery.length,reviews:data.reviews.length,blocksPreserved:sourceText.length,animation:animation.settings.image.url,status:'prepared-not-applied'};
  report.push(result);
}
fs.mkdirSync(path.join(wp,'Juegos-Cardozo'),{recursive:true});
fs.writeFileSync(path.join(wp,'Juegos-Cardozo/preparacion.json'),JSON.stringify(report,null,2));
console.log(report);
