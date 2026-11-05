const fs=require('node:fs'),path=require('node:path');
const base=path.resolve(__dirname,'..'),wp=path.resolve(base,'..'),workspace=path.resolve(wp,'..');
const read=p=>fs.readFileSync(p,'utf8');
const data=JSON.parse(read(path.join(workspace,'Games/Juegos/what-comes-after/JSON/game.json')));
const home=JSON.parse(read(path.join(wp,'Inicio-Cardozo/Elementor/Inicio-Cardozo-bloques.json')));
const about=JSON.parse(read(path.join(wp,'About-Cardozo/Elementor/About-Cardozo-bloques.json')));
const clone=n=>JSON.parse(JSON.stringify(n));
const walk=n=>[n,...(n.elements||[]).flatMap(walk)];
let seed=0xfcd0000;const id=()=> (++seed).toString(16);
const zero={unit:'px',top:'0',right:'0',bottom:'0',left:'0',isLinked:true};
const link=(url,attrs='')=>({url,is_external:'',nofollow:'',custom_attributes:attrs});
const widget=(type,title,settings={},cls='')=>({id:id(),elType:'widget',widgetType:type,settings:{_title:title,_css_classes:cls,_margin:zero,...settings},elements:[]});
const col=(title,elements,size=100,cls='')=>({id:id(),elType:'column',isInner:false,settings:{_title:title,_column_size:size,padding:zero,css_classes:cls},elements});
const section=(title,elements,cls='',inner=false,tag='section',anchor='')=>({id:id(),elType:'section',isInner:inner,settings:{_title:title,layout:inner?'full_width':'boxed',gap:'no',content_width:{unit:'px',size:1260},padding:zero,css_classes:(inner?'':'fc-home fc-game-block ')+cls,html_tag:tag,...(anchor?{_element_id:anchor}:{})},elements});
const escape=s=>s.replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;');
const paragraph=(title,text,cls='')=>widget('text-editor',title,{editor:'<p>'+escape(text)+'</p>'},cls);
const heading=(title,text,tag,cls='')=>widget('heading',title,{title:text,header_size:tag},cls);
const button=(title,url,cls='',icon='',attrs='')=>widget('button',title,{text:title,link:link(url,attrs),...(icon?{selected_icon:{value:icon,library:icon.startsWith('fab ')?'fa-brands':'fa-solid'},icon_align:'left'}:{})},cls);
const image=(title,url,cls='',href='',attrs='')=>widget('image',title,{image:{id:'',url},image_size:'full',caption_source:'none',link_to:href?'custom':'none',...(href?{link:link(href,attrs)}:{}),open_lightbox:'no'},cls);
const github='https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/';
const asset=p=>github+p;
const icons={Switch:'fas fa-toggle-on',Xbox:'fab fa-xbox',PlayStation:'fab fa-playstation',PC:'fas fa-desktop'};
// Reuse the exact native platform icons already used in the Games catalog.
const catalogCard=walk(home.content[5]).find(n=>n.settings?._css_classes==='fc-card-title'&&n.settings.title===data.title);
const card=walk(home.content[5]).find(n=>n.settings?.css_classes?.includes('fc-card')&&walk(n).includes(catalogCard));
const platforms=clone(walk(card).find(n=>n.settings?._css_classes?.trim()==='fc-platforms'));
platforms.settings._title='Platforms';platforms.settings._css_classes='fc-game-platforms';
platforms.settings.icon_list.forEach(n=>{n.link=link('');});
const trailer=widget('video','Trailer · Replace the video URL for each game',{video_type:'youtube',youtube_url:data.hero_video.url,show_image_overlay:'yes',image_overlay:{id:'',url:asset(data.gallery[0].image)},show_play_icon:'yes',lazy_load:'yes',yt_privacy:'',lightbox:'',aspect_ratio:'169'},'fc-game-video');
const hero=section('02 · Game overview · Title / Platforms / Trailer',[col('Overview',[
 button('All games','https://www.flynnsarcades.com/games-cardozo/','fc-game-back','fas fa-arrow-left'),
 section('Game title and trailer',[
  col('Title, tagline and platforms',[
   paragraph('Publisher',"Flynn's Arcade Publishing",'fc-game-eyebrow'),
   heading('Game title',data.title,'h1','fc-game-title'),paragraph('Game tagline',data.tagline,'fc-game-tagline'),platforms,
   button('Get the game','#purchase','fc-cta fc-game-get','fas fa-shopping-bag')
  ],45,'fc-game-copy'),
  col('Trailer · Click to load',[trailer,button('Watch on YouTube',data.hero_video.url,'fc-game-youtube','fas fa-external-link-alt','target|_blank,rel|noopener noreferrer')],55,'fc-game-video-col')
 ],'fc-game-showcase',true)
])],'fc-game-page',false,'section','overview');
const nav=section('03 · Game section navigation',[col('Page sections',[
 ['Overview','overview'],['Screenshots','screenshots'],['Purchase','purchase'],['About the game','about-game'],['Reviews','reviews']
].map(([text,url])=>button(text,'#'+url,'fc-game-section-link')))],'fc-game-sections',false,'nav');
const gallery=section('04 · Screenshots · 5 editable images',[col('Screenshot gallery',data.gallery.map((g,i)=>image('Screenshot '+(i+1),asset(g.image),'fc-game-shot',asset(g.image),'aria-label|View '+g.alt)))],'fc-game-gallery',false,'section','screenshots');
const purchase=section('05 · Purchase · Store links',[col('Available stores',[
 heading('Purchase heading','Purchase','h2','fc-game-purchase-title'),
 ...data.stores.map(s=>button(s.label,s.url,'fc-cta',icons[s.platform],'target|_blank,rel|noopener noreferrer'))
])],'fc-game-purchase',false,'section','purchase');
const featureList=data.blocks.find(b=>b.type==='list').items;
const features=section('Features & animated gameplay',[
 col('Features',[heading('Features heading','Features','h3','fc-game-features-heading'),widget('text-editor','Game features',{editor:'<ul>'+featureList.map(t=>'<li>'+escape(t)+'</li>').join('')+'</ul>'},'fc-game-features')],52),
 col('Animated gameplay',[image('Gameplay preview · Animated WebP','https://www.flynnsarcades.com/wp-content/uploads/2026/10/fc-home-what-comes-after-hover.webp','fc-game-animation'),button('Pause animation','#pause-animation','fc-game-animation-toggle','fas fa-pause','role|button,aria-label|Pause animation,aria-pressed|false')],48,'fc-game-animation-col')
],'fc-game-features-media',true);
const reviewSlides=[];
for(let i=0;i<data.reviews.length;i+=2){reviewSlides.push(section('Reviews · '+(i+1)+'–'+Math.min(i+2,data.reviews.length),data.reviews.slice(i,i+2).map(r=>col(r.outlet,[
 paragraph('Review quotation',r.quote,'fc-game-review-quote'),paragraph('Review author',r.author,'fc-game-review-author'),button(r.outlet,r.url,'fc-game-review-outlet','fas fa-external-link-alt','target|_blank,rel|noopener noreferrer'),...(r.translated_from?[paragraph('Translation note','Translated from '+r.translated_from,'fc-game-review-translation')]:[])
],50)),'fc-game-review-slide',true));}
const reviews=section('Press reviews',[col('Review carousel',[
 section('Review heading and controls',[col('Review toolbar',[
  heading('Review section title','Press reviews','h2','fc-game-reviews-title'),button('Previous review','#previous-review','fc-game-review-prev','fas fa-chevron-left','role|button,aria-label|Previous review'),heading('Review page counter','1 / '+reviewSlides.length,'div','fc-game-review-count'),button('Next review','#next-review','fc-game-review-next','fas fa-chevron-right','role|button,aria-label|Next review')
 ])],'fc-game-review-toolbar',true),...reviewSlides
])],'fc-game-reviews',true,'section','reviews');
const contentAbout=section('06 · About the game · Artwork / Story / Features / Reviews',[
 col('Game cover artwork',[image('Game cover artwork',asset(data.poster.image),'fc-game-artwork')],20,'fc-game-artwork-col'),
 col('Game information',[
  heading('About game · Accessible heading','About '+data.title,'h2','fc-game-about-title'),
  section('Genres',[col('Game genres',data.genres.map(g=>heading('Genre · '+g,g,'div','fc-game-genre')))],'fc-game-genres',true),
  ...data.blocks.filter(b=>b.type==='paragraph').map((b,i)=>paragraph('Game description · Paragraph '+(i+1),b.text,'fc-game-description')),features,reviews
 ],80,'fc-game-story-col')
],'fc-game-about',false,'section','about-game');
const related=clone(about.content.find(n=>n.settings.css_classes.includes('fc-about-projects')));
related.settings._title='07 · Developed by Flynn’s Arcade · Eternum / Lambs / Saga';
related.settings.css_classes='fc-home fc-game-block fc-game-related';related.settings._element_id='developed-games';
const relatedTitle=walk(related).find(n=>n.settings._css_classes==='fc-projects-title');relatedTitle.settings.title="Developed by Flynn's Arcade";relatedTitle.settings._title='Developed games title';
const footer=clone(home.content.slice(6));footer.forEach((n,i)=>n.settings._title=['08 · Find us everywhere','09 · Better news & Send prayers','10 · Footer credits'][i]);
for(const node of footer.flatMap(walk)){if(node.settings.link?.url?.includes('/about-3/'))node.settings.link.url='https://www.flynnsarcades.com/about-cardozo/';}
const setup=clone(home.content[0]);setup.settings._title='00 · Game design & interactions';setup.settings.css_classes='fc-home fc-setup fc-game-setup';
const header=require('../../Ajustes-Cabecera/referencia.cjs')();
const content=[setup,header,hero,nav,gallery,purchase,contentAbout,related,...footer];
for(const n of content.flatMap(walk))n.id=id();

// Retain only base, card and footer rules used by this reusable page.
function splitSelectors(sel){let depth=0,start=0;const out=[];for(let i=0;i<=sel.length;i++){if(sel[i]==='('||sel[i]==='[')depth++;else if(sel[i]===')'||sel[i]===']')depth--;if((sel[i]===','&&!depth)||i===sel.length){out.push(sel.slice(start,i));start=i+1;}}return out;}
function filterCss(css,keep){css=css.replace(/\/\*[\s\S]*?\*\//g,'');let out='',p=0;while(p<css.length){const o=css.indexOf('{',p);if(o<0)break;let d=1,q='',e=o+1;for(;e<css.length&&d;e++){const c=css[e];if(q){if(c==='\\')e++;else if(c===q)q='';}else if(c==='"'||c==="'")q=c;else if(c==='{')d++;else if(c==='}')d--;}const sel=css.slice(p,o).trim(),body=css.slice(o+1,e-1);if(sel.startsWith('@media')){const sub=filterCss(body,keep);if(sub)out+=sel+'{'+sub+'}\n';}else{const selectors=splitSelectors(sel).filter(keep);if(selectors.length)out+=selectors.join(',')+'{'+body+'}\n';}p=e;}return out;}
const keep=s=>/^\s*body\.page-id-6876/.test(s)||/\.fc-(?:home(?=[\s:.\[]|$)|setup|cta|card(?:-|\b)|cover|tags|platforms|local-footer|social-footer|social-copy|footer-social-col|home-social|social-bluesky|contact-footer|contact-col|newsletter-col|news-status|home-email|home-textlink|footer-credits|credit-)/.test(s);
let common=filterCss(read(path.join(wp,'Inicio-Cardozo/CSS/bloques.css'))+'\n'+read(path.join(wp,'Inicio-Cardozo/CSS/home-github.css')),keep);
common=common.replaceAll('body.page-id-6876','body:has(.fc-game-setup)').replaceAll('[data-elementor-id="6876"]','body:has(.fc-game-setup)').replaceAll('HOME · DESIGN & INTERACTIONS','GAME · DESIGN & INTERACTIONS');
common=common.replaceAll('.elementor-editor-active body:has(.fc-game-setup)','body.elementor-editor-active:has(.fc-game-setup)');
let projects=filterCss(read(path.join(wp,'About-Cardozo/CSS/about.css')),s=>/fc-(?:about-projects|projects-title|card|cover|project-summary|tags|platforms)/.test(s));
projects=projects.replaceAll('[data-elementor-id="6925"]','body:has(.fc-game-setup)').replaceAll('fc-about-projects','fc-game-related');
const css=common+'\n'+projects+'\n'+read(path.join(base,'CSS/game.css'));
const fonts=read(path.join(wp,'Inicio-Cardozo/CSS/fuentes-optimizadas.css'));
const previews=read(path.join(wp,'Inicio-Cardozo/JavaScript/preview-loader.js'));
const script=previews+'\n'+read(path.join(base,'JavaScript/game.js'));
const dialog='<dialog class="fc-game-photo-dialog" data-fc-gallery-dialog aria-label="Game screenshots"><div class="fc-game-photo-toolbar"><span data-fc-photo-count aria-live="polite"></span><button type="button" data-fc-photo-close aria-label="Close screenshot">×</button></div><div class="fc-game-photo-stage"><button type="button" data-fc-photo-step="-1" aria-label="Previous screenshot">‹</button><img alt=""><button type="button" data-fc-photo-step="1" aria-label="Next screenshot">›</button></div></dialog>';
const support='<style>'+fonts+'\n'+css+'</style>'+dialog+'<script>'+script+'</script>';
setup.elements[0].elements=[widget('html','Game · Styles, fonts & interactions',{html:support})];
for(const dir of ['HTML','Elementor','CSS','JavaScript'])fs.mkdirSync(path.join(base,dir),{recursive:true});
fs.writeFileSync(path.join(base,'HTML/Game-soporte.html'),support);
fs.writeFileSync(path.join(base,'CSS/game-cardozo.css'),css);
fs.writeFileSync(path.join(base,'Elementor/Game-plantilla.json'),JSON.stringify({version:'0.4',title:'Cardozo · Game page · What Comes After example',type:'page',content,page_settings:[]},null,2));
fs.writeFileSync(path.join(base,'Elementor/Game-portapapeles.json'),JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:content}));
fs.writeFileSync(path.join(base,'Elementor/game.json'),JSON.stringify(data,null,2));
const report={pageId:7106,sections:content.length,widgets:content.flatMap(walk).filter(n=>n.widgetType).length,screenshots:data.gallery.length,reviews:data.reviews.length,stores:data.stores.length,headerTemplate:7123,supportBytes:Buffer.byteLength(support)};
fs.writeFileSync(path.join(base,'Elementor/validacion.json'),JSON.stringify(report,null,2));console.log(report);
