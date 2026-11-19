/** Public WordPress -> static, language-organised GitHub Pages copy. */
import fs from 'node:fs/promises';
import {existsSync} from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {parseHTML} from './vendor/linkedom.mjs';
import {root,sharedNames,buildShared} from './compilar-compartidos.mjs';

const origin='https://www.flynnsarcades.com';
const github='https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/';
const cache=process.argv.find(a=>a.startsWith('--cache='))?.slice(8);
const stamp=new Date().toISOString();
const metaDir='Compartido/JSON/Sincronizacion';
const hash=s=>crypto.createHash('sha256').update(s).digest('hex').slice(0,14);
const exists=async file=>fs.access(file).then(()=>true,()=>false);
const write=async(file,data)=>{const target=path.join(root,file);await fs.mkdir(path.dirname(target),{recursive:true});await fs.writeFile(target,typeof data==='string'?data.trimEnd()+'\n':data)};
const read=async file=>fs.readFile(path.join(root,file),'utf8');
const text=e=>(e?.textContent||'').replace(/\s+/g,' ').trim();
const mains={Inicio:['Home','home'],Games:['Games','games'],About:['About','about'],Indies:['Indies','indies'],Press:['Press','press'],Privacy:['Privacy','privacy']};
const sourceInventory=JSON.parse(cache?await fs.readFile(path.join(cache,'inventory.json'),'utf8'):await read(metaDir+'/origen.json'));
const pages=sourceInventory.map(p=>{
  const slug=new URL(p.view).pathname.split('/').filter(Boolean).pop()||'home';
  const main=mains[p.title];
  return{...p,slug,kind:main?.[1]||'game',folder:main?.[0]||'Games/Juegos/'+slug,
    file:(main?.[0]||'Games/Juegos/'+slug)+'/HTML/index.html'};
});
const routes=new Map();
for(const p of pages){
  const pathname=new URL(p.view).pathname;
  for(const route of [pathname,'/'+p.slug+'/','/'+p.slug+'-cardozo/']) routes.set(route,p);
  routes.set('?page_id='+p.id.replace('post-',''),p);
}
for(const route of ['/','/inicio/','/inicio-cardozo/','/home/'])routes.set(route,pages.find(p=>p.kind==='home'));
for(const route of ['/about/','/about-cardozo/'])routes.set(route,pages.find(p=>p.kind==='about'));
const assetExt=/\.(?:css|jpe?g|png|gif|webp|avif|svg|ico|woff2?|ttf|otf|eot|zip|pdf|mp4|webm)$/i;
const assets=new Map(),aliases=new Map(),queue=[],failures=[];
const oldAssets=await exists(path.join(root,metaDir,'recursos.json'))?JSON.parse(await read(metaDir+'/recursos.json')):[];
const cachedAssets=new Map(oldAssets.map(a=>[a.url,a]));
const cachedByFile=new Map(oldAssets.map(a=>[a.file,a]));
const cssJobs=[];
let bytes=0;
function absolute(value,base=origin+'/'){
  try{return new URL(value.replaceAll('&amp;','&'),base)}catch{return null}
}
function register(value,base=origin+'/'){
  const url=absolute(value,base);if(!url||!['http:','https:'].includes(url.protocol)||!assetExt.test(url.pathname))return null;
  url.hash='';const key=url.href;
  if(aliases.has(key)&&aliases.get(key)!==key)return register(aliases.get(key));
  if(assets.has(key))return assets.get(key);
  const ext=path.posix.extname(url.pathname).toLowerCase();
  let category=ext==='.css'?'CSS':/\.(woff2?|ttf|otf|eot)$/.test(ext)?'Fonts':/\.(zip|pdf|mp4|webm)$/.test(ext)?'Downloads':'Images';
  const basename=decodeURIComponent(path.posix.basename(url.pathname,ext)).replace(/[^\w.-]+/g,'-').slice(0,80)||'resource';
  const repositoryFile=key.startsWith(github)?decodeURIComponent(url.pathname.slice(new URL(github).pathname.length)):'';
  const reuse=repositoryFile&&!repositoryFile.startsWith('WordPress/')&&!/\.(?:css|js)$/.test(ext)&&path.resolve(root,repositoryFile).startsWith(root+path.sep)&&existsSync(path.join(root,repositoryFile));
  const entry={url:key,file:reuse?repositoryFile:`Compartido/${category}/Sincronizado/${basename}-${hash(key)}${ext}`,...(reuse?{reused_original:true}:{})};
  assets.set(key,entry);queue.push(entry);return entry;
}
function routeOf(url){return url&&['www.flynnsarcades.com','flynnsarcades.com'].includes(url.hostname)?(routes.get('?page_id='+url.searchParams.get('page_id'))||routes.get(url.pathname.toLowerCase().replace(/\/?$/,'/'))):null}
function mapped(value,base=origin+'/'){
  if(!value||value.startsWith('@root/')||/^(?:#|data:|mailto:|tel:|javascript:|blob:)/i.test(value))return value;
  const url=absolute(value,base);if(!url)return value;
  const route=routeOf(url);if(route){url.searchParams.delete('page_id');return '@root/'+route.file+url.search+url.hash}
  const asset=register(url.href);return asset?'@root/'+asset.file+(url.hash||''):value;
}
function strings(value,base){
  if(typeof value==='string')return value.replace(/https?:\/\/[^\s"'<>`\\)]+/g,url=>mapped(url,base));
  if(Array.isArray(value))return value.map(v=>strings(v,base));
  if(value&&typeof value==='object')return Object.fromEntries(Object.entries(value).map(([k,v])=>[strings(k,base),strings(v,base)]));
  return value;
}
function inlineCss(css,base,ownedFile){
  const file=ownedFile||`Compartido/CSS/Sincronizado/bloque-${hash(css)}.css`;
  cssJobs.push({file,css,base});scanCss(css,base);return file;
}
function scanCss(css,base){
  for(const m of css.matchAll(/url\(\s*["']?([^"')]+)["']?\s*\)|@import\s+["']([^"']+)["']/g))register(m[1]||m[2],base);
}
function rewriteCss(css,base,file){
  const one=value=>{
    const url=absolute(value,base);if(!url||/^(?:data:|#)/i.test(value))return value;
    const entry=register(url.href);return entry?path.posix.relative(path.posix.dirname(file),entry.file)+(url.hash||''):url.href;
  };
  return css.replace(/url\(\s*(["']?)([^"')]+)\1\s*\)/g,(_,q,value)=>`url("${one(value)}")`)
    .replace(/(@import\s+)["']([^"']+)["']/g,(_,prefix,value)=>`${prefix}"${one(value)}"`);
}
const docs=[];
for(let offset=0;offset<pages.length;offset+=5){
  const results=await Promise.allSettled(pages.slice(offset,offset+5).map(async page=>{
    let html;
    if(cache)html=await fs.readFile(path.join(cache,'raw',page.id+'.html'),'utf8');
    else{const url=new URL(page.view);url.searchParams.set('fa_sync',Date.now());const response=await fetch(url,{signal:AbortSignal.timeout(30000)});if(!response.ok)throw Error(`${page.title}: ${response.status}`);html=await response.text()}
    const {document}=parseHTML(html);let main=document.querySelector('main#content');
    if(!main){
      const pageRoot=document.querySelector(`[data-elementor-id="${page.id.replace('post-','')}"]`);
      if(pageRoot){main=document.createElement('main');main.id='content';main.append(pageRoot);document.body.append(main)}
    }
    if(!main||!main.querySelector(`[data-elementor-id="${page.id.replace('post-','')}"]`))throw Error(`${page.title}: unexpected source`);
    docs.push({page,document,main});
  }));
  for(const result of results)if(result.status==='rejected')throw result.reason;
}
// Old GIF identifiers resolve to the already-optimised animation used by WordPress.
for(const {main}of docs)for(const data of main.querySelectorAll('script[type="application/json"]')){
  try{for(const[original,item]of Object.entries(JSON.parse(data.textContent)))if(item?.preview)aliases.set(original,item.preview)}catch{}
}
const prepared=[];
for(const {page,document:d,main}of docs){
  const styles=[];
  for(const link of d.head.querySelectorAll('link[rel="stylesheet"]')){
    if(/post-(3568|3252)\.css/.test(link.href))continue;
    const asset=register(link.href,page.view);if(asset)styles.push('@root/'+asset.file);
  }
  for(const style of d.head.querySelectorAll('style'))styles.push('@root/'+inlineCss(style.textContent,page.view));
  // There are no jQuery/Elementor dependencies in the site's authored interactions.
  // Retain their original parser order; only the image viewer needs a static adapter.
  let styleIndex=0;
  for(const style of [...main.querySelectorAll('style')]){
    const owned=style.closest('[data-elementor-id]')?.getAttribute('data-elementor-id')===page.id.replace('post-','');
    const ownedFile=owned&&styleIndex++===0?`${page.folder}/CSS/${page.kind}.css`:null;
    const link=d.createElement('link');link.rel='stylesheet';link.href='@root/'+inlineCss(style.textContent,page.view,ownedFile);style.replaceWith(link);
  }
  for(const image of main.querySelectorAll('img')){image.removeAttribute('srcset');image.removeAttribute('sizes');image.setAttribute('decoding','async')}
  for(const node of main.querySelectorAll('*')){
    for(const attribute of [...node.attributes]){
      if(['srcset','sizes'].includes(attribute.name))continue;
      let value=attribute.value;
      if(['href','src','poster','data-src'].includes(attribute.name))value=mapped(value,page.view);
      else if(value.startsWith('{')||value.startsWith('[')){
        try{value=JSON.stringify(strings(JSON.parse(value),page.view))}catch{value=strings(value,page.view)}
      }else if(/https?:\/\//.test(value))value=strings(value,page.view);
      node.setAttribute(attribute.name,value);
    }
  }
  let scriptIndex=0;
  for(const script of [...main.querySelectorAll('script')]){
    if(script.type==='application/json'){
      try{script.textContent=JSON.stringify(strings(JSON.parse(script.textContent),page.view))}catch{}
    }else if(!script.src){
      const code=script.textContent.replace("const promotions = root.querySelector('.fc-promotions');", "const promotions = root.querySelector('.fc-promotions');\n    if (!promotions) return;");
      if(!code.trim()){script.remove();continue}
      const owned=script.closest('[data-elementor-id]')?.getAttribute('data-elementor-id')===page.id.replace('post-','');
      const file=owned&&scriptIndex++===0?`${page.folder}/JavaScript/${page.kind}.js`:`Compartido/JavaScript/Sincronizado/bloque-${hash(code)}.js`;
      await write(file,code);script.textContent='';script.src='@root/'+file;
    }
  }
  const description=d.head.querySelector('meta[name="description"]')?.content||'';
  const title=text(d.querySelector('title'));
  const head=`<meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>${title.replaceAll('&','&amp;').replaceAll('<','&lt;')}</title><meta name="description" content="${description.replaceAll('&','&amp;').replaceAll('"','&quot;')}"><link rel="canonical" href="${github+page.file}"><link rel="icon" href="@root/Compartido/Images/Brand/favicon-original.png">${[...new Set(styles)].map(href=>`<link rel="stylesheet" href="${href}">`).join('')}<link rel="stylesheet" href="@root/Compartido/CSS/wordpress-estatico.css">`;
  const bodyClass=d.body.className;
  prepared.push({page,main,head,bodyClass});
}

async function download(entry){
  const old=cachedAssets.get(entry.url);
  let data;
  const cssSource=entry.file.endsWith('.css');
  if(entry.reused_original){data=await fs.readFile(path.join(root,entry.file));entry.bytes=data.length;entry.sha256=hash(data);bytes+=data.length;return}
  if(cssSource&&old&&!process.argv.includes('--refresh-assets')&&await exists(path.join(root,old.file))){
    data=await fs.readFile(path.join(root,old.file));
    const localReference=value=>{
      if(/^(?:data:|#|https?:)/.test(value))return value;
      const dependency=cachedByFile.get(path.posix.normalize(path.posix.join(path.posix.dirname(old.file),value.split(/[?#]/)[0])));
      if(!dependency)return value;
      return path.posix.relative(path.posix.dirname(entry.file),register(dependency.url).file);
    };
    for(const m of data.toString('utf8').matchAll(/url\(\s*["']?([^"')]+)["']?\s*\)|@import\s+["']([^"']+)["']/g)){
      const value=m[1]||m[2];if(/^(?:data:|#|https?:)/.test(value))continue;
      const dependency=cachedByFile.get(path.posix.normalize(path.posix.join(path.posix.dirname(old.file),value.split(/[?#]/)[0])));
      if(dependency)register(dependency.url);
    }
    const prepared=data.toString('utf8').replace(/url\(\s*(["']?)([^"')]+)\1\s*\)/g,(_,q,v)=>`url("${localReference(v)}")`).replace(/(@import\s+)["']([^"']+)["']/g,(_,p,v)=>`${p}"${localReference(v)}"`);
    entry.bytes=Buffer.byteLength(prepared);entry.sha256=hash(prepared);bytes+=entry.bytes;await write(entry.file,prepared);return;
  }
  if(!cssSource&&old&&await exists(path.join(root,old.file))){data=await fs.readFile(path.join(root,old.file))}
  else if(!cssSource&&!process.argv.includes('--refresh-assets')&&await exists(path.join(root,entry.file)))data=await fs.readFile(path.join(root,entry.file));
  else{
    const url=new URL(entry.url);
    const local=url.href.startsWith(github)?decodeURIComponent(url.pathname.slice(new URL(github).pathname.length)):'';
    const safeLocal=local&&path.resolve(root,local).startsWith(root+path.sep)&&await exists(path.join(root,local));
    const legacySuffix=url.origin===origin&&url.pathname.includes('/uploads/elementor/')?url.pathname.split('/uploads/elementor/')[1]:'';
    const equivalent=/^(Fonts|Images\/Backgrounds)\//.test(legacySuffix)?path.join(root,'Compartido',legacySuffix):'';
    if(safeLocal)data=await fs.readFile(path.join(root,local));
    else if(equivalent&&await exists(equivalent)){data=await fs.readFile(equivalent);entry.repair='Existing identical font/background; source URL returns 404'}
    else{let last;for(let attempt=0;attempt<3;attempt++)try{const r=await fetch(entry.url,{signal:AbortSignal.timeout(25000)});if(!r.ok)throw Error(String(r.status));data=Buffer.from(await r.arrayBuffer());break}catch(e){last=e}if(!data)throw last}
  }
  entry.bytes=data.length;entry.sha256=hash(data);bytes+=data.length;
  if(entry.file.endsWith('.css')){
    const css=data.toString('utf8');cssJobs.push({file:entry.file,css,base:entry.url});scanCss(css,entry.url);
  }else await write(entry.file,data);
}
let done=0;
while(done<queue.length){
  const batch=queue.slice(done,done+8);
  const results=await Promise.allSettled(batch.map(download));
  for(let i=0;i<results.length;i++)if(results[i].status==='rejected')failures.push({url:batch[i].url,error:results[i].reason.message});
  done+=batch.length;if(done%80<8||done>=queue.length)console.log(`Recursos: ${done}/${queue.length}`);
}
if(failures.length){await write(metaDir+'/recursos-pendientes.json',JSON.stringify(failures,null,2));throw Error(`${failures.length} recursos pendientes; las páginas originales aún no se han sustituido.`)}
for(const job of cssJobs)await write(job.file,rewriteCss(job.css,job.base,job.file));

const shared={};
for(const id of Object.keys(sharedNames)){
  const source=prepared.find(item=>item.main.querySelector(`[data-elementor-id="${id}"]`));
  if(!source)throw Error('Missing shared template '+id);
  shared[id]=source.main.querySelector(`[data-elementor-id="${id}"]`).outerHTML;
  await write('Compartido/HTML/'+sharedNames[id]+'.html',shared[id]);
}
const rows=[];
for(const {page,main,head,bodyClass}of prepared){
  for(const[id,name]of Object.entries(sharedNames))for(const block of [...main.querySelectorAll(`[data-elementor-id="${id}"]`)])block.replaceWith(main.ownerDocument.createComment(' shared:'+name+' '));
  const html=`<!doctype html>\n<html lang="en"><head>${head}</head><body class="${bodyClass}" data-page="${page.kind}" data-source-page="${page.id}"><a class="skip-link screen-reader-text" href="#content">Skip to content</a>${main.outerHTML}<script src="@root/Compartido/JavaScript/wordpress-estatico.js"></script></body></html>\n`;
  const template=page.folder+'/JSON/pagina.json';
  await write(template,JSON.stringify({source:page.view,synced_at:stamp,html},null,2)+'\n');
  rows.push({title:page.title,source:page.view,file:page.file,template,kind:page.kind});
}
await write(metaDir+'/origen.json',JSON.stringify(sourceInventory.map(({id,title,view})=>({id,title,view})),null,2)+'\n');
await write(metaDir+'/paginas.json',JSON.stringify(rows,null,2)+'\n');
await write(metaDir+'/recursos.json',JSON.stringify([...assets.values()],null,2)+'\n');
await write(metaDir+'/estado.json',JSON.stringify({source:origin,synced_at:stamp,pages:rows.length,gamePages:rows.filter(p=>p.kind==='game').length,assets:assets.size,assetBytes:bytes,privacy:pages.some(p=>p.kind==='privacy')?'Published in source; mirrored locally':'Not public; not included'},null,2)+'\n');
await buildShared({fromSnapshots:true});
// Structured content is also available beside each page, independently of markup.
const local=s=>s.replaceAll('@root/','');
const cardData=card=>({title:text(card.querySelector('.fc-card-title')),slug:new URL(card.querySelector('.fc-card-title a')?.getAttribute('href')||origin,origin).pathname.split('/').filter(Boolean).slice(-3,-2)[0],image:local(card.querySelector('.fc-cover img')?.getAttribute('src')||''),preview:local(card.querySelector('[data-fc-preview]')?.getAttribute('data-fc-preview')||''),tags:[...card.querySelectorAll('.fc-tags .elementor-icon-list-text')].map(text),platforms:[...card.querySelectorAll('.fc-platforms .elementor-icon-list-text')].map(text),platform_links:[...card.querySelectorAll('.fc-platforms a')].map(a=>({label:text(a),url:a.getAttribute('href')})),url:local(card.querySelector('.fc-card-title a')?.getAttribute('href')||'')});
const catalogDoc=parseHTML(shared[8134]).document;
const catalog=[...catalogDoc.querySelectorAll('.fc-card')].map(cardData);
await write('Games/JSON/catalog.json',JSON.stringify(catalog,null,2)+'\n');
await write('Compartido/JSON/proyectos.json',JSON.stringify([...parseHTML(shared[8251]).document.querySelectorAll('.fc-card')].map(cardData),null,2)+'\n');
for(const {page,document:d}of docs){
  const main=d.querySelector('main#content');
  const general={title:page.title,source_url:page.view,synced_at:stamp,sections:[...main.querySelectorAll('.elementor-top-section')].filter(n=>n.querySelector('h1,h2')).map(n=>({title:text(n.querySelector('h1,h2')),text:[...n.querySelectorAll('p')].map(text),images:[...n.querySelectorAll('img')].map(i=>({image:local(i.getAttribute('src')||''),alt:i.alt||''}))}))};
  if(page.kind==='game')await write(page.folder+'/JSON/game.json',JSON.stringify({...general,slug:page.slug,title:text(main.querySelector('h1')),paragraphs:[...main.querySelectorAll('.fc-game-description p,.fc-game-story p')].map(text),gallery:[...main.querySelectorAll('a[data-elementor-lightbox-slideshow] img')].map(i=>({image:local(i.src),alt:i.alt||''})),projects:'Compartido/JSON/proyectos.json'},null,2)+'\n');
  if(page.kind==='home')await write('Home/JSON/home.json',JSON.stringify({...general,featured_game:text(main.querySelector('h1')),hero_copy:text(main.querySelector('.fc-featured .fc-description')),promotions:[...main.querySelectorAll('.fc-promo-slide')].map(n=>({title:text(n.querySelector('.fc-promo-title')),description:text(n.querySelector('.fc-description')),image:local(n.querySelector('img')?.src||'')})),catalog:'Games/JSON/catalog.json'},null,2)+'\n');
  if(page.kind==='about')await write('About/JSON/about.json',JSON.stringify({...general,history:[...main.querySelectorAll('.fc-about-story > .elementor-container > .elementor-column')].map(n=>({title:text(n.querySelector('h2')),paragraphs:[...n.querySelectorAll('p')].map(text)})),projects:'Compartido/JSON/proyectos.json'},null,2)+'\n');
  if(page.kind==='press')await write('Press/JSON/press.json',JSON.stringify({...general,articles:[...main.querySelectorAll('.fc-press-row')].map(n=>({title:text(n.querySelector('.fc-article-title')),date:text(n.querySelector('.fc-article-eyebrow')),summary:text(n.querySelector('.fc-article-summary')),image:local(n.querySelector('img')?.src||''),article_url:n.querySelector('.fc-article-link a')?.getAttribute('href')||''})),presskits:[...main.querySelectorAll('.fc-presskit')].map(n=>({title:text(n.querySelector('.fc-kit-title')),image:local(n.querySelector('img')?.src||''),download:n.querySelector('.fc-kit-download a')?.getAttribute('href')||''}))},null,2)+'\n');
  if(page.kind==='privacy')await write('Privacy/JSON/privacy.json',JSON.stringify(general,null,2)+'\n');
  if(page.kind==='indies'){
    const studios=[];
    for(const n of main.querySelectorAll('.fc-studio-row')){
      const key=[...n.classList].find(c=>c.startsWith('fc-studio-key-'))?.slice(14)||'studio-'+studios.length;
      const data={name:text(n.querySelector('.fc-studio-name')),slug:key,source_url:page.view,summary:text(n.querySelector('.fc-studio-summary')),paragraphs:[...n.querySelectorAll('.fc-studio-bio p')].map(text),images:[...n.querySelectorAll('img')].map(i=>({image:local(i.src),alt:i.alt||''})),html:`Indies/Estudios/${key}/HTML/studio.html`,data:`Indies/Estudios/${key}/JSON/studio.json`};
      await write(data.data,JSON.stringify(data,null,2)+'\n');await write(data.html,n.outerHTML.replaceAll('@root/','../../../../'));
      studios.push(data);
    }
    await write('Indies/JSON/studios.json',JSON.stringify(studios,null,2)+'\n');
  }
}
await write('Compartido/JSON/site.json',JSON.stringify({source:origin,synced_at:stamp,navigation:pages.filter(p=>p.kind!=='game').map(p=>({title:p.title,file:p.file})),shared_templates:Object.fromEntries(Object.entries(sharedNames).map(([id,name])=>[name,{wordpress_id:Number(id),file:'Compartido/HTML/'+name+'.html'}])),privacy:{published:pages.some(p=>p.kind==='privacy')}},null,2)+'\n');
console.log(`Sincronizadas ${rows.length} páginas, ${catalog.length} tarjetas y ${assets.size} recursos (${(bytes/1048576).toFixed(1)} MB).`);
