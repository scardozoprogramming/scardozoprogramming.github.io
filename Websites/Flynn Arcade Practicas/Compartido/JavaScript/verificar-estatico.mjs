import fs from 'node:fs/promises';
import path from 'node:path';
import {parseHTML} from './vendor/linkedom.mjs';
import {root} from './compilar-compartidos.mjs';

const metadata='Compartido/JSON/Sincronizacion';
const pages=JSON.parse(await fs.readFile(path.join(root,metadata,'paginas.json'),'utf8'));
const external=JSON.parse(await fs.readFile(path.join(root,metadata,'dependencias-github.json'),'utf8'));
const keep=new Set(),pending=[],missing=[],remoteAssets=[],wordpressLinks=[];
const add=file=>{file=path.posix.normalize(file.replaceAll('\\','/'));if(file.startsWith('../')||path.posix.isAbsolute(file))throw Error('Path outside site: '+file);if(!keep.has(file)){keep.add(file);pending.push(file)}};
const paths=value=>{if(typeof value==='string')return [value];if(Array.isArray(value))return value.flatMap(paths);if(value&&typeof value==='object')return Object.entries(value).flatMap(([k,v])=>[k,...paths(v)]);return[]};
function reference(value,from,resource=false){
  if(!value||/^(?:#|data:|mailto:|tel:|javascript:|blob:)/i.test(value))return;
  if(/^https?:\/\//i.test(value)){
    if(resource&&/^https?:\/\/(?:www\.)?flynnsarcades\.com\//i.test(value))remoteAssets.push({from,url:value});
    if(!resource&&/^https?:\/\/(?:www\.)?flynnsarcades\.com\//i.test(value))wordpressLinks.push({from,url:value});
    return;
  }
  if(value.startsWith('//'))return;
  let clean;try{clean=decodeURIComponent(value.split(/[?#]/)[0])}catch{return}
  if(!clean||/[<>\n]/.test(clean))return;
  if(!/\.(?:html|css|m?js|cjs|json|jpe?g|png|gif|webp|avif|svg|ico|woff2?|ttf|eot|otf|zip|pdf|mp4|webm|txt)$/i.test(clean))return;
  const target=path.posix.normalize(path.posix.join(path.posix.dirname(from),clean));
  if(target.startsWith('../')||path.posix.isAbsolute(target)){missing.push({from,value,error:'outside root'});return}
  add(target);
}
async function walk(dir){let all=[];for(const entry of await fs.readdir(path.join(root,dir),{withFileTypes:true})){const file=dir?dir+'/'+entry.name:entry.name;if(entry.name==='.git'||(!dir&&entry.name==='versión anterior'))continue;if(entry.isDirectory())all.push(...await walk(file));else if(entry.isFile())all.push(file)}return all}
const all=await walk('');
for(const page of pages){add(page.file);add(page.template)}
for(const file of external)if(all.includes(file))add(file);
for(const file of all){
  if(/^Compartido\/JSON\//.test(file)&&!file.endsWith('recursos-pendientes.json'))add(file);
  if(/^(?:Home|Games|About|Indies|Press|Privacy)\/.*\/JSON\/(?:game|studio)\.json$/.test(file)||/^(?:Home|Games|About|Indies|Press|Privacy)\/JSON\//.test(file))add(file);
  if(/^Indies\/Estudios\/[^/]+\/HTML\/studio\.html$/.test(file))add(file);
  if(/^Compartido\/HTML\//.test(file))add(file);
  if(/^Compartido\/JavaScript\/(?:server\.cjs|compilar-compartidos\.mjs|sincronizar-wordpress\.mjs|verificar-estatico\.mjs)$/.test(file))add(file);
  if(/^Compartido\/JavaScript\/vendor\//.test(file))add(file);
  if(/(?:^|\/)(?:[^/]*LICENSE[^/]*|[^/]*OFL[^/]*|LEEME\.md|SINCRONIZACION\.md)$/i.test(file)&&!file.startsWith('WordPress/'))add(file);
}
for(let i=0;i<pending.length;i++){
  const file=pending[i];let buffer;
  try{buffer=await fs.readFile(path.join(root,file))}catch{missing.push({file,error:'missing'});continue}
  if(file.endsWith('.html')){
    const {document:d}=parseHTML(buffer.toString());
    for(const e of d.querySelectorAll('[href],[src],[poster],[data-fc-preview],[data-fc-preview-fallback]'))for(const a of['href','src','poster','data-fc-preview','data-fc-preview-fallback']){
      const v=e.getAttribute(a);if(v)reference(v.replaceAll('@root/','../'.repeat(file.split('/').length-1)),file,a!=='href'||e.tagName==='LINK');
    }
    for(const s of d.querySelectorAll('script[type="application/json"]'))try{for(const v of paths(JSON.parse(s.textContent)))if(/^(?:\.\.\/|\.\/|@root\/)/.test(v))reference(v.replaceAll('@root/','../'.repeat(file.split('/').length-1)),file,true)}catch{}
  }else if(file.endsWith('.css')){
    for(const m of buffer.toString().matchAll(/url\(\s*["']?([^"')]+)["']?\s*\)|@import\s+["']([^"']+)["']/g))reference(m[1]||m[2],file,true);
  }else if(/\.(mjs|cjs|js)$/.test(file)){
    for(const m of buffer.toString().matchAll(/(?:from\s*|import\s*)['"](\.[^'"]+)['"]/g))reference(m[1],file,true);
  }
}
const unused=all.filter(file=>!keep.has(file));
const report={pages:pages.length,kept:keep.size,unused:unused.length,missing,remoteAssets,wordpressLinks,unusedFiles:unused};
await fs.writeFile(path.join(root,metadata,'comprobacion.json'),JSON.stringify(report,null,2)+'\n');
console.log(JSON.stringify({pages:pages.length,kept:keep.size,unused:unused.length,missing:missing.slice(0,25),remoteAssets,wordpressLinks},null,2));
if(missing.length||remoteAssets.length)process.exitCode=1;
