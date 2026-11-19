import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

export const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
export const sharedNames = {7123:'header',8134:'catalogo',8251:'proyectos',8398:'footer'};

export async function buildShared({fromSnapshots=false}={}) {
  const pages = JSON.parse(await fs.readFile(path.join(root,'Compartido/JSON/Sincronizacion/paginas.json'),'utf8'));
  const blocks = Object.fromEntries(await Promise.all(Object.values(sharedNames).map(async name =>
    [name,await fs.readFile(path.join(root,'Compartido/HTML',name+'.html'),'utf8')])));
  for (const page of pages) {
    const saved = JSON.parse(await fs.readFile(path.join(root,page.template),'utf8'));
    let html = saved.html;
    if(!fromSnapshots)try{html=await fs.readFile(path.join(root,page.file),'utf8')}catch{}
    for (const [name,block] of Object.entries(blocks)) {
      const wrapped=`<!-- shared:${name}:start -->${block}<!-- shared:${name}:end -->`;
      html=html.replace(new RegExp(`<!-- shared:${name}:start -->[\\s\\S]*?<!-- shared:${name}:end -->`,'g'),()=>wrapped)
        .replaceAll(`<!-- shared:${name} -->`,wrapped);
    }
    const relativeRoot = path.posix.relative(path.posix.dirname(page.file),'.') || '.';
    html = html.replaceAll('@root/',relativeRoot+'/');
    await fs.mkdir(path.dirname(path.join(root,page.file)),{recursive:true});
    await fs.writeFile(path.join(root,page.file),html.replace(/[\t ]+$/gm,'').trimEnd()+'\n');
  }
  return pages.length;
}
if(process.argv[1] && path.resolve(process.argv[1])===fileURLToPath(import.meta.url))
  console.log(`Actualizadas ${await buildShared()} páginas desde los bloques compartidos.`);
