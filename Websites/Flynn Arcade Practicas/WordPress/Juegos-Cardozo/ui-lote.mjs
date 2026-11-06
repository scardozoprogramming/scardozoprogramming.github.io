// Used only inside cua_repl with the documented browser UI API.
import fs from 'node:fs/promises';
const frame = tab => tab.playwright.frameLocator('#elementor-preview-iframe');
const state = tab => tab.getAXState({emit:false,disableDiffing:true});
async function clickAction(tab, expression) {
  const ax = await state(tab), found = ax.match(expression);
  if(!found) throw Error('Visible action unavailable: '+expression);
  await tab.click(Number(found[1]));
  return tab.getAXState({emit:false});
}
export async function backupAndClear(tab, base, target) {
  const dest=base+'/Juegos/'+target.slug+'/Respaldo';
  await fs.mkdir(dest,{recursive:true});
  await fs.writeFile(dest+'/antes.html',await frame(tab).locator('body').evaluate(el=>el.innerHTML));
  await frame(tab).locator('#elementor-add-new-section').click({button:'right'});
  await clickAction(tab,/(\d+) Copiar todo el contenido/);
  const backup=await tab.clipboard.readText();
  if(backup.startsWith('{"type":"elementor"')) await fs.writeFile(dest+'/antes-elementor.json',backup);
  await frame(tab).locator('#elementor-add-new-section').click({button:'right'});
  await clickAction(tab,/(\d+) Eliminar todo el contenido/);
  await tab.playwright.getByRole('button',{name:'Borrar',exact:true}).click();
  await tab.getAXState({emit:false});
  return {backedUp:backup.startsWith('{"type":"elementor"') ? JSON.parse(backup).elements.length : 'Rendered HTML retained; WordPress revisions available'};
}
export async function pastePrepared(tab,base,target) {
  await tab.clipboard.writeText(await fs.readFile(base+'/Juegos/'+target.slug+'/Elementor/portapapeles.json','utf8'));
  await frame(tab).locator('#elementor-add-new-section').click({button:'right'});
  await clickAction(tab,/(\d+) Pegar desde otro sitio/);
  const ax=await state(tab), field=ax.match(/(\d+) text field .*elementor-paste-area-dialog__input/);
  if(!field)throw Error('Native paste dialog unavailable');
  await tab.pressKey(Number(field[1]),'Control+v');
  await tab.getAXState({emit:false});
  const title=await frame(tab).locator('.fc-game-title').innerText();
  return {inserted:title};
}
export async function checkMobile(tab) {
  await clickAction(tab,/(\d+) tab .*Description: Móvil vertical/);
  const check=await frame(tab).locator('body').evaluate(body=>({width:body.clientWidth,sections:[...body.querySelectorAll('.fc-game-page,.fc-game-purchase,.fc-game-about,.fc-game-related')].map(e=>({width:Math.round(e.getBoundingClientRect().width),scroll:e.scrollWidth}))}));
  await clickAction(tab,/(\d+) tab .*Description: Escritorio/);
  if(check.sections.some(s=>s.scroll>s.width+1))throw Error('Mobile overflow');
  return check;
}
export async function saveDraft(tab) {
  await clickAction(tab,/(\d+) checkbox Description: Ajustes de Página/);
  await tab.playwright.getByLabel('Estructura de página',{exact:true}).selectOption({label:'Lienzo de Elementor'});
  await tab.getAXState({emit:false});
  await tab.playwright.getByRole('button',{name:'Guardar opciones',exact:true}).click();
  await tab.getAXState({emit:false});
  await tab.playwright.getByRole('menuitem',{name:'Guardar borrador',exact:true}).click();
  return tab.getAXState({emit:false});
}
export async function verifyPreview(tab,expected) {
  const check=await tab.playwright.evaluate(()=>({title:document.querySelector('h1')?.textContent,headers:document.querySelectorAll('#fc-cardozo-navigation').length,overflow:document.documentElement.scrollWidth>innerWidth,captures:[...document.querySelectorAll('.fc-game-gallery img')].map(i=>({src:i.getAttribute('src'),href:i.closest('a')?.getAttribute('href')})),anchors:[...document.querySelectorAll('.fc-game-sections a')].map(a=>({href:a.getAttribute('href'),exists:!!document.querySelector(a.getAttribute('href'))})),stores:[...document.querySelectorAll('.fc-game-purchase a')].map(a=>a.href),trailer:document.querySelector('.fc-game-video')?.getAttribute('data-settings'),related:[...document.querySelectorAll('.fc-game-related h3')].map(e=>e.textContent)}));
  if(check.title!==expected||check.headers!==1||check.overflow||check.captures.length!==5||check.anchors.some(a=>!a.exists))throw Error('Preview validation failed: '+JSON.stringify(check));
  await tab.playwright.locator('.fc-game-shot a').first().click();
  let ax=await tab.getAXState({emit:false});check.galleryModal=ax.includes(expected+' screenshots');
  await tab.playwright.getByRole('button',{name:'Next screenshot',exact:true}).click();
  check.galleryNext=(await tab.getAXState({emit:false})).includes('2 / 5');
  await tab.pressKey(null,'Escape');await tab.getAXState({emit:false});
  await tab.playwright.locator('.fc-game-animation-toggle a').click();await tab.getAXState({emit:false});
  await tab.playwright.locator('.fc-game-animation-toggle a').click();await tab.getAXState({emit:false});
  check.animation=await tab.playwright.locator('.fc-game-animation img').evaluate(i=>({src:i.getAttribute('src'),loaded:i.complete&&i.naturalWidth>0}));
  if(await tab.playwright.locator('.fc-game-review-next a').count()) {
    await tab.playwright.locator('.fc-game-review-next a').click();await tab.getAXState({emit:false});
    check.reviewNext=await tab.playwright.locator('.fc-game-review-count').innerText();
    await tab.playwright.locator('.fc-game-review-prev a').click();await tab.getAXState({emit:false});
    check.reviewRestored=await tab.playwright.locator('.fc-game-review-count').innerText();
  }
  return check;
}
