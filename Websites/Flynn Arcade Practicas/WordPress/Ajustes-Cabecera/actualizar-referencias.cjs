const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname,'..');
const reference = require('./referencia.cjs');
const pages = ['Inicio','Games','About','Indies','Press'];
for(const name of pages) {
  const dir=path.join(root,name+'-Cardozo','Elementor');
  for(const suffix of ['bloques','portapapeles']) {
    const file=path.join(dir,name+'-Cardozo-'+suffix+'.json');
    const doc=JSON.parse(fs.readFileSync(file));
    const contents=doc.content||doc.elements;
    if(!contents[1]?.settings?.css_classes?.match(/fc-local-header|fc-shared-header-reference/))throw new Error('Unexpected header: '+file);
    contents[1]=reference();
    fs.writeFileSync(file,JSON.stringify(doc,null,suffix==='bloques'?2:undefined));
  }
}
const home=path.join(root,'Inicio-Cardozo','Elementor');
fs.writeFileSync(path.join(home,'Home-cabecera-portapapeles.json'),JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements:[reference()]}));
const partsPath=path.join(home,'Home-partes-nativas.json');
const parts=JSON.parse(fs.readFileSync(partsPath));parts.content[0]=reference();fs.writeFileSync(partsPath,JSON.stringify(parts,null,2));
console.log('Five local page exports now reference Elementor template 7123.');
