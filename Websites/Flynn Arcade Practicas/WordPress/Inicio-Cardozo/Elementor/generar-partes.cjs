// Native header/footer for Inicio Cardozo; no shared theme templates are edited.
const fs = require('node:fs');
const path = require('node:path');
const base = path.resolve(__dirname, '..');
let seed = 0xfc30000;
const id = () => (++seed).toString(16);
const zero = {unit:'px',top:'0',right:'0',bottom:'0',left:'0',isLinked:true};
const link = (url,attributes='') => ({url,is_external:'',nofollow:'',custom_attributes:attributes});
const widget = (type,title,settings={},classes='') => ({id:id(),elType:'widget',widgetType:type,settings:{_title:title,_css_classes:classes,_margin:zero,...settings},elements:[]});
const col = (title,elements,size=100,classes='') => ({id:id(),elType:'column',isInner:false,settings:{_title:title,_column_size:size,padding:zero,css_classes:classes},elements});
const section = (title,elements,classes,tag='section') => ({id:id(),elType:'section',isInner:false,settings:{_title:title,layout:'boxed',content_width:{unit:'px',size:1260},gap:'no',padding:zero,html_tag:tag,css_classes:'fc-home '+classes},elements});
const heading = (title,text,tag='h2',classes='') => widget('heading',title,{title:text,header_size:tag},classes);
const para = (title,text,classes='') => widget('text-editor',title,{editor:'<p>'+text+'</p>'},classes);
function button(title,url,classes='',icon='',attributes='') {
  return widget('button',title,{text:title,link:link(url,attributes),...(icon?{selected_icon:{value:icon,library:icon.startsWith('fab ')?'fa-brands':icon.startsWith('far ')?'fa-regular':'fa-solid'},icon_align:'left'}:{})},classes);
}
const socials = [
 ['Bluesky','https://bsky.app/profile/flynnsarcade.bsky.social','fas fa-circle','fc-social-bluesky'],
 ['Twitter','https://twitter.com/FlynnsArcades','fab fa-twitter',''],
 ['Instagram','https://www.instagram.com/flynnsarcadegames/','fab fa-instagram',''],
 ['TikTok','https://www.tiktok.com/@flynnsarcades','fab fa-tiktok',''],
 ['Steam','https://store.steampowered.com/publisher/Flynns','fab fa-steam',''],
 ['YouTube','https://www.youtube.com/user/jpd92363','fab fa-youtube',''],
 ['Facebook','https://www.facebook.com/FlynnsArcades','fab fa-facebook',''],
 ['Twitch','https://www.twitch.tv/flynnsarcades','fab fa-twitch',''],
 ['LinkedIn','https://www.linkedin.com/company/flynnsarcade/','fab fa-linkedin',''],
];
const socialWidgets = count => socials.slice(0,count).map(([name,url,icon,classes]) => {
  const b=button(name,url,'fc-home-social '+classes,icon);
  b.settings.link.is_external='on';
  b.settings.link.custom_attributes='rel|noopener noreferrer';
  return b;
});
const page = 'https://www.flynnsarcades.com/inicio-cardozo/';
const logo = widget('image','Publishing logo',{image:{id:'',url:'https://scardozoprogramming.github.io/Websites/Flynn%20Arcade%20Practicas/Compartido/Images/Brand/flynns-publishing-pixel.png'},image_size:'full',link_to:'custom',link:link(page,'aria-label|Flynn\'s Arcade home'),caption_source:'none',open_lightbox:'no'},'fc-home-logo');
const nav = [
 ['Home',page],['Games','https://www.flynnsarcades.com/games/'],['Indies','https://www.flynnsarcades.com/indies/'],['About','https://www.flynnsarcades.com/about-3/'],['Press','https://www.flynnsarcades.com/press/']
].map(([title,url],i)=>button(title,url,'fc-home-menu-item'+(i===0?' fc-active':''),'',i===0?'aria-current|page':''));
const header=section('00A · Home header · GitHub design',[
 col('Logo',[logo],25,'fc-logo-col'),
 col('Navigation',nav,50,'fc-nav-col'),
 col('Social channels & mobile menu',[...socialWidgets(5),button('Open navigation','#home-navigation','fc-home-nav-toggle','fas fa-bars','role|button,aria-expanded|false')],25,'fc-header-social-col'),
],'fc-local-header','header');
const footer=[
 section('05 · Find us everywhere',[
   col('Social introduction',[heading('Social title','Find us everywhere'),para('Social description','Except Tik Tok... at the last dance we broke our hips.')],30,'fc-social-copy'),
   col('Social channels',socialWidgets(9),70,'fc-footer-social-col'),
 ],'fc-local-footer fc-social-footer'),
 section('06 · Better news & Send prayers',[
   col('Better news',[
     heading('Newsletter title','Better news'),
     para('Newsletter introduction','Get all updates about our games release and more.'),
     para('Newsletter invitation',"Don't be shy."),
     para('Newsletter status','Newsletter coming soon','fc-news-status'),
     button('Follow us on Steam','https://store.steampowered.com/publisher/Flynns','fc-home-textlink','fas fa-external-link-alt'),
   ],50,'fc-contact-col fc-newsletter-col'),
   col('Send prayers',[
     heading('Contact title','Send prayers'),
     para('Contact introduction','You may prefer to be the first to contact us. We understand.'),
     para('Contact note','The newsletter is a complicated relationship.'),
     button('press@flynnsarcades.com','mailto:press@flynnsarcades.com','fc-home-email','far fa-envelope'),
   ],50,'fc-contact-col'),
 ],'fc-local-footer fc-contact-footer'),
 section('07 · Footer credits',[
   col('Studio name',[para('Studio name',"Flynn's Arcade")],33,'fc-credit-brand'),
   col('Studio motto',[para('Studio motto','Independent games. Arcade soul.')],34,'fc-credit-motto'),
   col('About link',[button('About the studio','https://www.flynnsarcades.com/about-3/','fc-home-textlink','fas fa-arrow-right')],33,'fc-credit-link'),
 ],'fc-local-footer fc-footer-credits'),
];
function writeClipboard(file,elements){fs.writeFileSync(path.join(__dirname,file),JSON.stringify({type:'elementor',siteurl:'https://www.flynnsarcades.com/wp-json/',elements},null,2));}
writeClipboard('Home-cabecera-portapapeles.json',[header]);
writeClipboard('Home-pie-portapapeles.json',footer);
fs.writeFileSync(path.join(__dirname,'Home-partes-nativas.json'),JSON.stringify({version:'0.4',title:'Inicio Cardozo · Header and footer',type:'page',content:[header,...footer],page_settings:[]},null,2));
console.log(JSON.stringify({header:header.id,footer:footer.map(f=>f.id)}));
