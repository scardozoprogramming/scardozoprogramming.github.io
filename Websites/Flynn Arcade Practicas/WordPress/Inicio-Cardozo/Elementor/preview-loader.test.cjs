const assert = require('node:assert/strict');
const setup = require('../JavaScript/preview-loader.js');
const tick = () => new Promise(resolve => setTimeout(resolve, 25));
function environment(urls, { saveData = false, reduced = false, failures = [] } = {}) {
  const requests = [];
  let concurrent = 0, peak = 0, observer;
  const document = { hidden:false, activeElement:null, addEventListener() {} };
  const view = {
    navigator:{connection:{saveData,effectiveType:'4g',addEventListener(){}}},
    matchMedia(query) { return {matches:query.includes('reduced') ? reduced : true,addEventListener(){}}; },
    requestAnimationFrame(callback){callback();},
    IntersectionObserver:class { constructor(callback){observer=callback;} observe(){} },
    Image:class {
      set src(url) {
        requests.push(url); concurrent++; peak=Math.max(peak,concurrent);
        setTimeout(()=>{concurrent--;if(failures.includes(url))this.onerror();else{this.naturalWidth=480;this.onload();}},3);
      }
      decode(){return Promise.resolve();}
    },
  };
  document.defaultView=view;
  const states = urls.map(url=>{
    const handlers = {};
    const attrs = new Map([['src','cover.jpg'],['srcset','cover-small.jpg 320w, cover.jpg 640w'],['sizes','400px']]);
    const classes=new Set();
    const image={alt:'',getAttribute:k=>attrs.get(k)||null,setAttribute:(k,v)=>attrs.set(k,v),removeAttribute:k=>attrs.delete(k),classList:{add:k=>classes.add(k),remove:k=>classes.delete(k)}};
    Object.defineProperty(image,'src',{set:v=>attrs.set('src',v)});
    const host={hidden:false,closest:()=>null,contains:element=>element===link,addEventListener:(event,callback)=>{handlers[event]=callback;}};
    const link={dataset:{fcPreview:url},querySelector:()=>image,closest:()=>host,getAttribute:()=> 'Discover game'};
    return {link,host,image,handlers,attrs,classes};
  });
  const root={ownerDocument:document,querySelectorAll:()=>states.map(s=>s.link)};
  const loader=setup(root);
  return {states,requests,document,loader,near(indices){observer(indices.map(i=>({target:states[i].host,isIntersecting:true})));},peak:()=>peak};
}
(async()=>{
  const preload=environment(['a.webp','b.webp','c.webp','d.webp','e.webp','f.webp']);
  preload.near([0,1,2,3,4,5]);
  await tick();
  assert.equal(preload.requests.length,6);
  assert.equal(preload.peak(),2,'Preloading must not start an unbounded batch');
  preload.states[0].handlers.pointerenter(); await tick();
  assert.equal(preload.states[0].attrs.get('src'),'a.webp');
  assert.equal(preload.requests.length,6,'A warm hover must reuse its loaded animation');
  preload.states[0].handlers.pointerleave();
  assert.equal(preload.states[0].attrs.get('src'),'cover.jpg');
  assert.match(preload.states[0].attrs.get('srcset'),/320w/);

  const shared=environment(['same.webp','same.webp']);
  shared.near([0,1]); await tick();
  assert.deepEqual(shared.requests,['same.webp'],'A card and highlight share one request');
  const limited=environment(['a.webp'],{saveData:true});
  limited.near([0]); await tick();
  assert.equal(limited.requests.length,0,'Save Data must prevent speculative downloads');
  limited.states[0].handlers.pointerenter(); await tick();
  assert.deepEqual(limited.requests,['a.webp'],'Explicit hover still works with Save Data');
  const reduced=environment(['a.webp'],{reduced:true});
  reduced.near([0]); reduced.states[0].handlers.pointerenter(); await tick();
  assert.equal(reduced.requests.length,0,'Reduced motion must keep the cover');
  const hidden=environment(['a.webp']); hidden.states[0].host.hidden=true;
  hidden.near([0]); await tick(); assert.equal(hidden.requests.length,0);
  const fallback=environment(['bad.webp'],{failures:['bad.webp']});
  fallback.states[0].link.dataset.fcPreviewFallback='fallback.gif';
  // Register the fallback as it appears in real native link attributes.
  const fallbackDocument=fallback.document;
  const fallbackRoot={ownerDocument:fallbackDocument,querySelectorAll:()=>[fallback.states[0].link]};
  setup(fallbackRoot);
  fallback.document.activeElement=fallback.states[0].link;
  fallback.states[0].handlers.focusin(); await tick();
  assert.equal(fallback.states[0].attrs.get('src'),'fallback.gif');
  console.log('PASS: bounded loading, warm hover, restoration, shared cache, Save Data, reduced motion, hidden cards, fallback');
})();
