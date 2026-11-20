/* Capture-only observability. Loaded by the local preview with ?portfolio=1. */
(()=>{const html=document.documentElement;let queued=false;let layer;
 const position=()=>{html.dataset.captureY=String(Math.round(scrollY));html.dataset.captureBottom=String(Math.abs(document.documentElement.scrollHeight-innerHeight-scrollY)<2);};
 const rebuild=()=>{queued=false;if(!layer){layer=document.createElement('div');layer.id='portfolio-capture-anchors';layer.setAttribute('aria-hidden','true');layer.style.cssText='position:absolute;top:0;left:0;width:0;height:0;pointer-events:none;';document.body.append(layer)}
 layer.style.top='0px';layer.style.top=-(layer.getBoundingClientRect().top+scrollY)+'px';
 const height=document.documentElement.scrollHeight,max=Math.max(0,height-innerHeight),step=Math.max(108,Math.floor((innerHeight-120)/108)*108);const positions=[];for(let y=0;y<max;y+=step)positions.push(y);if(positions.at(-1)!==max)positions.push(max);
 layer.replaceChildren(...positions.map(y=>{const a=document.createElement('span');a.id='portfolio-scroll-'+y;a.style.cssText='position:absolute;left:0;top:'+y+'px;width:0;height:0;scroll-margin:0;';return a}));const bottom=document.createElement('span');bottom.id='portfolio-bottom';bottom.style.cssText='position:absolute;top:'+max+'px;width:0;height:0;scroll-margin:0;';layer.append(bottom);
 html.dataset.capturePositions=JSON.stringify(positions);html.dataset.captureHeight=String(height);position();html.dataset.captureReady='true';};
 const schedule=()=>{html.dataset.captureReady='false';if(!queued){queued=true;requestAnimationFrame(rebuild)}};
 const style=document.createElement('style');style.textContent='html{scroll-behavior:auto!important;scroll-padding-top:0!important}';document.head.append(style);
 document.addEventListener('scroll',position,{passive:true});document.addEventListener('click',schedule);window.addEventListener('resize',schedule);new ResizeObserver(schedule).observe(document.body);document.fonts.ready.then(schedule);window.addEventListener('load',schedule);schedule();})();
