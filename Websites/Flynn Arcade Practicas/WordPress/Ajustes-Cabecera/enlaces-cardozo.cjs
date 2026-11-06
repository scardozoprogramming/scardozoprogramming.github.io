'use strict';

// Offline builder. Network access is used only by the explicit --refresh-public command.
const fs = require('node:fs');
const path = require('node:path');
const SNAPSHOT = path.join(__dirname, 'rutas-fuentes-publicas.json');
const GENERATED = path.join(__dirname, 'enlaces-cardozo.js');
const HEADER = '[data-elementor-id="7123"], #header7123';
// Explicit user scope: five main pages + 36 games. Point & Click (6984) is excluded.
const TARGET_IDS = new Set([6927,6929,6925,6931,6933,6935,6937,6939,6941,6943,6945,
  6947,6949,6951,6953,6955,6957,6959,6961,6876,6963,6965,6967,6969,6972,6976,
  6978,6980,6982,7098,7294,7296,7297,7298,7299,7300,7301,7302,7310,7311,7106]);

function titleText(value) {
  return String(value?.rendered ?? value ?? '').replace(/<[^>]*>/g, '')
    .replace(/&#(x[\da-f]+|\d+);/gi, (_, n) => {
      const code = n[0].toLowerCase() === 'x' ? parseInt(n.slice(1), 16) : Number(n);
      return code > 0 && code <= 0x10ffff ? String.fromCodePoint(code) : '';
    }).replace(/&(amp|apos|quot|nbsp|ndash|mdash);/gi,
      (_, n) => ({amp:'&', apos:"'", quot:'"', nbsp:' ', ndash:'-', mdash:'-'})[n.toLowerCase()]);
}
function matchKey(value) {
  return titleText(value).replace(/(?:\s|-)cardozo\s*$/i, '').normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/[^a-z0-9]/g, '');
}
function pageId(value) {
  const id = String(value ?? '').replace(/^post-/, '');
  if (!/^[1-9]\d*$/.test(id) || !Number.isSafeInteger(Number(id))) throw Error('Invalid page id: ' + value);
  return id;
}

// Self-contained: this exact function is serialized into the authored browser script.
function normalizeHref(href, config, options = {}) {
  if (typeof href !== 'string' || options.download) return href;
  const raw = href.trim();
  if (!raw || raw.startsWith('#') || /^(?:mailto|tel|javascript|data|blob):/i.test(raw)) return href;
  let url;
  try { url = new URL(raw, options.baseUrl || config.origin + '/'); } catch { return href; }
  if (!/^https?:$/.test(url.protocol) || url.username || url.password || !config.origins.includes(url.origin)) return href;
  let pathname;
  try { pathname = decodeURIComponent(url.pathname).toLowerCase(); } catch { return href; }
  if (/\/(?:wp-admin|wp-content|wp-json|wp-includes)(?:\/|$)|\/(?:wp-login|xmlrpc)\.php(?:\/|$)/i.test(pathname)
      || (pathname !== '/index.php' && /\.[a-z0-9]{1,8}\/?$/i.test(pathname))
      || ['rest_route', 'attachment_id', 'download', 's', 'feed'].some(key => url.searchParams.has(key))) return href;
  const key = pathname.replace(/\/+$/, '') + '/';
  const params = url.searchParams;
  if (pathname.replace(/[^a-z0-9]/g, '').replace(/cardozo$/, '') === 'pointclick'
      || ['page_id','p','elementor-preview','preview_id'].some(k => params.getAll(k).some(id => ['3436','6984'].includes(id)))
      || ['pagename','name'].some(k => params.getAll(k).some(name => name.toLowerCase().replace(/[^a-z0-9]/g, '').replace(/cardozo$/, '') === 'pointclick'))) return href;
  let target;
  const pathTarget = config.targets[config.paths[key]];
  // A verified target permalink wins over stale routing parameters left in widgets.
  if (pathTarget && decodeURIComponent(pathTarget.path).toLowerCase().replace(/\/+$/, '') + '/' === key) {
    target = pathTarget;
  } else if (params.has('page_id') || params.has('p') || params.has('elementor-preview')) {
    const ids = ['page_id', 'p', 'elementor-preview'].filter(k => params.has(k)).flatMap(k => params.getAll(k));
    const matches = ids.map(id => config.ids[id]);
    if (matches.some(value => !value) || matches.some(value => value !== matches[0])) return href;
    target = config.targets[matches[0]];
  } else if (params.has('pagename') || params.has('name')) {
    const names = ['pagename', 'name'].filter(k => params.has(k)).flatMap(k => params.getAll(k));
    const matches = names.map(name => config.paths['/' + name.toLowerCase().replace(/^\/+|\/+$/g, '') + '/']);
    if (matches.some(value => !value) || matches.some(value => value !== matches[0])) return href;
    target = config.targets[matches[0]];
  } else {
    target = config.targets[config.paths[key]];
  }
  if (!target) return href;
  const output = new URL(target.path, config.origin);
  // Keep unrelated query entries verbatim, including repeated keys, encoding and order.
  const routing = new Set(['page_id','p','pagename','name','preview','preview_id','preview_nonce','elementor-preview']);
  const kept = url.search.slice(1).split('&').filter(part => {
    if (!part) return false;
    let name;
    try { name = decodeURIComponent(part.split('=')[0].replace(/\+/g, ' ')); } catch { return true; }
    return !routing.has(name);
  });
  if (target.status !== 'publish' && options.previewDrafts) {
    output.pathname = '/';
    kept.unshift('page_id=' + target.id, 'preview=true');
  }
  output.search = kept.length ? '?' + kept.join('&') : '';
  output.hash = url.hash;
  return output.href;
}

function browserRuntime(config, normalize) {
  const doc = document;
  const slot = '__fcCardozoLinkNormalizer';
  const previous = window[slot];
  if (previous?.signature === config.signature) return;
  previous?.stop();
  let observer;
  let stopped = false;
  let scheduled = false;
  let active = false;
  const roots = new Set();
  const state = {signature: config.signature, stop() {
    stopped = true;
    observer?.disconnect();
    roots.clear();
    doc.removeEventListener('DOMContentLoaded', start);
  }};
  window[slot] = state;
  function hasHeader() { return !!doc.querySelector(config.headerSelector); }
  function update(anchor) {
    if (!anchor.isConnected || anchor.hasAttribute('download')) return;
    const before = anchor.getAttribute('href');
    const previewDrafts = config.previewDrafts === true ||
      (config.previewDrafts === 'logged-in' && doc.body?.classList.contains('logged-in'));
    const after = normalize(before, config, {baseUrl: doc.baseURI, previewDrafts});
    if (after !== before) anchor.setAttribute('href', after);
  }
  function scan(root) {
    if (root.matches?.('a[href]')) update(root);
    root.querySelectorAll?.('a[href]').forEach(update);
  }
  function flush() {
    scheduled = false;
    if (stopped) return;
    if (!hasHeader()) { active = false; roots.clear(); return; }
    if (!active) { active = true; roots.clear(); scan(doc); return; }
    const batch = Array.from(roots).filter(root => root.isConnected);
    roots.clear();
    // Scan only changed subtrees, with overlapping roots coalesced.
    for (const root of batch) {
      if (!batch.some(other => other !== root && other.contains(root))) scan(root);
    }
  }
  function queue(root) {
    if (root) roots.add(root);
    if (!scheduled) { scheduled = true; queueMicrotask(flush); }
  }
  function start() {
    if (stopped) return;
    active = hasHeader();
    if (active) scan(doc);
    observer = new MutationObserver(records => {
      const enabled = hasHeader();
      for (const record of records) {
        if (record.type === 'attributes') {
          if (record.attributeName === 'href' || record.attributeName === 'download') {
            // Own href mutations are idempotent, so they never schedule another write.
            if (enabled) update(record.target);
          } else queue(record.target);
        } else {
          for (const node of record.addedNodes) if (node.nodeType === 1) queue(node);
          if (record.removedNodes.length) queue();
        }
      }
    });
    observer.observe(doc.documentElement, {subtree:true, childList:true, attributes:true,
      attributeFilter:['href','download','data-elementor-id','id']});
  }
  if (doc.readyState === 'loading') doc.addEventListener('DOMContentLoaded', start, {once:true});
  else start();
}

function pathKey(value) {
  return decodeURIComponent(value).toLowerCase().replace(/\/+$/, '') + '/';
}
function build(verifiedTargets, publicSources, options = {}) {
  const snapshot = options.metadata || JSON.parse(fs.readFileSync(SNAPSHOT, 'utf8'));
  publicSources ??= snapshot.sources;
  if (!Array.isArray(verifiedTargets) || !Array.isArray(publicSources)) throw Error('Targets and sources must be arrays');
  const origin = new URL(options.origin || snapshot.origin).origin;
  const origins = [...new Set([origin, ...(options.origins || snapshot.internalOrigins || [])])].map(value => new URL(value).origin);
  const previewDrafts = options.previewDrafts ?? 'logged-in';
  if (![true, false, 'logged-in'].includes(previewDrafts)) throw Error('previewDrafts must be true, false or "logged-in"');
  const config = {version:1, origin, origins, headerSelector:HEADER, previewDrafts,
    targets:Object.create(null), paths:Object.create(null), ids:Object.create(null)};
  const keys = new Map();
  const matchedTargetIds = new Set();
  const diagnostics = {matchedSources:[], unmatchedSources:[], excludedSources:[], targetsWithoutSource:[], missingInventoryTargets:[]};
  function put(table, key, id) {
    if (Object.hasOwn(table, key) && table[key] !== id) throw Error('Ambiguous mapping: ' + key);
    table[key] = id;
  }
  function addKey(key, id) {
    if (!key) return;
    if (!keys.has(key)) keys.set(key, new Set());
    keys.get(key).add(id);
  }
  for (const record of verifiedTargets) {
    const id = pageId(record.id);
    const title = titleText(record.title);
    const slug = record.slug;
    if (!TARGET_IDS.has(Number(id)) || matchKey(title) === 'pointclick' || matchKey(slug) === 'pointclick') {
      throw Error('Target outside the explicit 41-page scope (Point & Click is excluded): ' + id);
    }
    // Never derive a target slug from title, inventory URL, local game slug or suffix.
    if (!title || typeof slug !== 'string' || !slug || /[\s/?#<>\\]/.test(slug) || slug === '.' || slug === '..') {
      throw Error('Exact verified slug and title required for target ' + id);
    }
    if (!['publish','draft','pending','future','private'].includes(record.status)) throw Error('Invalid verified status for target ' + id);
    if (Object.hasOwn(config.targets, id)) throw Error('Duplicate target id ' + id);
    const targetPath = '/' + slug + '/';
    if (/\/(?:wp-admin|wp-content|wp-json|wp-includes)(?:\/|$)|\.[a-z0-9]{1,8}\/$/i.test(decodeURIComponent(targetPath))) {
      throw Error('Unsafe target slug for ' + id);
    }
    config.targets[id] = {id:Number(id), title, slug, status:record.status, path:targetPath};
    put(config.paths, pathKey(targetPath), id);
    put(config.ids, id, id);
    addKey(matchKey(title), id);
    addKey(matchKey(slug), id);
    for (const alias of snapshot.localGameAliases || []) {
      if (pageId(alias.id) === id) {
        addKey(matchKey(alias.slug), id);
        addKey(matchKey(alias.title), id);
        put(config.paths, pathKey('/' + alias.slug + '/'), id);
      }
    }
  }
  for (const source of publicSources) {
    const id = pageId(source.id);
    if (source.status !== 'publish') throw Error('Source is not a public published page: ' + id);
    const url = new URL(source.link);
    if (!origins.includes(url.origin)) throw Error('Source outside configured site: ' + source.link);
    if (id === '3436' || matchKey(source.title) === 'pointclick' || matchKey(source.slug) === 'pointclick') {
      diagnostics.excludedSources.push({id:Number(id), title:titleText(source.title), path:url.pathname});
      continue;
    }
    const candidates = new Set([...(keys.get(matchKey(source.title)) || []), ...(keys.get(matchKey(source.slug)) || [])]);
    if (candidates.size > 1) throw Error('Ambiguous source page ' + id);
    const targetId = [...candidates][0];
    if (!targetId) { diagnostics.unmatchedSources.push({id:Number(id), title:titleText(source.title), path:url.pathname}); continue; }
    put(config.paths, pathKey(url.pathname), targetId);
    // API slug is also an evidenced source alias; home has slug 'inicio' but link '/'.
    if (source.slug) put(config.paths, pathKey('/' + source.slug + '/'), targetId);
    put(config.ids, id, targetId);
    matchedTargetIds.add(targetId);
    diagnostics.matchedSources.push({id:Number(id), path:url.pathname, targetId:Number(targetId), targetPath:config.targets[targetId].path});
  }
  for (const target of Object.values(config.targets)) {
    if (!matchedTargetIds.has(String(target.id))) diagnostics.targetsWithoutSource.push({id:target.id, title:target.title});
  }
  for (const target of snapshot.expectedTargets || []) {
    if (!Object.hasOwn(config.targets, pageId(target.id))) diagnostics.missingInventoryTargets.push(target);
  }
  const {createHash} = require('node:crypto');
  config.signature = createHash('sha256').update(JSON.stringify(config)).digest('hex');
  const serialized = JSON.stringify(config).replace(/</g, '\\u003c').replace(/\u2028/g, '\\u2028').replace(/\u2029/g, '\\u2029');
  const script = '// Generated by enlaces-cardozo.cjs; rebuild after verified slug/status changes.\n' +
    '(' + browserRuntime.toString() + ')(' + serialized + ', ' + normalizeHref.toString() + ');\n';
  return {config, script, diagnostics};
}

async function refreshPublicSources() {
  // Node fetch has no browser cookie jar. No credentials, cookies, auth, editing or POST.
  const homepage = 'https://www.flynnsarcades.com/';
  async function get(url) {
    const response = await fetch(url, {credentials:'omit', cache:'no-store', signal:AbortSignal.timeout(30000)});
    if (!response.ok) throw Error('Public GET failed: ' + response.status + ' ' + url);
    return response;
  }
  const html = await (await get(homepage)).text();
  // Include host/protocol aliases only after a public GET proves a redirect here.
  const originChecks = await Promise.all(['http://www.flynnsarcades.com/',
    'https://flynnsarcades.com/', 'http://flynnsarcades.com/'].map(async requested => {
    try {
      const response = await get(requested);
      await response.body?.cancel();
      return {requested, finalUrl:response.url, status:response.status,
        confirmed:new URL(response.url).origin === new URL(homepage).origin};
    } catch { return {requested, confirmed:false}; }
  }));
  const links = html.match(/<link\b[^>]*>/gi) || [];
  const discovery = links.find(tag => /rel=["']https:\/\/api\.w\.org\/["']/i.test(tag));
  const apiRoot = discovery?.match(/href=["']([^"']+)["']/i)?.[1];
  if (!apiRoot || new URL(apiRoot).origin !== new URL(homepage).origin) throw Error('No same-origin public REST discovery link');
  const homepageRestLink = links.find(tag => /wp-json\/wp\/v2\/pages\/\d+/.test(tag));
  const homepagePageId = Number(homepageRestLink?.match(/pages\/(\d+)/)?.[1]) || null;
  const endpoint = new URL('wp/v2/pages', apiRoot);
  const all = [];
  const snapshotRequest = String(Date.now());
  let totalPages = 1;
  for (let page = 1; page <= totalPages; page++) {
    const url = new URL(endpoint);
    url.search = new URLSearchParams({context:'view', status:'publish', per_page:'100', page:String(page),
      _fields:'id,slug,link,title,status', _cardozo_snapshot:snapshotRequest});
    const response = await get(url);
    totalPages = Number(response.headers.get('x-wp-totalpages') || 1);
    const rows = await response.json();
    for (const row of rows) {
      if (row.status !== 'publish') throw Error('Unexpected non-public record');
      all.push({id:row.id, slug:row.slug, link:row.link, title:row.title.rendered, status:row.status});
    }
  }
  const sources = all.filter(row => !/\bcardozo\s*$/i.test(titleText(row.title)));
  const cardozoRecords = all.filter(row => /\bcardozo\s*$/i.test(titleText(row.title)));
  const publicTargets = cardozoRecords.filter(row => TARGET_IDS.has(row.id));
  const gamesDir = path.join(__dirname, '../Juegos-Cardozo');
  const read = name => JSON.parse(fs.readFileSync(path.join(gamesDir, name), 'utf8'));
  const expectedTargets = read('inventario-paginas-restantes.json').filter(row => TARGET_IDS.has(Number(pageId(row.id))))
    .map(row => ({id:Number(pageId(row.id)), title:publicTargets.find(p => p.id === Number(pageId(row.id)))?.title || row.title}));
  const localGameAliases = ['restantes.json', 'paginas.json'].flatMap(file => read(file).map(row =>
    ({id:Number(pageId(row.id)), title:row.title, slug:row.slug, provenance:'WordPress/Juegos-Cardozo/' + file})))
    .filter(row => TARGET_IDS.has(row.id));
  const publicHomeTarget = publicTargets.find(row => row.id === 6876);
  const headerHtml = publicHomeTarget ? await (await get(publicHomeTarget.link)).text() : '';
  const snapshot = {collectedAt:new Date().toISOString(), origin:new URL(homepage).origin,
    internalOrigins:[new URL(homepage).origin, ...originChecks.filter(row => row.confirmed).map(row => new URL(row.requested).origin)],
    apiRoot, endpoint:endpoint.href, originChecks,
    discovery:{homepage, apiLinkTag:discovery, homepagePageId,
      headerPage:publicHomeTarget?.link || null, header7123Observed:/data-elementor-id=["']7123["']/.test(headerHtml),
      headerClassesObserved:headerHtml.includes('fc-shared-header fc-shared-instance')},
    access:'Public unauthenticated GET only; no cookies or credentials. Local inventories supply titles/ids and game aliases, never verified target slugs.',
    scope:{targetCount:41, excludedTargetIds:[6984], excludedSourceIds:[3436], excludedCategory:'Point & Click'},
    sources:sources.sort((a,b) => a.id-b.id), publicTargets:publicTargets.sort((a,b) => a.id-b.id),
    excludedPublicTargets:cardozoRecords.filter(row => !TARGET_IDS.has(row.id)), expectedTargets, localGameAliases};
  fs.writeFileSync(SNAPSHOT, JSON.stringify(snapshot, null, 2) + '\n');
  return snapshot;
}

function runChecks() {
  const assert = require('node:assert/strict');
  const vm = require('node:vm');
  const metadata = {origin:'https://www.flynnsarcades.com', internalOrigins:['https://www.flynnsarcades.com'],
    localGameAliases:[{id:6939, title:'Dexter Cardozo', slug:'dexter-stardust'},
      {id:7299, title:'Space Ducks: The great escape Cardozo', slug:'space-ducks-the-great-escape'}]};
  const sources = [
    {id:3231, title:'Inicio', slug:'inicio', link:metadata.origin+'/', status:'publish'},
    {id:4032, title:'About', slug:'about-3', link:metadata.origin+'/about-3/', status:'publish'},
    {id:4676, title:'Dexter', slug:'dexter', link:metadata.origin+'/dexter/', status:'publish'},
    {id:5060, title:'Space Ducks: The great escape', slug:'spaceducks', link:metadata.origin+'/spaceducks/', status:'publish'},
    {id:6797, title:'BoardWalk Builders', slug:'boardwalk-builders', link:metadata.origin+'/boardwalk-builders/', status:'publish'}];
  // Deliberately non-inferred destination names prove output uses verified input.
  const targets = [
    {id:6876, title:'Inicio Cardozo', slug:'inicio-cardozo', status:'publish'},
    {id:6925, title:'About Cardozo', slug:'about-cardozo', status:'publish'},
    {id:6939, title:'Dexter Stardust Cardozo', slug:'verified-dexter-27', status:'publish'},
    {id:7299, title:'Space Ducks: The great escape Cardozo', slug:'verified-space-ducks-12', status:'draft'},
    {id:6933, title:'Boardwalk Builders Cardozo', slug:'verified-boardwalk-9', status:'publish'}];
  const result = build(targets, sources, {metadata});
  const n = (href, opts) => normalizeHref(href, result.config, opts);
  const O = metadata.origin;
  assert.equal(result.diagnostics.matchedSources.length, 5);
  assert.equal(n('/'), O+'/inicio-cardozo/');
  assert.equal(n('/inicio/'), O+'/inicio-cardozo/');
  assert.equal(n('/about-3?utm=x#team'), O+'/about-cardozo/?utm=x#team');
  assert.equal(n('/dexter/?page_id=4676&preview=true&utm=a%20b&utm=c#buy'), O+'/verified-dexter-27/?utm=a%20b&utm=c#buy');
  assert.equal(n('/?page_id=4676'), O+'/verified-dexter-27/');
  assert.equal(n('/?p=4676'), O+'/verified-dexter-27/');
  assert.equal(n('/index.php?page_id=4676'), O+'/verified-dexter-27/');
  assert.equal(n('/dexter-stardust/'), O+'/verified-dexter-27/');
  assert.equal(n('/?pagename=dexter'), O+'/verified-dexter-27/');
  assert.equal(n('/BoardWalk-Builders'), O+'/verified-boardwalk-9/');
  assert.equal(n('/spaceducks/'), O+'/verified-space-ducks-12/');
  assert.equal(n('/spaceducks/?utm=abc#shots', {previewDrafts:true}), O+'/?page_id=7299&preview=true&utm=abc#shots');
  assert.equal(n('/verified-space-ducks-12/', {previewDrafts:true}), O+'/?page_id=7299&preview=true');
  assert.equal(n('/?page_id=7299&preview=true', {previewDrafts:true}), O+'/?page_id=7299&preview=true');
  assert.equal(n('/verified-dexter-27/?page_id=6939&preview=true&preview_id=2&preview_nonce=x#buy'), O+'/verified-dexter-27/#buy');
  assert.equal(n('/verified-dexter-27/?page_id=99999&utm=a'), O+'/verified-dexter-27/?utm=a');
  assert.equal(n('/verified-dexter-27/?page_id=3231'), O+'/verified-dexter-27/');
  assert.equal(n('../dexter/', {baseUrl:O+'/games/'}), O+'/verified-dexter-27/');
  const unchanged = ['#overview','mailto:a@b.com','tel:123','javascript:void(0)',
    'https://store.steampowered.com/dexter/','https://twitter.com/FlynnsArcades',
    'https://www.flynnsarcades.com.evil.test/dexter/','http://www.flynnsarcades.com/dexter/',
    '/wp-admin/','/wp-json/wp/v2/pages','/wp-content/uploads/cover.webp','/wp-login.php',
    '/dexter.png','/dexter/?download=1','/?rest_route=/wp/v2/pages','/?attachment_id=4676',
    '/?page_id=99999','/?page_id=4676&page_id=3231','/?s=dexter','/unknown/','/%ZZ/'];
  for (const href of unchanged) assert.equal(n(href), href);
  assert.equal(n('/dexter/', {download:true}), '/dexter/');
  for (const href of ['/', '/dexter/', '/spaceducks/', '/?page_id=4676']) {
    for (const previewDrafts of [true,false]) assert.equal(n(n(href,{previewDrafts}),{previewDrafts}), n(href,{previewDrafts}));
  }
  assert.throws(() => build([{...targets[0], slug:undefined}], sources, {metadata}), /Exact verified slug/);
  assert.throws(() => build([{...targets[0], status:'Elementor'}], sources, {metadata}), /Invalid verified status/);
  assert.throws(() => build([{id:6984,title:'Point & Click Cardozo',slug:'point-click-cardozo',status:'draft'}], sources, {metadata}), /41-page scope/);
  assert.throws(() => build([{...targets[0],id:99999}], sources, {metadata}), /41-page scope/);
  assert.equal(n('/point-click/?page_id=4676'), '/point-click/?page_id=4676');
  assert.equal(n('/?page_id=3436'), '/?page_id=3436');
  assert.equal(n('/?page_id=6984'), '/?page_id=6984');
  const plus = build([{id:6927,title:'8-Colors Star Guardians + Cardozo',slug:'verified-eight-colors',status:'publish'}],
    [{id:5746,title:'8-Colors Star Guardians',slug:'8-colors-star-guardians',link:O+'/8-colors-star-guardians/',status:'publish'}], {metadata});
  assert.equal(plus.diagnostics.matchedSources.length,1);
  assert.throws(() => build([targets[0], {...targets[1], slug:targets[0].slug}], sources, {metadata}), /Ambiguous mapping/);
  new vm.Script(result.script);
  // Minimal DOM harness: no browser/UI. Check guard, real href writes and observer idempotence.
  let callback, scans = 0, writes = 0, observers = 0, header = false;
  const anchor = {href:'/dexter/', isConnected:true, hasAttribute:() => false,
    getAttribute() { return this.href; }, setAttribute(_name,value) { writes++; this.href=value; }};
  const document = {readyState:'complete', baseURI:O+'/', body:{classList:{contains:() => true}}, documentElement:{},
    querySelector:() => header ? {} : null, querySelectorAll:() => { scans++; return [anchor]; }, removeEventListener() {}};
  const context = {document, window:{}, URL, Set, queueMicrotask:fn=>fn(), MutationObserver:class {
    constructor(fn) { callback=fn; observers++; } observe() {} disconnect() {}
  }};
  vm.runInNewContext(result.script, context);
  assert.equal(writes,0); assert.equal(scans,0);
  header=true;
  callback([{type:'childList', addedNodes:[{nodeType:1}], removedNodes:[]}]);
  assert.equal(anchor.href,O+'/verified-dexter-27/'); assert.equal(writes,1);
  callback([{type:'attributes', attributeName:'href', target:anchor}]);
  assert.equal(writes,1);
  anchor.href='/spaceducks/';
  callback([{type:'attributes', attributeName:'href', target:anchor}]);
  assert.equal(anchor.href,O+'/?page_id=7299&preview=true');
  header=false; anchor.href='/dexter/';
  callback([{type:'attributes', attributeName:'href', target:anchor}]);
  assert.equal(anchor.href,'/dexter/');
  vm.runInNewContext(result.script, context);
  assert.equal(observers,1);
  header=true;
  const added = {nodeType:1,isConnected:true,matches:selector=>selector==='a[href]',
    href:'/about-3/',hasAttribute:()=>false,getAttribute(){return this.href;},
    setAttribute(_name,value){this.href=value;},querySelectorAll:()=>[],contains:()=>false};
  callback([{type:'childList',addedNodes:[added],removedNodes:[]}]);
  assert.equal(added.href,O+'/about-cardozo/');
  // Real public metadata + clearly synthetic draft slugs exercise all 41 source associations.
  // These fixtures stay in memory and are never written into the generated browser artifact.
  if (fs.existsSync(SNAPSHOT)) {
    const snapshot = JSON.parse(fs.readFileSync(SNAPSHOT,'utf8'));
    const fixtures = snapshot.expectedTargets.map(row => snapshot.publicTargets.find(p=>p.id===row.id) ||
      {...row,slug:'test-fixture-' + row.id,status:'draft'});
    const all = build(fixtures,snapshot.sources,{metadata:snapshot});
    assert.equal(fixtures.length,41);
    assert.equal(all.diagnostics.matchedSources.length,41);
    assert.deepEqual(all.diagnostics.targetsWithoutSource,[]);
    assert.deepEqual(all.diagnostics.missingInventoryTargets,[]);
    assert.deepEqual(all.diagnostics.unmatchedSources.map(row=>row.id).sort((a,b)=>a-b),[4100,6326]);
    assert.equal(all.config.targets['6984'],undefined);
    assert.equal(all.config.paths['/point-click/'],undefined);
    assert.equal(all.config.ids['3436'],undefined);
    for (const source of all.diagnostics.matchedSources) {
      const expected=all.config.origin+source.targetPath;
      assert.equal(normalizeHref(source.path,all.config),expected);
      assert.equal(normalizeHref('/?page_id='+source.id,all.config),expected);
      assert.equal(normalizeHref(source.targetPath,all.config),expected);
    }
    for (const origin of snapshot.internalOrigins) {
      assert.equal(normalizeHref(origin+'/dexter/',all.config),all.config.origin+all.config.targets['6939'].path);
      const media=origin+'/wp-content/uploads/cover.webp';
      assert.equal(normalizeHref(media,all.config),media);
    }
  }
  return 'Passed URL semantics, exact-slug validation, idempotence, script parsing and DOM observer checks.';
}

module.exports = {build, buildNormalizer:build, normalizeHref, refreshPublicSources, runChecks};

if (require.main === module) {
  (async () => {
    const [command, input] = process.argv.slice(2);
    if (command === '--check') console.log(runChecks());
    else if (command === '--refresh-public') {
      const snapshot = await refreshPublicSources();
      console.log(JSON.stringify({sources:snapshot.sources.length, publicTargets:snapshot.publicTargets.length, saved:SNAPSHOT}));
    } else if (command === '--public' || (command === '--build' && input)) {
      const snapshot = JSON.parse(fs.readFileSync(SNAPSHOT, 'utf8'));
      const targets = command === '--public' ? snapshot.publicTargets : JSON.parse(fs.readFileSync(path.resolve(input), 'utf8'));
      const result = build(targets, snapshot.sources);
      fs.writeFileSync(GENERATED, result.script);
      console.log(JSON.stringify({saved:GENERATED, targets:targets.length, diagnostics:result.diagnostics}, null, 2));
    } else throw Error('Usage: node enlaces-cardozo.cjs --check | --refresh-public | --public | --build exact-verified-targets.json');
  })().catch(error => { console.error(error.message); process.exitCode = 1; });
}
