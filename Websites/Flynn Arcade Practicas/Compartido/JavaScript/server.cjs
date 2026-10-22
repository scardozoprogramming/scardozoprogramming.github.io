const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');

const root = path.resolve(__dirname, '../..');
const folders = new Set(['Home', 'Games', 'Indies', 'About', 'Press', 'Compartido']);
const types = {
  '.html': 'text/html; charset=utf-8', '.css': 'text/css; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8', '.json': 'application/json; charset=utf-8',
  '.jpg': 'image/jpeg', '.jpeg': 'image/jpeg', '.png': 'image/png',
  '.webp': 'image/webp', '.svg': 'image/svg+xml', '.ttf': 'font/ttf',
  '.zip': 'application/zip', '.md': 'text/markdown; charset=utf-8', '.txt': 'text/plain; charset=utf-8',
};
const insideRoot = (file) => {
  const relative = path.relative(root, file);
  return relative !== '..' && !relative.startsWith('..' + path.sep) && !path.isAbsolute(relative);
};
const server = http.createServer((request, response) => {
  response.setHeader('X-Content-Type-Options', 'nosniff');
  if (!['GET', 'HEAD'].includes(request.method)) {
    response.writeHead(405, { Allow: 'GET, HEAD' }); response.end(); return;
  }
  let url;
  let name;
  try {
    url = new URL(request.url, 'http://localhost');
    name = decodeURIComponent(url.pathname).replace(/^\/+/, '');
  } catch { response.writeHead(400); response.end('Bad request'); return; }
  if (!name) {
    response.writeHead(302, { Location: '/Home/HTML/index.html' + url.search }); response.end(); return;
  }
  if (!folders.has(name.split('/')[0]) || name.includes('\0') || name.includes('\\')) {
    response.writeHead(404); response.end('Not found'); return;
  }
  try {
    const target = path.resolve(root, name);
    if (!insideRoot(target)) throw new Error('Outside site');
    const real = fs.realpathSync(target);
    if (!insideRoot(real)) throw new Error('Outside site');
    const stat = fs.statSync(real);
    if (stat.isDirectory()) {
      const page = path.join(real, 'HTML/index.html');
      if (!fs.existsSync(page)) throw new Error('Not a page');
      response.writeHead(302, { Location: '/' + path.relative(root, page).replaceAll('\\', '/') + url.search });
      response.end(); return;
    }
    const extension = path.extname(real).toLowerCase();
    if (!stat.isFile() || !types[extension]) throw new Error('Not a public file');
    const etag = `W/"${stat.size}-${Math.floor(stat.mtimeMs)}"`;
    response.setHeader('ETag', etag);
    response.setHeader('Cache-Control', 'no-cache');
    if (request.headers['if-none-match'] === etag) { response.writeHead(304); response.end(); return; }
    response.setHeader('Content-Type', types[extension]);
    response.setHeader('Content-Length', stat.size);
    if (extension === '.zip') response.setHeader('Content-Disposition', `attachment; filename="${path.basename(real)}"`);
    response.writeHead(200);
    if (request.method === 'HEAD') response.end();
    else fs.createReadStream(real).on('error', () => response.destroy()).pipe(response);
  } catch { response.writeHead(404); response.end('Not found'); }
});

module.exports = server;
if (require.main === module) {
  server.on('error', (error) => { console.error(error.message); process.exitCode = 1; });
  server.listen(Number(process.env.PORT || 4173), '127.0.0.1', () => console.log(`Flynn's Arcade: http://127.0.0.1:${server.address().port}/Home/HTML/index.html`));
}
