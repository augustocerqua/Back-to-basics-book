// Run with Node.js. Dependencies are cached outside the book repository.
// node code/skeleton-mecha/render.cjs [--preview]
const fs=require('node:fs');
const path=require('node:path');
const os=require('node:os');
const http=require('node:http');
const {createRequire}=require('node:module');
const runtime=path.join(os.homedir(),'.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const req=createRequire(path.join(runtime,'package.json'));
const {chromium}=req('playwright');
const sharp=req('sharp');
const cache=path.join(os.tmpdir(),'backbone-mecha-render');
const vendor=path.join(cache,'vendor');
const frames=path.join(cache,'frames');
const out=path.resolve(__dirname,'../../images');
async function main(){
 fs.mkdirSync(vendor,{recursive:true});fs.mkdirSync(frames,{recursive:true});
 const assets={
  'three.module.js':'build/three.module.js',
  'three.core.js':'build/three.core.js',
  'FontLoader.js':'examples/jsm/loaders/FontLoader.js',
  'TextGeometry.js':'examples/jsm/geometries/TextGeometry.js',
  'helvetiker_bold.typeface.json':'examples/fonts/helvetiker_bold.typeface.json'
 };
 for(const [name,url] of Object.entries(assets)){
  const target=path.join(vendor,name);
  if(!fs.existsSync(target)){const r=await fetch('https://cdn.jsdelivr.net/npm/three@0.180.0/'+url);if(!r.ok)throw new Error(url+': '+r.status);fs.writeFileSync(target,Buffer.from(await r.arrayBuffer()));}
 }
 const server=http.createServer((r,s)=>{
  const url=new URL(r.url,'http://localhost'),p=url.pathname.startsWith('/vendor/')?path.join(vendor,path.basename(url.pathname)):path.join(__dirname,url.pathname==='/'?'scene.html':path.basename(url.pathname));
  if(!fs.existsSync(p)){s.writeHead(404);s.end();return;}
  s.setHeader('Content-Type',p.endsWith('.html')?'text/html':p.endsWith('.json')?'application/json':'text/javascript');fs.createReadStream(p).pipe(s);
 });
 await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
 let browser;
 try{
  browser=await chromium.launch({executablePath:'C:/Program Files/Google/Chrome/Application/chrome.exe',headless:true,args:['--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  const page=await browser.newPage({viewport:{width:1440,height:1080},deviceScaleFactor:1});
  page.on('pageerror',e=>console.error('PAGE ERROR',e));
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.waitForFunction(()=>window.ready,{timeout:60000});
  const times=[1.7,4.5,7.4,10.8,13,18];
  for(const t of times){console.log('CHECK',t,await page.evaluate(t=>window.renderAt(t),t));await page.screenshot({path:path.join(cache,`preview-${t}.png`)});}
  const tiles=await Promise.all(times.map(async(t,i)=>({input:await sharp(path.join(cache,`preview-${t}.png`)).resize(480,360).toBuffer(),left:i%3*480,top:Math.floor(i/3)*360})));
  await sharp({create:{width:1440,height:720,channels:3,background:'#ffffff'}}).composite(tiles).png().toFile(path.join(cache,'contact-sheet.png'));
  if(process.argv.includes('--preview')){console.log('PREVIEWS',cache);return;}
  // 250 frames at 80 ms per frame = exactly 20 seconds.
  for(let i=0;i<250;i++){
   await page.evaluate(t=>window.renderAt(t),i*.08);
   await page.screenshot({path:path.join(frames,String(i).padStart(4,'0')+'.png')});
   if(i%25===0)console.log(`FRAME ${i}/250`);
  }
  await page.setViewportSize({width:2880,height:2160});
  await page.evaluate(()=>{window.captureScale(2);window.renderAt(18);});
  await page.screenshot({path:path.join(out,'backbone-skeleton-mecha-3d.png')});
  console.log('FRAMES',frames);
 }finally{if(browser)await browser.close();await new Promise(r=>server.close(r));}
}
main().catch(e=>{console.error(e);process.exitCode=1;});
