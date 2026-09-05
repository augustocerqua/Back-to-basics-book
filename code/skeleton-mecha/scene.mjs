import * as THREE from 'three';
import {FontLoader} from 'three/addons/FontLoader.js';
import {TextGeometry} from 'three/addons/TextGeometry.js';

// A deterministic 20-second film: renderAt(t) also allows frame-by-frame export.
const scene=new THREE.Scene();
scene.background=new THREE.Color('#f7f8fa');
const renderer=new THREE.WebGLRenderer({antialias:true,preserveDrawingBuffer:true});
renderer.setSize(innerWidth,innerHeight);
renderer.setPixelRatio(1);
renderer.outputColorSpace=THREE.SRGBColorSpace;
document.body.prepend(renderer.domElement);
const camera=new THREE.PerspectiveCamera(34,innerWidth/innerHeight,0.1,100);
scene.add(new THREE.HemisphereLight(0xffffff,0x8b979c,2.3));
const key=new THREE.DirectionalLight(0xffffff,3.2);key.position.set(-5,7,8);scene.add(key);
const rim=new THREE.DirectionalLight(0xbde7ed,2.5);rim.position.set(6,3,-4);scene.add(rim);
const font=await new FontLoader().loadAsync('/vendor/helvetiker_bold.typeface.json');
const root=new THREE.Group();scene.add(root);
const ink=new THREE.MeshStandardMaterial({color:0x121b23,roughness:0.44,metalness:0.3});
const bone=new THREE.MeshStandardMaterial({color:0xeeeef0,roughness:0.36,metalness:0.5});
const joint=new THREE.MeshStandardMaterial({color:0x758b90,roughness:0.27,metalness:0.8});
const accent=new THREE.MeshStandardMaterial({color:0x1a9c9d,emissive:0x0b7479,emissiveIntensity:0.32,metalness:0.5,roughness:0.35});
const gold=new THREE.MeshStandardMaterial({color:0xba8f37,roughness:0.4,metalness:0.7});
const parts=[];
const geomCache=new Map();
function text(parent,word,x,y,z,width,height,rotation=0,material=ink){
  const k=word;
  if(!geomCache.has(k)){
    const g=new TextGeometry(word,{font,size:1,depth:0.1,curveSegments:3,bevelEnabled:true,bevelThickness:0.007,bevelSize:0.005,bevelSegments:1});
    g.computeBoundingBox();g.translate(-(g.boundingBox.max.x+g.boundingBox.min.x)/2,-(g.boundingBox.max.y+g.boundingBox.min.y)/2,0);g.computeBoundingBox();geomCache.set(k,g);
  }
  const g=geomCache.get(k),b=g.boundingBox,m=new THREE.Mesh(g,material);
  m.scale.set(width/(b.max.x-b.min.x),height/(b.max.y-b.min.y),0.65);
  m.position.set(x,y,z);m.rotation.z=rotation;parent.add(m);return m;
}
function tube(parent,points,r=0.05,mat=bone){
  const curve=new THREE.CatmullRomCurve3(points.map(p=>new THREE.Vector3(...p)));
  const mesh=new THREE.Mesh(new THREE.TubeGeometry(curve,32,r,8,false),mat);parent.add(mesh);return mesh;
}
function ball(parent,p,r=0.13,mat=joint){const m=new THREE.Mesh(new THREE.SphereGeometry(r,12,8),mat);m.position.set(...p);parent.add(m);return m;}
function ring(parent,p,r=0.13){const m=new THREE.Mesh(new THREE.TorusGeometry(r,0.028,6,20),accent);m.position.set(...p);parent.add(m);return m;}
function part(name,center,start,offset,spin){
  const group=new THREE.Group();root.add(group);group.position.set(...center);
  const obj={name,group,center:new THREE.Vector3(...center),start,offset:new THREE.Vector3(...offset),spin,materials:[]};parts.push(obj);return group;
}
function capsule(parent,a,b,r=0.09,mat=bone){
  const av=new THREE.Vector3(...a),bv=new THREE.Vector3(...b),d=bv.clone().sub(av);
  const m=new THREE.Mesh(new THREE.CapsuleGeometry(r,Math.max(0.01,d.length()-2*r),4,8),mat);
  m.position.copy(av.add(bv).multiplyScalar(.5));m.quaternion.setFromUnitVectors(new THREE.Vector3(0,1,0),d.normalize());parent.add(m);return m;
}
function labelAlong(parent,word,a,b,width=0.2,z=0.15){
  const dx=b[0]-a[0],dy=b[1]-a[1],angle=Math.atan2(dy,dx);
  text(parent,word,(a[0]+b[0])/2,(a[1]+b[1])/2,z,Math.hypot(dx,dy)*0.86,width,angle);
}
function arcText(parent,word,cx,cy,rx,ry,start,end,height,z=0.24){
  [...word].forEach((ch,i)=>{if(ch===' ')return;const a=start+(end-start)*(i+.5)/word.length;
    const dx=-rx*Math.sin(a)*(end-start),dy=ry*Math.cos(a)*(end-start);
    text(parent,ch,cx+rx*Math.cos(a),cy+ry*Math.sin(a),z,height*.72,height,Math.atan2(dy,dx));
  });
}

// Spine: individual vertebrae arriving in sequence, forming the central frame.
for(let i=0;i<16;i++){
  const y=-1.05+i*.235,g=part('spine',[0,y,-.18],.3+i*.055,[Math.sin(i*2)*3,1.5+i*.15,-2.5],i%2?1.2:-1.2);
  capsule(g,[-.13,0,0],[.13,0,0],.095);capsule(g,[-.22,0,-.02],[.22,0,-.02],.045,joint);
  if(y<.3)ring(g,[0,0,.105],.065);
}
const neck=part('neck',[0,3.02,0],1.05,[0,4,-2],.4);
text(neck,'IDENTIFICATION',0,.1,.16,.85,.105);text(neck,'ASSUMPTIONS',0,-.055,.16,.75,.105);

// Pelvis and legs follow the anatomy and vocabulary of the printed cover.
const pelvis=part('pelvis',[0,-1.4,0],2,[0,-4,2],-.65);
for(const s of [-1,1]){
  tube(pelvis,[[0,.12,.05],[s*.64,.5,0],[s*.98,.37,0],[s*1.0,.0,.12],[s*.62,-.48,.18],[s*.24,-.59,.12],[0,-.05,.07]],.085);
  tube(pelvis,[[s*.2,-.1,.07],[s*.46,-.27,.15],[s*.42,-.5,.18]],.055);
}
text(pelvis,'REGRESSION DISCONTINUITY',0,.22,.22,1.82,.16);
text(pelvis,'DESIGN',0,-.02,.24,.67,.18);
text(pelvis,'CUTOFF',-.56,-.25,.25,.38,.11);text(pelvis,'LOCAL ATE',.53,-.25,.25,.43,.11);
for(const s of [-1,1]){
  const thigh=part('legs',[s*.66,-2.85,0],2.7+(s+1)*.13,[s*4,-2,-1],s*1.5);
  capsule(thigh,[s*.05,.88,0],[-s*.09,-.95,0],.13);
  ball(thigh,[s*.05,.88,0],.18);ball(thigh,[-s*.09,-.95,0],.16);ring(thigh,[-s*.09,-.95,.12],.14);
  labelAlong(thigh,s<0?'SELECTION BIAS':'DONOR POOL',[-s*.09,-.78],[s*.05,.7],.21,.16);
  const shin=part('legs',[s*.58,-4.58,0],3.15+(s+1)*.13,[s*3,-3,2],-s*1.3);
  capsule(shin,[-.08,.65,0],[-.07,-.67,0],.075);capsule(shin,[.09,.65,-.06],[.1,-.67,-.02],.055);
  labelAlong(shin,s<0?'ATE / ATT':'SPILLOVERS',[0,-.54],[0,.54],.17,.11);ring(shin,[0,.76,.04],.14);
  const foot=part('feet',[s*.58,-5.43,.18],3.8+(s+1)*.1,[s*3,-2,3],s*.8);
  for(let j=0;j<5;j++)capsule(foot,[(j-2)*.075,.09,-.1],[(j-2)*.12+s*.07,-.1,.4-(j*.035)],.045);
  text(foot,s<0?'ROBUSTNESS':'VALIDITY',0,-.1,.47,.54,.1);
}

// Rib units are separate curved 3D assemblies with raised method names.
const ribNames=[['RANDOMIZED',1.55],['CONTROLLED TRIALS',2.05],['DIFFERENCE-IN-DIFFERENCES',2.36],['PARALLEL TRENDS',1.95],['SYNTHETIC CONTROL METHOD',2.4],['FORECASTING COUNTERFACTUALS',2.3]];
ribNames.forEach(([word,width],i)=>{
  const y=2.48-i*.385,span=1.01+Math.sin(i/5*Math.PI)*.23;
  const g=part('ribs',[0,y,0],4.55+i*.3,[(i%2?-1:1)*4,1.1,-3],(i%2?-1:1)*1.5);
  for(const s of [-1,1])tube(g,[[0,.12,-.27],[s*.6,.17,-.28],[s*span,.04,-.07],[s*(span+.08),-.11,.31],[s*.7,-.17,.56],[s*.07,-.1,.62]],.055);
  text(g,word,0,-.095,.695,width,.16);
});
for(const s of [-1,1]){
  const shoulder=part('shoulders',[s*1.05,2.86,0],6.45,[s*5,2,1],s*1.1);
  tube(shoulder,[[-s*.85,.02,0],[-s*.4,.06,.12],[s*.22,-.05,.1],[s*.48,-.15,0]],.085);ball(shoulder,[s*.49,-.2,0],.22);ring(shoulder,[s*.49,-.2,.15],.19);
  text(shoulder,s<0?'TREATMENT EFFECT':'CAUSAL DESIGN',0,.12,.2,.97,.115);
  const upper=part('arms',[s*1.76,1.34,0],7+(s+1)*.27,[s*5,1,2],-s*1.2);
  capsule(upper,[-s*.18,1.03,0],[s*.22,-1.03,0],.125);ball(upper,[s*.22,-1.03,0],.18);ring(upper,[s*.22,-1.03,.13],.14);
  labelAlong(upper,s<0?'INSTRUMENTAL VARIABLES':'MATCHING',[s*.2,-.92],[-s*.18,.93],.195,.15);
  const fore=part('arms',[s*2.14,-.56,0],7.75+(s+1)*.27,[s*5,-1,-2],s*1.4);
  capsule(fore,[-.07,.76,0],[s*.18-.07,-.8,0],.065);capsule(fore,[.08,.76,-.02],[s*.18+.08,-.8,-.02],.065);
  labelAlong(fore,s<0?'RELEVANCE':'COVARIATE BALANCE',[s*.15,-.64],[0,.63],.13,.13);
  const hand=part('hands',[s*2.42,-1.73,.03],8.65+(s+1)*.2,[s*4,-1.5,3],s*1.2);
  for(let j=0;j<5;j++){
    const x=(j-2)*.085,endX=x+(j-2)*.085;
    capsule(hand,[x,.24,0],[x,-.1,.02],.038);
    capsule(hand,[x,-.1,.02],[endX,-.4-Math.sin(j/4*Math.PI)*.23,.05],.03);
    capsule(hand,[endX,-.4-Math.sin(j/4*Math.PI)*.23,.05],[endX+(j-2)*.025,-.54-Math.sin(j/4*Math.PI)*.23,.12],.028);
  }
  text(hand,s<0?'LATE':'GPS',0,.04,.11,.27,.095);
}

// Skull: sculpted cranium with eye sockets, nasal opening and articulated jaw.
const skull=part('skull',[0,4.25,0],10,[0,4.2,-4],-1.1);
const outline=new THREE.Shape();
outline.moveTo(-.53,-.46);outline.bezierCurveTo(-.9,-.3,-.91,.12,-.82,.47);outline.bezierCurveTo(-.82,.96,-.47,1.19,0,1.19);outline.bezierCurveTo(.47,1.19,.82,.96,.82,.47);outline.bezierCurveTo(.91,.12,.9,-.3,.53,-.46);outline.lineTo(.37,-.62);outline.lineTo(-.37,-.62);outline.closePath();
for(const s of [-1,1]){const hole=new THREE.Path();hole.absellipse(s*.35,.05,.255,.235,0,Math.PI*2,true);outline.holes.push(hole);}
const nose=new THREE.Path();nose.moveTo(0,-.12);nose.lineTo(.12,-.36);nose.quadraticCurveTo(0,-.31,-.12,-.36);nose.closePath();outline.holes.push(nose);
const skullGeo=new THREE.ExtrudeGeometry(outline,{depth:.34,bevelEnabled:true,bevelThickness:.045,bevelSize:.035,bevelSegments:2,curveSegments:18});
const cranium=new THREE.Mesh(skullGeo,bone);skull.add(cranium);
arcText(skull,'COUNTERFACTUAL',0,.56,.75,.47,Math.PI*.94,Math.PI*.06,.18,.405);
text(skull,'POTENTIAL',0,.55,.414,.97,.155);text(skull,'OUTCOMES',0,.34,.414,.91,.155);
for(const s of [-1,1]){
  const eye=new THREE.Mesh(new THREE.TorusGeometry(.16,.022,6,24),accent);eye.position.set(s*.35,.045,.04);skull.add(eye);
  text(skull,s<0?'RUBIN':'MODEL',s*.35,.045,.1,.32,.09,0,ink);
  text(skull,s<0?'ATE':'ATT',s*.42,-.32,.41,.21,.105);
}
for(let i=0;i<8;i++){
 const tooth=new THREE.Mesh(new THREE.BoxGeometry(.074,.095,.065),bone);tooth.position.set((i-3.5)*.09,-.56,.4);skull.add(tooth);
}
const jaw=part('jaw',[0,3.64,.08],11,[0,-2,4],.65);
tube(jaw,[[-.62,.3,0],[-.57,-.1,.12],[-.35,-.3,.21],[0,-.37,.25],[.35,-.3,.21],[.57,-.1,.12],[.62,.3,0]],.085);
arcText(jaw,'CAUSAL INFERENCE',0,.04,.61,.29,Math.PI*1.08,Math.PI*1.92,.13,.36);

// A low-profile launch platform and graphic speed lines suggest a mecha hangar.
const stage=new THREE.Mesh(new THREE.CylinderGeometry(2.6,2.85,.14,64),new THREE.MeshStandardMaterial({color:0xe1e8e9,metalness:.65,roughness:.4}));stage.position.set(0,-5.69,0);root.add(stage);
for(const r of [2.1,2.55]){
 const m=new THREE.Mesh(new THREE.TorusGeometry(r,.02,6,80),r===2.1?accent:gold);m.rotation.x=Math.PI/2;m.position.y=-5.61;root.add(m);
}
const speed=new THREE.Group();root.add(speed);
for(let i=0;i<26;i++){
 const a=i*2.39996,r=3.3+(i%4)*.6,y=Math.sin(a)*5;
 const geo=new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(Math.cos(a)*r,y,-1),new THREE.Vector3(Math.cos(a)*(r+.55),y+Math.sin(a)*.6,-1)]);
 speed.add(new THREE.Line(geo,new THREE.LineBasicMaterial({color:0xc5d3d5,transparent:true,opacity:.4})));
}
const phases=[
 [0,'01 / CORE','IDENTIFICATION','Assumptions form the backbone.'],
 [2,'02 / FOUNDATION','REGRESSION<br>DISCONTINUITY DESIGN','Selection bias / donor pool / ATE / ATT'],
 [4.5,'03 / THORAX','RANDOMIZED<br>CONTROLLED TRIALS','Difference-in-differences'],
 [5.8,'03 / THORAX','SYNTHETIC<br>CONTROL METHOD','Forecasting counterfactuals'],
 [7,'04 / ARMS','INSTRUMENTAL<br>VARIABLES','Relevance / LATE'],
 [8.1,'04 / ARMS','MATCHING','Covariate balance'],
 [10,'05 / CRANIUM','COUNTERFACTUAL','Potential outcomes / Rubin'],
 [11.7,'06 / ASSEMBLED','CAUSAL INFERENCE','Many methods. One counterfactual question.'],
 [15,'THE BACKBONE','BACK TO BASICS','The Backbone of Impact Evaluation Methods']
];
for(const p of parts){
 p.group.traverse(o=>{if(o.isMesh&&o.material===ink){o.material=ink.clone();p.materials.push(o.material);}});
}
const clamp=x=>Math.max(0,Math.min(1,x));
const ease=x=>{x=clamp(x);return 1-Math.pow(1-x,3);};
window.renderAt=function(t){
  for(const p of parts){
    const q=ease((t-p.start)/1.25),v=1-q;
    p.group.visible=t>p.start-.12;
    if(p.name==='spine'&&p.center.y>.3&&t>6.9)p.group.visible=false;
    p.group.position.copy(p.center).addScaledVector(p.offset,v);
    p.group.rotation.set(v*p.spin*.3,v*p.spin,v*p.spin*.37);
    // A short, diminishing mechanical settle after each component docks.
    const land=t-p.start-1.25;
    if(land>0&&land<.45)p.group.position.y+=Math.sin(land*24)*.025*Math.exp(-land*7);
    const highlight=Math.exp(-Math.pow((land-.4)/.75,2));
    for(const m of p.materials)m.color.set(0x121b23).lerp(new THREE.Color(0x148589),highlight*.95);
  }
  root.rotation.y=t<12?.14+Math.sin(t*.48)*.34:t<16?.3*(1-ease((t-12)/4)):Math.sin((t-16)*.5)*.045;
  const pulse=Math.exp(-Math.pow((t-12.35)/.35,2));
  accent.emissiveIntensity=.22+pulse*2.2;
  speed.visible=t<12.6;
  speed.rotation.z=.015*Math.sin(t);
  const camZ=21.9;
  camera.position.set(0,1.25,camZ);camera.lookAt(-2.05,-.13,0);
  camera.updateProjectionMatrix();
  const phase=phases.filter(p=>t>=p[0]).at(-1);
  document.querySelector('#count').textContent=phase[1];
  document.querySelector('#word').innerHTML=phase[2];
  document.querySelector('#sub').textContent=phase[3];
  document.querySelector('#progress').style.width=`${Math.min(100,t/20*100)}%`;
  document.querySelector('#flash').style.opacity=pulse*.055;
  renderer.render(scene,camera);
  return {triangles:renderer.info.render.triangles,parts:parts.filter(p=>p.group.visible).length};
};
window.captureScale=s=>{renderer.setPixelRatio(s);document.body.style.zoom=s;};
window.renderAt(0);window.ready=true;
