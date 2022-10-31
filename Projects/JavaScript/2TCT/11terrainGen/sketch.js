const width = window.innerWidth;
const height = window.innerHeight;
const scl = 20;

const xychange = 0.2;
const tchange = 0.1;
const zscale = 180;

let cols;
let rows;
let terrain = [];
let toffset = 0;
let zcolor;

function setup() {
  createCanvas(width, height, WEBGL);

  cols = floor(width*2/scl);
  rows = floor(height*1.4/scl);

  let yoffset = 0;
  for (let y=0; y<rows; y++) {
    terrain[y] = Array(cols);
    let xoffset = 0;
    for (let x=0; x<cols; x++) {
      terrain[y][x] = map(noise(xoffset,yoffset), 0, 1, 0, zscale);
      xoffset += xychange;
    }
    yoffset += xychange;
  }
}

function draw() {
  background(0);
  //noFill();
  
  rotateX(PI/3);
  translate(-cols*scl/2, -rows*scl/2);
  
  for (let y=0; y<rows-1; y++) {
    beginShape(TRIANGLE_STRIP);
    for (let x=0; x<cols; x++) {
      zcolor = map(terrain[y][x], 0, zscale, 0, 255);
      fill(zcolor, 150);
      stroke(zcolor, 150);
      vertex(x*scl, y*scl, terrain[y][x]);
      vertex(x*scl, (y+1)*scl, terrain[y+1][x]);
    }
    endShape();
  }

  for (let y=rows-1; y>0; y--) {
    terrain[y] = terrain[y-1].slice();
  }
  let xoffset = tchange;
  toffset += tchange;
  for (let x=0; x<cols; x++) {
    terrain[0][x] = map(noise(xoffset,toffset), 0, 1, 0, zscale);
    xoffset += xychange;
  }
}