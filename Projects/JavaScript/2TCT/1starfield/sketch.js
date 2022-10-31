const width = window.innerWidth;
const height = window.innerHeight;

let stars = new Array(400);

function setup() {
  createCanvas(width, height);
  
  for (let i=0; i<stars.length; i++) {
    stars[i] = new Star();
  }
}

function draw() {
  background(0);

  translate(width/2, height/2)
  for (let i=0; i<stars.length; i++) {
    stars[i].update();
    stars[i].show();
  }
}