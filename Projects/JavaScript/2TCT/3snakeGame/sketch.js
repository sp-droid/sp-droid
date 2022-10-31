const width = window.innerWidth;
const height = window.innerHeight;
const scl = 30;

let cols;
let rows;
let food;
let s;

function setup() {
  createCanvas(width, height);
  frameRate(10);

  cols = floor(width/scl);
  rows = floor(height/scl);

  s = new Snake();
  pickLoc();
}

function pickLoc() {
  food = createVector(floor(random(cols)), floor(random(rows)));
  food.mult(scl);
}

function draw() {
  background(51)
  s.update();
  s.show();

  if (s.eat(food)) { pickLoc() } 

  fill(255,0,100);
  rect(food.x, food.y, scl, scl)
}

function keyPressed() {
  if (keyCode == UP_ARROW) { s.dir(0,-1) }
  else if (keyCode == DOWN_ARROW) { s.dir(0,1) }
  else if (keyCode == RIGHT_ARROW) { s.dir(1,0) }
  else if (keyCode == LEFT_ARROW) { s.dir(-1,0) }
}