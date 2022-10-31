const width = window.innerWidth;
const height = window.innerHeight;

function setup() {
  createCanvas(width, height);
  textSize(width / 10);
}

function draw() {
  textAlign(CENTER);
  text('Hello world', width/2, height/2);

  if (mouseIsPressed) {
    fill(0);
  } else {
    fill(125);
  }
  ellipse(mouseX, mouseY, 80, 80);
}