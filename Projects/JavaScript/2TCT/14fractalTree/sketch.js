const width = window.innerWidth;
const height = window.innerHeight;

let angle;
let ilen;
let slider;
let slider2;
let stoplen = 20;

function setup() {
  createCanvas(width, height);
  slider = createSlider(0, PI, 0.5, 0.1);
  slider.position(20,36);
  slider.input(sliderChange);
  valueDisplayer = createP();
  valueDisplayer.position(20,2);
  valueDisplayer.style('color', 'white')

  slider2 = createSlider(stoplen, height/2, height/3)
  slider2.position(200,36);
  slider2.input(sliderChange);
  valueDisplayer2 = createP();
  valueDisplayer2.position(200,2);
  valueDisplayer2.style('color', 'white')
}

function draw() {
  background(51);
  fill(255);
  valueDisplayer.html('The value is '+slider.value())
  valueDisplayer2.html('The value is '+slider2.value())

  translate(width/2, height)

  angle = slider.value()
  ilen = slider2.value()
  stroke(255);

  if (stoplen > 4) { stoplen = 0.9*stoplen }
  branch(ilen, stoplen);
}

function sliderChange() {
  stoplen = 100;
}

function branch(len, stoplen) {
  line(0, 0, 0, -len);

  translate(0,-len);

  if (len > stoplen) {
    push();
    rotate(angle);
    branch(len*0.67, stoplen);
    pop();

    push();
    rotate(-angle);
    branch(len*0.67, stoplen);
    pop();
  }
}