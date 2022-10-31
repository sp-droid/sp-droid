const width = window.innerWidth;
const height = window.innerHeight;

const g = 9.8/60;
const r1 = height/4;
const r2 = r1*0.4;
const m1 = 10;
const m2 = 10;

let theta1 = 3;
let theta2 = 0;
let omega1 = 0;
let omega2 = 0;
let alpha1;
let alpha2;
let x1;
let x2;
let y1;
let y2;

let e;
let originalE;
let damp;
let potential;
let kinetic;

let trail = new Array(1000);

function setup() {
  createCanvas(width, height);

  valueDisplayer = createP();
  valueDisplayer.position(20,2);
  valueDisplayer2 = createP();
  valueDisplayer2.position(20,22);
  valueDisplayer3 = createP();
  valueDisplayer3.position(20,42);

  for (let i=0; i<trail.length; i++) {
    trail[i] = createVector(0,0);
  }
  y1 = r1*cos(theta1);
  y2 = y1+r2*cos(theta2);
  originalE = m1*g*(r1-y1)+m2*g*(r1+r2-y2);
  e = originalE;
}

function draw() {
  background(255);
  translate(width/2, height/2);

  // Calculate angular acceleration
  alpha1 = (-g*(2*m1+m2)*sin(theta1)-m2*g*sin(theta1-2*theta2)-2*sin(theta1-theta2)*m2*(omega2**2*r2+omega1**2*r1*cos(theta1-theta2)))/r1/(2*m1+m2-m2*cos(2*theta1-2*theta2));
  alpha2 = (2*sin(theta1-theta2)*(omega1**2*r1*(m1+m2)+g*(m1+m2)*cos(theta1)+omega2**2*r2*m2*cos(theta1-theta2)))/r2/(2*m1+m2-m2*cos(2*theta1-2*theta2));

  // Dampening to counteract integration losses
  damp = originalE/e;

  // Integration
  omega1 = damp*(omega1+alpha1);
  omega2 = damp*(omega2+alpha2);

  theta1 += omega1;
  theta2 += omega2;

  // Polar to cartesian
  x1 = r1*sin(theta1);
  y1 = r1*cos(theta1);
  x2 = x1+r2*sin(theta2);
  y2 = y1+r2*cos(theta2);

  // Paint the trail
  for (let i=trail.length-1; i>-1; i--) {
    point(trail[i].x, trail[i].y);
    if (i<trail.length-1) {
      trail[i+1].x = trail[i].x;
      trail[i+1].y = trail[i].y;
    }
  }
  trail[0].x = x2;
  trail[0].y = y2;

  // Paint the pendulum
  stroke(0);
  strokeWeight(2);
  fill(0);
  line(0,0,x1, y1);
  ellipse(x1,y1,m1,m1);
  line(x1,y1,x2, y2);
  ellipse(x2,y2,m2,m2);

  // Write useful parameters
  potential = m1*g*(r1-y1)+m2*g*(r1+r2-y2);
  kinetic = (0.5*m1*(omega1*r1)**2+0.5*m2*((omega1*r1)**2+(omega2*r2)**2+2*r1*r2*omega1*omega2*cos(theta1-theta2)));
  e = potential+kinetic;

  valueDisplayer.html('Total energy: '+e);
  valueDisplayer2.html('Kinetic: '+kinetic);
  valueDisplayer3.html('Potential: '+potential);
}