const width = window.innerWidth;
const height = window.innerHeight;

// Court dimensions
const realWidth = 6.1;
const realSinglesWidth = 5.18;
const realLength = 13.41;
const realDoubleFaultLength = 0.76;
const realFaultLength = 1.98;
const realProportion = realLength/realWidth;
const courtWidth = 9.5/10*Math.min(width, height/realProportion);
const courtLength = courtWidth*realProportion;
const paddingX = (width-courtWidth)/2;
const paddingY = (height-courtLength)/2;
const singlesSide = courtWidth/2*(1-realSinglesWidth/realWidth);
const doublesFaultLine = realDoubleFaultLength*courtLength/realLength;
const faultLine = realFaultLength*courtLength/realLength;

// Interface & sound
let Vslider;
let volumeSlider;
let soundAttack;
let soundDefense;

let reactionTime = 3000; let startTime = 0;

// Simulation
const minAngle = Math.PI/180*4;
const maxAngle = Math.PI/2-Math.atan((0.3*courtLength+faultLine)/(courtWidth/2-singlesSide))-minAngle;

let x = 0; let y = 0; let vx = 0; let vy = 0; let v = 0;
let ready = true; let returnable = false;
let flip = 1;

// Physics
const physicsTimeRatio = 10;
let timeStep = 0; // 500 per second
const cD = 0.55; const density = 1.225; const skirtArea = 0.0034212; const mass = 0.005;

function setup() {
  createCanvas(width, height);

  Vslider = createSlider(0,2000,150);
  Vslider.position(width-180,40);
  volumeSlider = createSlider(0, 10, 3);
  volumeSlider.position(width-180,90);
  volumeSlider.input(updateVolume);


  soundAttack = loadSound('soundAttack.mp3');
  soundDefense = loadSound('soundDefense.mp3');
  soundAttack.setVolume(0.3);
  soundDefense.setVolume(0.3);

  setTimeout(startShot, 3000);
  frameRate(144);
}

function updateVolume() {
  soundAttack.setVolume(volumeSlider.value()/10);
  soundDefense.setVolume(volumeSlider.value()/10);
}

function draw() {
  background(52, 152, 80);
  
  fill(255); stroke(255); strokeWeight(2);
  // Doubles area
  line(paddingX, paddingY, paddingX+courtWidth, paddingY);
  line(paddingX, paddingY, paddingX, paddingY+courtLength);
  line(paddingX, paddingY+courtLength, paddingX+courtWidth, paddingY+courtLength);
  line(paddingX+courtWidth, paddingY, paddingX+courtWidth, paddingY+courtLength);
  // Net
  line(paddingX, paddingY+courtLength/2, paddingX+courtWidth, paddingY+courtLength/2);
  // Singles lines
  line(paddingX+singlesSide, paddingY, paddingX+singlesSide, paddingY+courtLength);
  line(paddingX+courtWidth-singlesSide, paddingY, paddingX+courtWidth-singlesSide, paddingY+courtLength);
  // Double serve fault line
  strokeWeight(1);
  line(paddingX, paddingY+doublesFaultLine, paddingX+courtWidth, paddingY+doublesFaultLine);
  line(paddingX, paddingY+courtLength-doublesFaultLine, paddingX+courtWidth, paddingY+courtLength-doublesFaultLine);
  // Serve fault lines
  line(paddingX, paddingY+courtLength/2-faultLine, paddingX+courtWidth, paddingY+courtLength/2-faultLine);
  line(paddingX, paddingY+courtLength/2+faultLine, paddingX+courtWidth, paddingY+courtLength/2+faultLine);
  line(paddingX+courtWidth/2, paddingY, paddingX+courtWidth/2, paddingY+courtLength/2-faultLine);
  line(paddingX+courtWidth/2, paddingY+courtLength/2+faultLine, paddingX+courtWidth/2, paddingY+courtLength);


  fill(173, 22, 77); stroke(173, 22, 77);
  circle(paddingX+courtWidth/2, paddingY+courtLength/5, 40);
  fill(0, 62, 51); stroke(0, 62, 51);
  circle(paddingX+courtWidth/2, height-paddingY-courtLength/5, 40);
  fill(255); stroke(255);
  circle(x, y, 12);

  textSize(18); text(`V0 [km/h]: ${Vslider.value()}`, width-170, 34);
  text("Volume", width-146, 86);

  text(`Framerate: ${Math.trunc(frameRate())} fps`, 50, 50);
  text(`Fastest reaction: ${Math.trunc(reactionTime)} ms`, 50, 80);
  if (ready === false) {
    timeStep = deltaTime/physicsTimeRatio;
    for (let i=0; i<physicsTimeRatio; i++) {
      x += vx*courtLength/realLength*timeStep/1000*flip; y += vy*courtLength/realLength*timeStep/1000*flip;
      let angle = Math.atan(vx/vy);
      v -= 0.5*cD*density*skirtArea*v**2*timeStep/1000/mass;
      v = Math.max(0, v);
      vx = v*Math.sin(angle);
      vy = v*Math.cos(angle);
      
      if ((Math.abs(y-height/2)>courtLength/2) || Math.abs(x-paddingX-courtWidth/2)>courtWidth/2-singlesSide) {
        vx = 0; vy = 0;

        ready = true;
        returnable = false;

        setTimeout(startShot, 200);
        break;
      }
    }
  }
}

function hitShuttle() {

  v = Vslider.value()/3.6;
  let angle = random(minAngle, maxAngle);
  if (random() < 0.5) { angle *= -1; }
  vx = v*Math.sin(angle);
  vy = v*Math.cos(angle);
  flip = 1;

  ready = false;
  returnable = true;
  startTime = millis();

}

function returnShuttle(winlose) {
  if (winlose === true) {
    if (millis() - startTime - deltaTime < reactionTime) {
      reactionTime = millis() - startTime - deltaTime;
    }
    v = random(250, v);
    soundDefense.play()
    flip = -1;
  } else {
    reactionTime = 3000;
  }
  returnable = false;
}

function startShot() {
  x = paddingX+courtWidth/2;
  y = paddingY+courtLength/5;

  soundAttack.play();
  setTimeout(hitShuttle, 800);
}

function keyPressed() {
  if (returnable === true) {
    if (keyCode == 37) {
      if (x < paddingX+courtWidth/2) {
        returnShuttle(true);
      } else { returnShuttle(false); }
    } else if (keyCode == 39) {
      if (x < paddingX+courtWidth/2) {
        returnShuttle(false);
      } else { returnShuttle(true); }
    }   
  }
}