let font;

let angle = 0;

function preload() {
    font = loadFont('OpenSans-Regular.ttf');
}

function setup() {
    createCanvas(windowWidth, windowHeight, WEBGL);
    textFont(font);
    textSize(25);
}

function draw() {
    background(51);
    
    fill(255)
    text(round(1000/deltaTime), -width/2, -height/2+25);
    
    directionalLight(237,213,158, -sqrt(2), sqrt(2), 0);
    //ambientLight(20);

    push();
    noStroke();
    rotateX(1+angle);
    rotateY(0.2+angle*2);
    //rotateZ(angle*20);
    ambientMaterial(255);
    specularMaterial(0, 0, 255);
    box(400);
    pop();
    
    translate(0,350);
    stroke(0);
    rotateX(HALF_PI);
    specularMaterial(255);
    plane(1200,1200);

    angle += 0.004;
}