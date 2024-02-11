let nParticles;
let noiseScale;
let speed;

let particles = []

function setup() {
    nParticles = 3000;
    noiseScale = 0.002;
    speed = 1.8;

    createCanvas(windowWidth, windowHeight);

    for (let i=0; i<nParticles; i++) {
        particles.push(createVector(random(width), random(height)))
    }
    stroke(255);
    strokeWeight(2);
}

function mouseReleased() {
    noiseSeed(millis())
}

function draw() {
    // This trick is super cool. Each draw is not a clean slate, but one on top of another, so
    // if we use the second value to change the alpha it let's you see previous draws = trails
    background(0, 10);
    for (let i=0; i<nParticles; i++) {
        let p = particles[i];
        point(p.x, p.y);
        let n = (noise(p.x*noiseScale, p.y*noiseScale)-0.5)*2;
        let angle = TAU * n;
        p.x += speed*cos(angle);
        p.y += speed*sin(angle);
        if (!onScreen(p)) {
            p.x = random(width);
            p.y = random(height);
        }
    }
}

function onScreen(vector) {
    return vector.x >= 0 && vector.y >= 0 && vector.x < width && vector.y < height;
}