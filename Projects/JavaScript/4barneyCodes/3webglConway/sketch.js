let scale;
let noiseScale;

let sliderScale;
let textsliderScale;
let sliderNoiseScale;
let textsliderNoiseScale;

let scaleXY;
let randomTex;
let golShader;
let prevFrame;
let hold;

function preload() {
    golShader = loadShader('gol.vert', 'gol.frag');
}

function setup() {
    createCanvas(windowWidth, windowHeight, WEBGL);
    background(0);
    stroke(255);

    // UI
    let button = createButton('Reseed');
    button.position(56, 24, 100);
    button.mousePressed(() => {
        newRandomTex(randomTex);
        golShader.setUniform('tex', randomTex);
        hold = true;
    });

    sliderScale = createSlider(4, 1000, 400, 1);
    sliderScale.position(20,64);
    textsliderScale = createP();
    textsliderScale.position(20,32);
    textsliderScale.style('color', 'white')
    textsliderScale.html(`Scale: ${sliderScale.value()}`);
    sliderScale.input(() => {textsliderScale.html(`Scale: ${sliderScale.value()}`)})
    sliderScale.changed(initialize);

    sliderNoiseScale = createSlider(0.001, 10, 1, 0.1);
    sliderNoiseScale.position(20,96);
    textsliderNoiseScale = createP();
    textsliderNoiseScale.position(20,64);
    textsliderNoiseScale.style('color', 'white')
    textsliderNoiseScale.html(`Noise scale: ${sliderNoiseScale.value()}`);
    sliderNoiseScale.input(() => {textsliderNoiseScale.html(`Noise scale: ${sliderNoiseScale.value()}`)})
    sliderNoiseScale.changed(initialize);

    initialize();
}

function initialize() {
    scale = sliderScale.value();
    noiseScale = sliderNoiseScale.value();

    scaleXY = [scale, round(scale*height/width)];

    prevFrame = createGraphics(width, height);
    prevFrame.noSmooth();
    shader(golShader);
    golShader.setUniform("scaleXY", scaleXY);

    randomTex = createImage(scaleXY[0], scaleXY[1]);
    newRandomTex(randomTex);
    golShader.setUniform('tex', randomTex);
    hold = true;
}

function draw() {
    if(mouseIsPressed) {
        for (let i=0; i<round(width/scaleXY[0]); i++) {
            for (let j=0; j<round(height/scaleXY[1]); j++) {
                let x = round(mouseX)+i/2-width/2
                let y = round(mouseY)+j/2-height/2
                point(x, y);
            }
        }
    }
    else {
        if (hold === true) {
            hold = false;
        } else {
            // Copy the rendered image into our prevFrame image
            prevFrame.image(get(), 0, 0);  
            // Set the image of the previous frame into our shader
            golShader.setUniform('tex', prevFrame);
        }
        

        // Give the shader a surface to draw on
        rect(-width/2,-height/2,width,height);
    }
}

function newRandomTex(randomTex) {
    for (let i=0; i<randomTex.width; i++) {
        for (let j=0; j<randomTex.height; j++) {
            if (noise(i*noiseScale, j*noiseScale, millis()) > 0.5) {
                randomTex.set(i, j, 255);
            } else {
                randomTex.set(i, j, 0);
            }
        }
    }
    randomTex.updatePixels();
}