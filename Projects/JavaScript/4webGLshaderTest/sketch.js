let font;

let myShader;
let initialTexture;
let textureSize = 16;

function preload() {
    font = loadFont('OpenSans-Regular.ttf');

    myShader = loadShader('shader.vert', 'shader.frag');
}

function setup() {
    createCanvas(windowWidth, windowHeight, WEBGL);
    textFont(font);
    textSize(25);

    initialTexture = createGraphics(textureSize, textureSize);
    initialTexture.background(255,255,0);
    initialTexture.noStroke();
    for (let y = 0; y < textureSize; y++) {
        for (let x = 0; x < textureSize; x++) {
            if ((x + y) % 2 === 0) { 
                initialTexture.fill(255);
            } else {
                initialTexture.fill(0);
            };
            initialTexture.rect(x, y, 2, 2);
        };
    };
    myShader.setUniform('u_texture', initialTexture);
}

function draw() {
    fill(255);
    text(round(1000/deltaTime), 0,0);

    background(0,255,0);
    shader(myShader);
    rect(-width/2,-height/2,width,height);
}