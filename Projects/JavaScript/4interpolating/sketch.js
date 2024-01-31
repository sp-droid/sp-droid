const width = window.innerWidth;
const height = window.innerHeight;

const squareX = 500;
const squareY = Math.round(squareX/width*height);
const squareWidth = width / squareX;
const squareHeight = height / squareY;
const noiseScale = 100;
const noiseLevel = 10;

let textValue = squareY;
let frameHistory = Array(35).fill(0);
let positionHistory = 0;
let frameNumber = 0;

let provinceMap;
let dimX;
let dimY;

function preload () {
    provinceMap = new arr3D();
}

function setup() {
    createCanvas(width, height);
    dimY = provinceMap.length;
    dimX = provinceMap[0].length;
    console.log(dimX, dimY)

    noStroke();
    textSize(30);
    frameRate(60);
}

function draw() {
    frameNumber += 1;
    // Nested loops to iterate through each row and column
    for (let i = 0; i < squareX; i++) {
        for (let j = 0; j < squareY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareHeight;

            x += squareWidth/2;
            y += squareHeight/2;

            //x += ((Math.min(1,noise(noiseScale*x*frameNumber)))*noiseLevel-0.5*noiseLevel);
            //y += ((Math.min(1,noise(noiseScale*y*frameNumber)))*noiseLevel-0.5*noiseLevel);

            // Normalized centers
            let centerx = x/width*dimX;
            let centery = y/height*dimY;

            centerx = clamp(centerx, 0, dimX-1);
            centery = clamp(centery, 0, dimY-1);

            // if (frameCount/2 % 2 === 0) {
            //     interpNearest(centerx, centery);
            // } else {
            //     interpMajority(centerx, centery);
            // }
            //interpNearest(centerx, centery); //1.751
            //interpBilinear(centerx, centery); //1.579
            interpMajority(centerx, centery); //1.565

            // Draw the square
            rect(x, y, squareWidth, squareHeight);
        };
    };
    if (frameNumber > 4 && frameNumber < 40) {
        frameHistory[positionHistory] = 1000/deltaTime;
        positionHistory += 1;
    } else if (frameNumber === 40) {
        noLoop();
    };
    const average = frameHistory.reduce((a, b) => a + b, 0)/positionHistory;
    fill(0);
    text([positionHistory,average], 20, 35);
}

function clamp(value, minValue, maxValue) {
    return Math.min(maxValue, Math.max(minValue,value));
}

function interpMajority(centerx, centery) {
    // No idea about this. I know what I want but not how can i get it
    const left = Math.floor(centerx);
    const right = Math.ceil(centerx);
    const up = Math.floor(centery);
    const bottom = Math.ceil(centery);

    const candidates = [
        provinceMap[up][left][1],
        provinceMap[up][right][1],
        provinceMap[bottom][left][1],
        provinceMap[bottom][right][1]
    ];

    const score = [
        (right-centerx)*(bottom-centery),
        (centerx-left)*(bottom-centery),
        (right-centerx)*(centery-up),
        (centerx-left)*(centery-up)
    ];
    const scores = Array(4).fill(0);

    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < 4; j++) {
            if (i === j) {
                scores[i] += score[i];
            } else {
                if (candidates[i]===candidates[j]) {
                    scores[j] += score[i];
                };
            };
        };
    };
    
    const maxIndex = scores.indexOf(Math.max(...scores));

    if (maxIndex===0) { fill(provinceMap[up][left]);
    } else if (maxIndex===1) { fill(provinceMap[up][right]);
    } else if (maxIndex===2) { fill(provinceMap[bottom][left]);
    } else { fill(provinceMap[bottom][right]) };
}

function interpBilinear(centerx, centery) {
    const left = Math.max(0,Math.floor(centerx));
    const right = Math.min(dimX-1,Math.ceil(centerx));
    const up = Math.max(0,Math.floor(centery));
    const bottom = Math.min(dimY-1,Math.ceil(centery));

    // Interpolate between lower X values
    const Rxleft = (right-centerx)/(right-left);
    const Rxright = (centerx-left)/(right-left);
    
    let R1 = [0,0,0];
    for (let i=0; i<3; i++) {
        R1[i] = Rxleft*provinceMap[bottom][left][i]+Rxright*provinceMap[bottom][right][i];
    } 
    
    let R2 = [0,0,0];
    for (let i=0; i<3; i++) {
        R2[i] = Rxleft*provinceMap[up][left][i]+Rxright*provinceMap[up][right][i];
    }
    let rgb = [0,0,0];
    const Rybottom = (up-centery)/(up-bottom);
    const Ryup = (centery-bottom)/(up-bottom);
    for (let i=0; i<3; i++) {
        rgb[i] = Rybottom*R1[i]+Ryup*R2[i];
    }
    fill(rgb);
}

function interpNearest(centerx, centery) {
    centerx = Math.floor(centerx);
    centery = Math.floor(centery);
    const rgb = provinceMap[centery][centerx];
    fill(rgb);
}

function interpDefault(centerx, centery) {
    const i = Math.floor(centerx);
    const j = Math.floor(centery);
    if ((i + j) % 2 === 0) {
        fill(0,0,0); // Even squares are black
    } else {
        fill(255,255,255); // Odd squares are white
    }
}