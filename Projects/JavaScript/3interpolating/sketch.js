let provinceMap;
let dimX;
let dimY;
let newDimX;
let newDimY;

let width;
let height;

function preload () {
    provinceMap = new arr3D();
}

function setup() {
    
    dimY = provinceMap.length;
    dimX = provinceMap[0].length;

    createCanvas(window.innerWidth, window.innerHeight);

    noStroke();
    frameRate(1);
    textSize(30);
}

let i = 0;
const iters = [1,2,4,8,12,16];
function draw() {
    const iter = iters[i]
    i++;
    clear();
    const squareWidth = 8/iter;

    newDimX = dimX*iter;
    newDimY = dimY*iter;

    width = newDimX*squareWidth;
    height = newDimY*squareWidth;

    // Nested loops to iterate through each row and column
    for (let i = 0; i < newDimX; i++) {
        for (let j = 0; j < newDimY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareWidth;

            // Normalized centers. From 0 to (N-1)
            let centerx = (x+squareWidth/2)*newDimX/width;
            let centery = (y+squareWidth/2)*newDimY/height;

            interpDefault(centerx, centery);

            // Draw the square
            rect(x, y, squareWidth, squareWidth);
        };
    };

    for (let i = 0; i < newDimX; i++) {
        for (let j = 0; j < newDimY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareWidth;

            // Normalized centers. From 0 to (N-1)
            let centerx = (x+squareWidth/2)*dimX/width;
            let centery = (y+squareWidth/2)*dimY/height;

            interpNearest(centerx, centery); //1.751

            // Draw the square
            rect(x+width+1, y, squareWidth, squareWidth);
        };
    };
    for (let i = 0; i < newDimX; i++) {
        for (let j = 0; j < newDimY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareWidth;

            // Normalized centers. From 0 to (N-1)
            let centerx = (x+squareWidth/2)*dimX/width;
            let centery = (y+squareWidth/2)*dimY/height;

            interpBilinear(centerx, centery); //1.579

            // Draw the square
            rect(x, y+height+2, squareWidth, squareWidth);
        };
    };
    for (let i = 0; i < newDimX; i++) {
        for (let j = 0; j < newDimY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareWidth;

            // Normalized centers. From 0 to (N-1)
            let centerx = (x+squareWidth/2)*dimX/width;
            let centery = (y+squareWidth/2)*dimY/height;

            interpMajority(centerx, centery); //1.565

            // Draw the square
            rect(x+width+1, y+height+2, squareWidth, squareWidth);
        };
    };
    for (let i = 0; i < newDimX; i++) {
        for (let j = 0; j < newDimY; j++) {
            // Calculate the top left position of each square
            let x = i * squareWidth;
            let y = j * squareWidth;

            // Normalized centers. From 0 to (N-1)
            let centerx = (x+squareWidth/2)*dimX/width;
            let centery = (y+squareWidth/2)*dimY/height;
            const noise1 = (noise(centerx*0.1,centery*0.1)-0.5)*3.5; // First layer of noise, targets big features. The other ones target progressively smaller ones
            const noise2 = (noise(centerx*1,centery*1)-0.5)*1.5;
            const noise3 = (noise(centerx*10,centery*10)-0.5)*0.5;
            // if (i==0) {console.log((noise(y*noiseScale)-0.5)*noiseFactor)}
            const noiseTotal = noise1+noise2+noise3;
            centerx += noiseTotal; centery += noiseTotal;
            centerx = clamp(centerx, 0, dimX-1);
            centery = clamp(centery, 0, dimY-1);

            interpMajority(centerx, centery); //1.565

            // Draw the square
            rect(x*2+2*width+2, y*2+1, squareWidth*2, squareWidth*2);
        };
    };

    fill(255);
    text("Pixels: 64²x"+iter+"²",2.7*width, 0.1*height);
    text(1,0.47*width, 0.98*height);
    text(2,1.47*width, 0.98*height);
    text(3,0.47*width, 1.08*height);
    text(4,1.47*width, 1.08*height);
    text(5,2.04*width, 1.03*height);
    if (iter == iters[iters.length-1]) {
        noLoop();
    }
}

function clamp(value, minValue, maxValue) {
    return Math.min(maxValue, Math.max(minValue,value));
}

// CAREFUL. In reality you would use the ID of the province, not the green channel. Be sure the image has colors with unique green channel values
function interpMajority(centerx, centery) {
    const left = Math.max(0,Math.floor(centerx));
    const right = Math.min(dimX-1,Math.ceil(centerx));
    const up = Math.max(0,Math.floor(centery));
    const bottom = Math.min(dimY-1,Math.ceil(centery));

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