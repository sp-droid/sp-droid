let rules = {
    "X": "F+[[X]-X]-F[-FX]+X",
    "F": "FF"
}

let word = "X";

let len = 0.5;

function setup() {
    createCanvas(windowWidth, windowHeight);

    // Gradient background
    c1 = color(249, 246, 247);
    c2 = color(218, 219, 186);
    
    for(let y=0; y<height; y++){
        n = map(y,0,height,0,1);
        let newc = lerpColor(c1,c2,n);
        stroke(newc);
        line(0,y,width, y);
    }
}

function draw() {
    // background(150)
}

function generate() {
    let next = "";

    for (let i=0; i<word.length; i++) {
        let c = word[i];

        if (c in rules) {
            next += rules[c];
        } else {
            next += c;
        }
    }
}