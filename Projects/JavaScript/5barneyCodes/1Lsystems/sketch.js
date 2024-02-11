// Declaration
let treeDepth;
let rules;
let growthLength;
let growthAngle;
let drawRules;

let word;

let sliderDepth;
let textSliderDepth;
let sliderAngle;
let textSliderAngle;
let sliderLength;
let textSliderLength;

function setup() {
    // Initialization
    treeDepth = 3;
    rules = {
        "X": "F+[[X]-X]-F[-FX]+X",
        "F": "FF"
    }
    drawRules = {
        "F": (i) => {
            stroke(45, 18, 7);
            strokeWeight(6/(1+log(i+2)/10));
            line(0,0,0,-growthLength);
            translate(0, -growthLength);
        },
        "-": (i) => {
            rotate(growthAngle);
        },
        "+": (i) => {
            rotate(-growthAngle);
        },
        "[": (i) => {
            push();
        },
        "]": (i) => {
            noStroke();
            fill(47, 183, 35);
            ellipse(0,0,3,10)
            pop();
        },
    }

    // Setup
    createCanvas(windowWidth, windowHeight);
    noLoop();

    // Sliders
    sliderDepth = createSlider(2, 10, 7, 1);
    sliderDepth.position(20,36);
    sliderDepth.input(draw);
    textSliderDepth = createP();
    textSliderDepth.position(20,2);
    textSliderDepth.style('color', 'white')

    sliderAngle = createSlider(-90, 90, 25, 1);
    sliderAngle.position(20,72);
    sliderAngle.input(draw);
    textSliderAngle = createP();
    textSliderAngle.position(20,38);
    textSliderAngle.style('color', 'white')

    sliderLength = createSlider(0, 30, 2.7, 0.1);
    sliderLength.position(20,108);
    sliderLength.input(draw);
    textSliderLength = createP();
    textSliderLength.position(20,74);
    textSliderLength.style('color', 'white')

    draw();
}

function draw() {
    push();
    background(120);
    
    textSliderDepth.html(`L system depth: ${sliderDepth.value()}`);
    treeDepth = sliderDepth.value();
    textSliderAngle.html(`Turn angle: ${sliderAngle.value()}º`);
    growthAngle = sliderAngle.value()*PI/180;
    textSliderLength.html(`Step length: ${sliderLength.value()}`);
    growthLength = sliderLength.value();

    word = "X";
    for (let i=0; i<treeDepth; i++) {
        word = generate();
    }

    translate(100, height-100);
    rotate(growthAngle);
    for (i=0; i<word.length; i++) {
        let c = word[i];
        if (c in drawRules) {
            drawRules[c](i);
        }
    }
    pop();
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
    return next;
}