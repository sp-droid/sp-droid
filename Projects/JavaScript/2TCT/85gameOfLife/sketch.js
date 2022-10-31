const width = window.innerWidth;
const height = window.innerHeight;
const scl = 10;

let cols;
let rows;
let x;
let y;

let gen = [];
let nextGen = [];

let state;
let sum;

function setup() {
  createCanvas(width, height);
  
  cols = floor(width/scl);
  rows = floor(height/scl);
  frameRate = 1;
  for (let i=0; i<cols; i++) { 
    gen[i] = new Array(rows);
    nextGen[i] = new Array(rows);
    for (let j=0; j<rows; j++) {
      gen[i][j] = floor(random(2));
    }
  }


}

function draw() {
  background(0);
  
  for (let i=0; i<cols; i++) {
    x = i * scl;
    for (let j=0; j<rows; j++) {
      y = j * scl;
      state = gen[i][j];

      sum = countNeighbors(gen, i, j);
      if (state===1) {
        fill(0)
        if (sum<2 || sum>3) {
          state = 0;
        }
      }
      else {
        fill(255)
        if (sum===3) {
          state = 1;
        }
      }
      rect(x, y, scl, scl);
      nextGen[i][j] = state;
    } 
  }
  gen = structuredClone(nextGen);
}

function countNeighbors(arr, x, y) {
  sum = 0;
  for (let i=-1; i<2; i++) { 
    for (let j=-1; j<2; j++) {
      sum += arr[(x+i+cols) % cols][(y+j+rows) % rows];
    }
  }
  sum -= arr[x][y];
  return sum
}