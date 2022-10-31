function Star() {
    const farsize = 0;
    const closesize = 2;
    const vz = 20;
    const depth = 2000;

    [this.x, this.y, this.z, this.pz] = initStar();

    function initStar() {
        x = random(-width/2,width/2);
        y = random(-height/2,height/2);
        z = random(depth);
        pz = z;
        return [x, y, z, pz];
    }

    this.update = function() {
        this.z -= vz;

        if (this.z<1) {
            [this.x, this.y, this.z, this.pz] = initStar() 
        }
    }

    this.show = function() {
        fill(255);
        noStroke();

        this.sx = map(this.x/this.z, 0, 1, 0, width);
        this.sy = map(this.y/this.z, 0, 1, 0, height);

        this.r = map(this.z, 0, depth, closesize, farsize);
        ellipse(this.sx, this.sy, this.r, this.r);

        this.px = map(this.x/this.pz, 0, 1, 0, width);
        this.py = map(this.y/this.pz, 0, 1, 0, height);

        stroke(255);
        line(this.px, this.py, this.sx, this.sy);
        this.pz = this.z;
    }
}