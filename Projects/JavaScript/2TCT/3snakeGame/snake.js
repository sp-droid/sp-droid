function Snake() {
    this.x = 0;
    this.y = 0;
    this.vx = 1;
    this.vy = 0;
    this.total = 1;
    this.tail = [];
  
    this.eat = function(foodpos) {
        if (this.x===foodpos.x && this.y===foodpos.y) { 
            this.total++;
            return true 
        }
        else { return false }
    }

    this.dir = function(vx, vy) {
        this.vx = vx;
        this.vy = vy;
    }

    this.update = function() {
        if (this.total === this.tail.length) {
            for (let i=0; i<this.total-1; i++) {
                this.tail[i] = this.tail[i+1];
            }
        }
        this.tail[this.total-1] = createVector(this.x, this.y);

        for (let i=0; i<this.total-2; i++) {
            if (this.x===this.tail[i].x && this.y===this.tail[i].y) { 
                this.total = 1;
                this.tail = [];
            }
        }

        this.x = this.x + this.vx*scl;
        this.y = this.y + this.vy*scl;

        this.x = constrain(this.x, 0, floor(width/scl)*scl-scl)
        this.y = constrain(this.y, 0, floor(height/scl)*scl-scl)
    }
  
    this.show = function() {
        fill(255);
        for (let i=0; i<this.tail.length; i++) {
            rect(this.tail[i].x, this.tail[i].y, scl, scl);
        }
    }
  }