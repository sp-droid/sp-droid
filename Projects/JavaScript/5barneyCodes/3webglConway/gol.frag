precision mediump float;

varying vec2 vTexCoord;
uniform sampler2D tex;
uniform vec2 scaleXY;

void main() {
    vec2 uv = vTexCoord;

    // UVs from p5.js start from the top, in the shader they start from the bottom
    uv.y = 1.0 - uv.y;
    uv = floor(uv * scaleXY);

    float cellState = texture2D(tex, (uv+0.5)/scaleXY).r;

    float num = -cellState;
    for (float i=-1.0; i<2.0; i++) {
        for (float j=-1.0; j<2.0; j++) {
            float x = (uv.x + i + 0.5);
            float y = (uv.y + j + 0.5);

            num += texture2D(tex, vec2(x, y)/scaleXY).r;
        }
    }

    if (cellState > 0.5) {
        if (num < 1.5) {
            cellState = 0.0;
        } else if (num > 3.5) {
            cellState = 0.0;
        }
    } else {
        if (num > 2.5 && num < 3.5) {
            cellState = 1.0;
        }
    }

    gl_FragColor = vec4(vec3(cellState), 1.0);

}