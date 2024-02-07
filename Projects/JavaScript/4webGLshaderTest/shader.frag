// shader.frag
precision mediump float;

varying vec2 vTexCoord;

uniform sampler2D u_texture;

void main() {
  vec4 texColor = texture2D(u_texture, vTexCoord);
  
  // Do your computation here
  // For now, let's just pass the texture color directly
  if (vTexCoord.x < 0.5) {
    gl_FragColor = vec4(texColor.xyz, 1.0);
  } else {
    gl_FragColor = vec4(vTexCoord.xy, 0.0, 0.0);
  }
}