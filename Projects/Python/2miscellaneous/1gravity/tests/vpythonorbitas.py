import vpython as vp

scene = vp.canvas(width=600, height=600, center=vp.vec(0,5,0))

Sun = vp.sphere(pos=vp.vec(0,0,0), radius=100, color=vp.color.orange)
earth = vp.sphere(pos=vp.vec(-200,0,0), radius=10,texture = vp.textures.earth, make_trail = True)

earthv = vp.vec(0,0,8)

for i in range(1000):
    vp.rate(100)
    dist = vp.mag(earth.pos)
    RadialVector = (earth.pos-Sun.pos)/dist
    Fgrav = -10000*RadialVector/dist**2
    earthv = earthv+Fgrav
    earth.pos += earthv
    if dist <= Sun.radius: break