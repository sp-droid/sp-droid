import vpython as vp
import numpy as np

scene = vp.canvas(width=600, height=600, center=vp.vec(0,5,0))

o1 = vp.sphere(pos=vp.vec(0,0,0),radius=100,color=vp.color.orange,make_trail = True)
o2 = vp.sphere(pos=vp.vec(-300,0,0),radius=10,texture = vp.textures.earth,make_trail = True)

o1v = vp.vec(0,0,0)
o2v = vp.vec(0,0,0)

for i in range(10000):
    vp.rate(100)

    #Atracción gravitatoria
    dist = vp.mag(o2.pos-o1.pos)
    RadialVector = (o2.pos-o1.pos)/dist
    Fgrav = -100000*RadialVector/dist**2
    o1v = o1v-Fgrav/100
    o2v = o2v+Fgrav/10

    #Colisiones
    if dist <= o1.radius:
        ang = np.arccos(vp.dot(RadialVector,vp.norm(o2v)))-np.pi
        print(ang*360/2/np.pi)
        o1v = (o1.radius-o2.radius)/(o2.radius+o1.radius)*o1v
        o2v = (o2.radius-o1.radius)/(o2.radius+o1.radius)*o2v
    
    o1.pos += o1v
    o2.pos += o2v
