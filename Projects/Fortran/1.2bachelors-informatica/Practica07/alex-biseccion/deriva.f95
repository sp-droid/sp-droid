module derivacion

implicit none

contains
!!Las variables de entrada son:
!-La distancia entre puntos del intervalo
!-El valor de la función en puntos posteriores (fx3) y anteriores (f7x)
!Nomenclatura--> derivada(orden)_método(grado del error)

!Progresivas
function derivada1_progresivaO1(fx,fx1,h)
    REAL*8  ::  derivada1_progresivaO1, fx, h, fx1

    derivada1_progresivaO1 = (fx1-fx)/h

end function

function derivada1_progresivaO2(fx,fx1,fx2,h)
    REAL*8  ::  derivada1_progresivaO2, fx, h, fx1, fx2

    derivada1_progresivaO2 = (-fx2+4*fx1-3*fx)/(2*h)

end function

function derivada2_progresivaO1(fx,fx1,fx2,h)
    REAL*8  ::  derivada2_progresivaO1, fx, fx1, fx2, h

    derivada2_progresivaO1 = (fx2-2*fx1+fx)/(h**2)

end function

function derivada2_progresivaO2(fx,fx1,fx2,fx3,h)
    REAL*8  ::  derivada2_progresivaO2, fx, fx1, fx2, fx3, h

    derivada2_progresivaO2 = (-fx3+4*fx2-5*fx1+2*fx)/(h**2)

end function

!Regresivas
function derivada1_regresivaO1(fx,f1x,h)
    REAL*8  ::  derivada1_regresivaO1, fx, h, f1x

    derivada1_regresivaO1 = (fx-f1x)/h
end function

function derivada1_regresivaO2(fx,f1x,f2x,h)
    REAL*8  ::  derivada1_regresivaO2, fx, f1x, f2x, h

    derivada1_regresivaO2 = (f2x-4*f1x+3*fx)/(2*h)
end function

function derivada2_regresivaO1(fx,f1x,f2x,h)
    REAL*8  ::  derivada2_regresivaO1, fx, f1x, f2x, h

    derivada2_regresivaO1 = (f2x-2*f1x+fx)/(h**2)
end function

function derivada2_regresivaO2(fx,f1x,f2x,f3x,h)
    REAL*8  ::  derivada2_regresivaO2, fx, f1x, f2x, f3x, h

    derivada2_regresivaO2 = (-f3x+4*f2x-5*f1x+2*fx)/(h**2)
end function

!Centradas
function derivada1_centradaO2(f1x,fx1,h)
    REAL*8  ::  derivada1_centradaO2, f1x, fx1, h

    derivada1_centradaO2 = (fx1-f1x)/(2*h)
end function

function derivada1_centradaO4(f1x,f2x,fx1,fx2,h)
    REAL*8  ::  derivada1_centradaO4, f1x, f2x, fx1, fx2, h

    derivada1_centradaO4 = (-fx2+8*fx1-8*f1x+f2x)/(12*h)
end function

function derivada2_centradaO2(f1x,fx1,fx,h)
    REAL*8  ::  derivada2_centradaO2, f1x, fx1, fx, h

    derivada2_centradaO2 = (fx1-2*fx+f1x)/(h**2)
end function

function derivada2_centradaO4(f1x,f2x,fx,fx1,fx2,h)
    REAL*8  ::  derivada2_centradaO4, f1x, f2x, fx, fx1, fx2, h

    derivada2_centradaO4 = (-fx2+16*fx1-30*fx+16*f1x-f2x)/(12*h**2)
end function

end module derivacion