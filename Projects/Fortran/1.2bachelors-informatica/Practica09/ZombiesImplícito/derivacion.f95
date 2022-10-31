module derivacion

implicit none

contains
!!Las variables de entrada son:
!-La distancia entre puntos del intervalo
!-El valor de la función en puntos posteriores (fx3) y anteriores (f7x)
!Nomenclatura--> derivada(orden)_método(grado del error)

function derivada1_centradaO2(f1x,fx1,h)
    REAL*8  ::  derivada1_centradaO2, f1x, fx1, h

    derivada1_centradaO2 = (fx1-f1x)/(2*h)
end function


end module derivacion