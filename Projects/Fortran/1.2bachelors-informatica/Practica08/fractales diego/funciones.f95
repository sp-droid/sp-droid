module funciones

implicit none

contains

function fintR(x) !Función f:R-->R a integrar en el módulo de integración
    REAL*8  ::  fintR, x

    fintR = x

end function

function ecuaciones(x) !Ecuaciones no lineales a resolver en el módulo de álgebra no lineal
    REAL*8  ::  ecuaciones, x

    ecuaciones = x**3+x**2-3*x-3

end function

function sistecuaciones(X,n) !Sistemas de ecuaciones no lineales a resolver en el módulo de álgebra no lineal
    integer             ::  n
    REAL*8              ::  sistecuaciones(n), X(n)

    !NOTACIÓN, x(1) corresponde a la primera variable x, x(2) a la y... en el caso de que estemos trabajando en ese sistema
    sistecuaciones(1) = x(1)**3.d0-3.d0*(x(2)**2.d0)*x(1)-12.d0
    sistecuaciones(2) = 3.d0*(x(1)**2.d0)*x(2)-x(2)**3.d0
    !sistecuaciones(1)=x(1)**7-(21.d0*(x(1)**5)*(x(2)**2))+35*((x(1)**3)*(x(2)**4))-7*(x(1)*(x(2)**6))-1
    !sistecuaciones(2)=7*((x(1)**6)*(x(2)**1))-35*((x(1)**4)*(x(2)**3))+21*((x(1)**2)*(x(2)**5))-x(2)**7

end function

function funcion1(x) !Polinomio de grado 3
    REAL*8  ::  funcion1, x

    funcion1 = x**3/5+4*x**2/5-7*x/5-2

end function

function derivadafuncion1(x)
    REAL*8  ::  derivadafuncion1, x

    derivadafuncion1 = 3./5*x**2+8./5*x-7./5

end function

function funcion2(x) !Tangente hiperbólica de x+2
    REAL*8  ::  funcion2, x
    
    funcion2 = tanh(x+2)
    
end function
    
function derivadafuncion2(x)
    REAL*8  ::  derivadafuncion2, x
    
    derivadafuncion2 = 1/(cosh(x+2))**2
   
end function

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

!Jacobiano de una función calculado numéricamente para Newton-Raphson específicamente
function jacob(X0,n) 
    REAL*8  ::  jacob(n,n), X0(n)
    integer ::  n

    integer     ::  i, j
    REAL*8      ::  h
    REAL*8,allocatable  ::  Xceteris(:), fant(:), fpost(:)

    h = 0.01d0
    allocate(Xceteris(n),fant(n),fpost(n))

    do j = 1,n
        Xceteris = X0
        Xceteris(j) = Xceteris(j)-h
        fant = sistecuaciones(Xceteris,n)
        Xceteris(j) = Xceteris(j)+2*h
        fpost = sistecuaciones(Xceteris,n)
        do i = 1,n
            jacob(i,j) = derivada1_centradaO2(fant(i),fpost(i),h)
        enddo
    enddo


end function

end module funciones