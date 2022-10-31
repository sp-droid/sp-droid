module funciones

implicit none

contains

function fintR(x) !Función f:R-->R a integrar en el módulo de integración
    REAL*8  ::  fintR, x

    fintR = x

end function

function ecuaciones(x) !Ecuaciones no lineales a resolver en el módulo de algebral no lineal
    REAL*8  ::  ecuaciones, x

    ecuaciones = x**3+x**2-3*x-3

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

end module funciones