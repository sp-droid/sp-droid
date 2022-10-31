module funciones

implicit none

contains

function fintR(x) !Función f:R-->R a integrar en el módulo de integración
    REAL*8  ::  fintR, x

    fintR = x

end function

function ecuaciones(x) !Ecuaciones no lineales a resolver en el módulo de álgebra no lineal
    REAL*8  ::  ecuaciones, x

    ecuaciones = (x/3.d0)**2 - 20*exp(-(x**2)/100)

end function

function sistecuaciones(X,n) !Sistemas de ecuaciones no lineales a resolver en el módulo de álgebra no lineal
    integer             ::  n
    REAL*8              ::  sistecuaciones(n), X(n)

    !NOTACIÓN, x(1) corresponde a la primera variable x, x(2) a la y... en el caso de que estemos trabajando en ese sistema
    sistecuaciones(1) = x(1)**2-4
    sistecuaciones(2) = 2*x(2)+2

end function

function edo1(x) !Ecuación diferencial ordinaria edo1D=dy/dx donde y=x(2) y x=x(1)
    REAL*8  ::  edo1, x(2)

    edo1 = x(2)

end function

function sedo(x,m) !Sistema de ecuaciones diferenciales, x=x(1),y1=x(2),y2=x(3)
    integer     ::  m
    REAL*8      ::  sedo(m), x(m+1), a, b, g, i, o

    open(10,file="Parametros1.txt")
        read(10,*) a
        read(10,*) b
        read(10,*) g
        read(10,*) i
        read(10,*) o
    close(10)

    sedo(1) = a-g*x(2)*x(3)-b*x(2)
    sedo(2) = g*x(2)*x(3)+o*x(4)-i*x(2)*x(3)
    sedo(3) = b*x(2)+i*x(2)*x(3)-o*x(4)

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