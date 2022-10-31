module integracion
use funciones

implicit none

contains

function intervalR(a,b,h,n,metodo)
    REAL*8       ::  a, b, h, intervalR
    integer      ::  n, metodo

    if(n==0) then
        n = int((b-a)/h)
        h = (b-a)/n
    elseif(h==0) then
        h = (b-a)/n
    endif

    if(metodo==0)   then  !Constante, Riemann
        intervalR = integRconstante(a,h,n)
    elseif(metodo==1) then  !Lineal, Trapecio
        intervalR = integRlineal(a,h,n)
    elseif(metodo==2) then  !Parabólico, Simpson
        !intervalR = 
    endif
    
end function

function integRconstante(a,h,n)
    REAL*8      ::  a, h, integRconstante
    integer     ::  n

    integer     ::  i
    REAL*8      ::  x, suma

    suma = 0

    do i = 0,n-1
        x = a+i*h
        suma = suma + h*fintR(x)
    enddo
    integRconstante = suma

end function

function integRlineal(a,h,n)
    REAL*8      ::  a, h, integRlineal
    integer     ::  n

    integer     ::  i
    REAL*8      ::  x, suma

    suma = 0

    do i = 0,n-1
        x = a+i*h
        suma = suma + h*(fintR(x)+fintR(x+h))/2
    enddo
    integRlineal = suma

end function

end module integracion