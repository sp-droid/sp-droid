module integracion
use funciones
use algebra_nolineal

implicit none

contains

!Función que llama a uno u otro método y dota de valor a h y n según los parámetros escogidos
function intervalR(a,b,h,n,metodo)
    REAL*8       ::  a, b, h, intervalR
    integer      ::  n, metodo

    if(n==0) then
        n = int((b-a)/h)
        h = (b-a)/n
    elseif(h<EPSILON(1.d0)) then
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

!Integral en R de orden 0 (rectángulos)
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

!Integral en R de orden 1 (trapecios)
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

!Método de Euler explícito
function meteuler(a,b,n,u0,f1)
    integer  ::  n
    REAL*8   ::  a, b, u0, meteuler(2,n+1)

    integer     ::  i
    REAL*8      ::  h

    interface 
       function f1(x)
           REAL*8 :: x(2), f1
       end function 
    end interface 

    h = (b-a)/n
    meteuler(1,1) = a
    meteuler(2,1) = u0
    
    do i = 1,n
        meteuler(1,i+1) = a+i*h
        meteuler(2,i+1) = meteuler(2,i)+f1(meteuler(:,i))*h
    enddo

end function

function sisteuler(a,b,n,u0,m,f1)
    integer  ::  n, m
    REAL*8   ::  a, b, u0(m), sisteuler(m+1,n+1)

    integer     ::  i
    REAL*8      ::  h

    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface 

    h = (b-a)/n
    sisteuler(1,1) = a
    sisteuler(2:m+1,1) = u0(:)
    
    do i = 1,n
        sisteuler(1,i+1) = a+i*h
        sisteuler(2:m+1,i+1) = sisteuler(2:m+1,i)+f1(sisteuler(:,i),m)*h
    enddo

end function

!Método de Euler implícito
function eulerimp(a,b,n,u0,m,f1)
    integer  ::  n, m
    REAL*8   ::  a, b, u0(m), eulerimp(m+1,n+1)

    integer     ::  i
    REAL*8      ::  h, tol

    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface

    tol = 10.d0**(-6)
    h = (b-a)/n
    eulerimp(1,1) = a
    eulerimp(2:m+1,1) = u0(:)
    
    do i = 1,n
        eulerimp(1,i+1) = a+i*h
        call newton_raphsonmod(eulerimp(:,i+1),eulerimp(2:m+1,i+1),eulerimp(2:m+1,i),tol,h,m,f1)
    enddo

end function

end module integracion