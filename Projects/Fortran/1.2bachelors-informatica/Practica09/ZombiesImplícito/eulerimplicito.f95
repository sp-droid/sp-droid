module eulerimplicito
use funciones_zombies
use algebra_nolineal

implicit none

contains

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

    tol = 10.d0**(-8)
    h = (b-a)/n
    eulerimp(1,1) = a
    eulerimp(2:m+1,1) = u0(:)
    
    do i = 1,n
        eulerimp(1,i+1) = a+i*h
        call newton_raphsonmod(eulerimp(:,i+1),eulerimp(2:m+1,i+1),eulerimp(2:m+1,i),tol,h,m,f1)
    enddo

end function

end module eulerimplicito