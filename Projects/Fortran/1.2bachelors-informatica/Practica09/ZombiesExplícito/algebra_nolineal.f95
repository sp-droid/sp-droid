module algebra_nolineal
use funciones_zombies

implicit none

contains

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

end module algebra_nolineal