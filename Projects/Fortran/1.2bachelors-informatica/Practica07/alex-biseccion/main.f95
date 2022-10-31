program sistemasnolineales
use algebra_nolineal

implicit none

REAL*8      ::  x0, sol, tol

x0 = 10
tol = 10.d-6
call biseccion(x0,sol,tol,funcion3)

write(*,*) sol

end program sistemasnolineales