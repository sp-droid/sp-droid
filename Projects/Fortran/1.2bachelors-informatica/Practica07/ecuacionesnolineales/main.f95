program sistemasnolineales
use algebra_nolineal

implicit none

REAL*8      ::  x0, sol, tol

x0 = 10
tol = 10**(-6)
call newton_raphson1D(x0,sol,tol)

write(*,*) sol

end program sistemasnolineales