program sistemasnolineales
use algebra_nolineal

implicit none

REAL*8,allocatable      ::  x0(:), sol(:)
REAL*8                  ::  tol
integer                 ::  n

n = 2
allocate(x0(n))
allocate(sol(n))

x0(1) = 1
x0(2) = 1
tol = 10**(-6)
call newton_raphson(x0,sol,tol,sistecuaciones)

write(*,*) sol(:)

end program sistemasnolineales