program allroots
use algebra_nolineal

implicit none

REAL*8,allocatable      ::  sol(:)
REAL*8                  ::  tol
integer                 ::  numraices

numraices = 2
allocate(sol(numraices))

tol = 10**(-6)
call all_roots1D(sol,numraices,tol,ecuaciones)

write(*,*) sol(:)

end program allroots