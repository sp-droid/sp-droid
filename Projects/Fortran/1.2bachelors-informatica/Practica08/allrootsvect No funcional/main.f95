program allroots
use algebra_nolineal

implicit none

REAL*8,allocatable      ::  sol(:,:)
REAL*8                  ::  tol
integer                 ::  numraices, n, i

numraices = 1
allocate(sol(numraices,2))

tol = 10.d0**(-6)
call all_roots2D(sol,numraices,tol,sistecuaciones)

do i = 1,numraices
    write(*,*) sol(i,:)
enddo

end program allroots