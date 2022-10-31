program Q2
use funciones

implicit none

integer,parameter   ::  n = 1000, D = 3, a = (-5-D), b = (5+D)
integer     ::  i
REAL*8      ::  x, y, sumat, cant
REAL*8,allocatable  ::  XV(:), FV(:)


allocate(XV(N+1))
allocate(FV(N+1))
cant = n
do i = 0,n
    XV(i+1) = a + i*(b-a)/cant
    XV(i+1) = f(XV(i+1))
enddo
do i = 0,n-1
    sumat = XV(i+1)*(b-a)/cant + sumat
enddo

write(*,*) sumat

end program Q2