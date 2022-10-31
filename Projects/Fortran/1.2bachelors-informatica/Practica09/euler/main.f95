program euler
use integracion

implicit none

REAL*8                  ::  a, b, u0, h
integer                 ::  n, i
REAL*8,allocatable      ::  u(:,:)

a = 0
b = 10
n = 0
h = 1
u0 = 1 !Condición inicial

if(n==0) n = int((b-a)/h)
allocate(u(2,n+1))

u(:,:) = meteuler(a,b,n,u0,edo1)

open(unit=10,file="Puntos.dat")
do i = 1,n+1
    write(10,*) u(1,i), u(2,i)
enddo
close(10)

end program euler