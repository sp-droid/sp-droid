program eulersistemas
use integracion

implicit none

REAL*8                  ::  a, b, h
integer                 ::  n, i, m
REAL*8,allocatable      ::  u(:,:), u0(:)

a = 0   !Tiempo inicial
b = 6  !Tiempo final
n = 0   !Numero de puntos-1
h = 0.1   !Diferencial de tiempo
m = 3   !Numero de ecuaciones
allocate(u0(m))
!Condiciones iniciales
u0(1) = 480
u0(2) = 20
u0(3) = 0

if(n==0) n = int((b-a)/h)
allocate(u(m+1,n+1))

u(:,:) = eulerimp(a,b,n,u0,m,sedo)

open(unit=10,file="Puntos.dat")
do i = 1,n+1
    write(10,*) u(:,i)
    write(*,*) u(:,i)
enddo
close(10)

end program eulersistemas