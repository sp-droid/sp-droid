program eulersistemasimplicito
use eulerimplicito

implicit none

REAL*8                  ::  a, b, h
integer                 ::  n, i, m, j
REAL*8,allocatable      ::  u(:,:), u0(:)
character(27)            ::  archivo
character(2)            ::  num_archivo

a = 0   !Tiempo inicial
b = 4  !Tiempo final
n = 0   !Numero de puntos-1
h = 0.01   !Diferencial de tiempo

if(n==0) n = int((b-a)/h)
!m es el número de ecuaciones, u0 las condiciones iniciales

!Sistema sencillo
m = 3
allocate(u0(m))
u0(1) = 480
u0(2) = 20
u0(3) = 0
allocate(u(m+1,n+1))

u(:,:) = eulerimp(a,b,n,u0,m,zombies1)

do j=1,m
    write(num_archivo,"(I2.2)") j
    archivo = "Resultados1\Zombies1-" // num_archivo // '.dat'
    open(unit=10,file=Archivo)
        do i=1,n+1
            write(10,*) u(1,i), u(j+1,i)
        enddo
    close(10)
enddo

!Sistema complejo 
m = 5
deallocate(u0)
deallocate(u)
allocate(u0(m))
u0(1) = 500
u0(2) = 1
u0(3) = 0
u0(4) = 0
u0(5) = 0
allocate(u(m+1,n+1))

u(:,:) = eulerimp(a,b,n,u0,m,zombies2)

do j=1,m
    write(num_archivo,"(I2.2)") j
    archivo = "Resultados2\Zombies2-" // num_archivo// '.dat'
    open(unit=10,file=Archivo)
        do i=1,n+1
            write(10,*) u(1,i), u(j+1,i)
        enddo
    close(10)
enddo

!Sistema propio
m = 6
deallocate(u0)
deallocate(u)
allocate(u0(m))
u0(1) = 1
u0(2) = 1000
u0(3) = 0
u0(4) = 0
u0(5) = 0
u0(6) = 0
allocate(u(m+1,n+1))

u(:,:) = eulerimp(a,b,n,u0,m,zombies3)

do j=1,m
    write(num_archivo,"(I2.2)") j
    archivo = "Resultados3\Zombies3-" // num_archivo // '.dat'
    open(unit=10,file=Archivo)
        do i=1,n+1
            write(10,*) u(1,i), u(j+1,i)
        enddo
    close(10)
enddo

end program eulersistemasimplicito