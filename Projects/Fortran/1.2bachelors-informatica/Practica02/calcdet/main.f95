program calcdet
use algebra_lineal

implicit none

integer ::  n, i
REAL*8  ::  det
REAL*8,allocatable  ::  A(:,:)


write(*,*) "Introduce el numero de filas y/o columnas (cuadrada)"
read(*,*) n

allocate(A(n,n))

write(*,*) "Ahora teclee los elementos de cada fila separados por espacios y las propias filas por enter"
do i = 1,n
    read(*,*) A(i,:)
enddo

call determinante(A,det)
write(*,*) det


deallocate(A)

end program calcdet