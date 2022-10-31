program metodogauss
use algebra_lineal

implicit none

integer ::  fil, col, i, nsol
REAL*8,allocatable  ::  AB(:,:), R(:)


write(*,*) "Introduce a continuacion y por orden, el numero de filas y columnas"
read(*,*) fil, col

allocate(AB(fil,col))

write(*,*) "Ahora teclee los coeficientes separados por espacios y las ecuaciones por enter (matriz ampliada)"
do i = 1,fil
    read(*,*) AB(i,:)
enddo

call metgauss(AB,R)
nsol = size(R,1)

do i = 1,nsol
    write(*,*) "x",i,":",R(i)
enddo




deallocate(AB)

end program metodogauss