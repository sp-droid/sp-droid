program algorqr
use algebra_lineal

implicit none
    
integer ::  fil, col, i
REAL*8,allocatable  ::  A(:,:), Q(:,:), R(:,:)


write(*,*) "Introduce a continuacion el numero de filas y columnas"
read(*,*) fil, col
    
allocate(A(fil,col))
allocate(R(fil,col))
allocate(Q(fil,fil))
    
write(*,*) "Ahora teclee los elementos de la matriz, separando columnas por espacios y filas por enter"
do i = 1,fil
    read(*,*) A(i,:)
enddo
    
call factorQRhouseholder(A,Q,R)

do i = 1,fil
    write(*,*) R(i,:)
enddo

write(*,*)

do i = 1,fil
    write(*,*) Q(i,:)
enddo

write(*,*)
A = matmul(Q,R)
do i = 1,fil
    write(*,*) A(i,:)
enddo

end program algorqr