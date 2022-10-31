program algorqr
use algebra_lineal

implicit none
    
integer ::  n, i
REAL*8,allocatable  ::  A(:,:), autoval(:)
    
    
write(*,*) "Introduce a continuacion el numero de filas o columnas (cuadrada)"
read(*,*) n
    
allocate(A(n,n))
allocate(autoval(n))
    
write(*,*) "Ahora teclee los elementos de la matriz, separando columnas por espacios y filas por enter"
do i = 1,n
    read(*,*) A(i,:)
enddo
    
call algoritmoQR(A,autoval,2)

do i = 1,n
    if(abs(autoval(i))<10.d0**(-10)) then
        write(*,*) "Autovalor", i, 0.d0
    else
        write(*,*) "Autovalor", i, autoval(i)
    endif
enddo

deallocate(A)
deallocate(autoval)
end program algorqr