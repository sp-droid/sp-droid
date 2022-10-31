program shift

use algebra_lineal    

implicit none

!Variables matriz

integer :: i, n
REAL*8, allocatable :: A(:,:), R(:)


!Programa
write(*,*) "Define el tamano de la matriz ampliada (n)x(n+1) (Teclee n)"
read(*,*) n
write(*,*)

write(*,*) "Defina la matriz ampliada"
allocate(A(n,n+1))
allocate(R(n))
do i=1,n
    read(*,*) A(i,:)
enddo
write(*,*)

call metodoscalor(A,R)
write(*,*) R(:)

end program shift