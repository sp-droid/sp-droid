program shift

use algebra_lineal    

implicit none

!Variables matriz

integer :: i, n
real*8, allocatable :: A(:,:), autovalores(:), autovectores(:,:)


!Programa
write(*,*) 'Define el tamano de la matriz n x n'
read(*,*) n
write(*,*)

write(*,*) 'Define la matriz'
allocate(A(n,n))
do i=1,n
    read(*,*) A(i,:)
enddo
write(*,*)
allocate(autovalores(n))
allocate(autovectores(n,n))

call algoritmoQR(A,autovalores,2)
write(*,*) autovalores(:)

call metshift(A,autovalores,autovectores)
write(*,*) autovalores(:)
write(*,*) "------------------"
do i=1,n
    write(*,*) autovectores(i,:)
enddo

end program shift