program Q3
implicit none
!DECL
integer,parameter   ::  D=3, n =(5+D)
integer     ::  M, i, j
REAL*8,allocatable  ::  A(:,:)
REAL*8      ::  sumat, cant

!Cuerpo
sumat = 0
do M = 0,n
    allocate(A(M+1,M+1))
    A = 0
    do i = 0,M
        do j = 0,M
            cant = M
            A(i+1,j+1) = (i/cant)**j
        enddo
    enddo
    A = matmul(A,A)
    do i = 1,M+1
        sumat = sumat + A(i,i)
    enddo
    deallocate(A)
enddo

write(*,*)  sumat

end program Q3