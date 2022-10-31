program gramvshouse
use algebra_lineal

implicit none
    
integer ::  n, i, j, metodo
REAL*8  ::  t, t1
REAL*8,allocatable  ::  A(:,:), autoval(:)

!-------------------------------------------------------
!Variabes necesarias para la subrutina dgeev
!-------------------------------------------------------

character :: JOBVL,JOBVR 
integer :: lda 
real*8,allocatable :: WR(:),WI(:) 
real*8,allocatable :: VL(:,:),VR(:,:)
integer :: LDVL,LDVR 
real*8,allocatable:: work(:) 
integer :: lwork 
integer :: info 

    
write(*,*) "Introduce un multiplo de 4 para la dimension de la matriz"
read(*,*) n
    
allocate(A(n,n))
allocate(autoval(n))
autoval = 0
    
write(*,*) "Ahora teclee los elementos de una matriz 4x4, separando columnas por espacios y filas por enter"
do i = 1,4
    read(*,*) A(i,1:4)
enddo

do i = 1,4
    do j = 1,n,4
        A(i,j:j+3) = A(i,1:4)
    enddo
enddo
do i = 1,n,4
    do j = 1,4
        A(i+j-1,:) = A(j,:)
    enddo
enddo

! ##LAPACK##

    JOBVL='N' 
    JOBVR='N'
    
    LDA=max(1,n)
    
    allocate(WR(N));allocate(WI(N)) 
    
    if (JOBVR=='V') then 
        LDVR=N
    elseif (JOBVR=='N') then
        LDVR=1
    endif
    
    if (JOBVL=='V') then
        LDVL=N
    elseif (JOBVL=='N') then
        LDVL=1
    endif
    
    if (JOBVR=='V'.or.JOBVL=='V') then 
        LWORK=max(1,4*n)
    else
        LWORK=max(1,3*n)
    endif
    
    allocate(VR(LDVR,N));allocate(VL(LDVL,N));allocate(Work(Max(1,LWORK)))

! ##LAPACK##

CALL CPU_TIME(t)

call dgeev (JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)

CALL CPU_TIME(t1)

write(*,*) "(Lapack)Tiempo en s:", t1-t

metodo = 1

CALL CPU_TIME(t)

!call algoritmoQR(A,autoval,metodo)

CALL CPU_TIME(t1)

write(*,*) "(GRAM-SCHMIDT)Tiempo en s:", t1-t

autoval = 0
metodo = 2

CALL CPU_TIME(t)

call algoritmoQR(A,autoval,metodo)

CALL CPU_TIME(t1)

write(*,*) "(HOUSEHOLDER)Tiempo en s:", t1-t

deallocate(A)
deallocate(autoval)

end program gramvshouse