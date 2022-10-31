program Q1
use auxiliar

implicit none

!DECL SENO GRADO 8, 1/(1+X) GRADO 20
REAL*8,parameter    ::  D=3, pi=Acos(-1.d0), A=pi/(2*D), B=(1-D/20)
REAL*8      ::  sumat, facto
integer     ::  i

!Ini
i = -1
sumat = 0
!Cuerpo
do
    i = i + 1
    if((2*i+1)>8) EXIT
    call factorial((2*i+1),facto)
    sumat = A**(2*i+1)*(-1)**(i)/facto + sumat
enddo
write(*,*) sumat

sumat = 0
i = -1
do
    i = i + 1
    if(i>20) EXIT
    sumat = sumat + B**i
enddo
write(*,*) sumat

end program Q1