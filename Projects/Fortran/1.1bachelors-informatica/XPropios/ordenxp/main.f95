program ordenxp
implicit none

!Declaración de variables
integer     ::  n, i, j
real*8, allocatable    :: V(:)
REAL        :: x

!Inicialización de variables

!Cuerpo del programa

write(*,*)"Cuantos numeros desea ordenar?"
read(*,*) n
allocate(V(n))
V = 0.d0
write(*,*) "Introduzcalos para proceder a ordenarlos"
do i=1,n
    read(*,*) V(i)
enddo

do i = 1,n-1

    do j = 1,n-1
        if(V(j)>V(j+1)) then
            V(j) = V(j)+V(j+1)
            V(j+1) = V(j)-V(j+1)
            V(j) = V(j)-V(j+1)
        endif
    enddo
enddo

write(*,900) V(:)
900 format (F16.3)

end program ordenxp