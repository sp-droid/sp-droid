program esferapos
use variables
use espacio

implicit none
!Declaración
integer       ::  n_filas, i, io
REAL*8        ::  radio
REAL*8        ::  centro(3)

type(nodo), allocatable  :: puntos(:)

!Inicialización
n_filas = 0
centro = 0
centro(3) = 2
radio = 2

!Cuerpo del programa
!Contado de puntos y lectura de datos
open(unit=10, file="datos.dat")
do 
    read(10,*,iostat=io)
    if(io<0) EXIT
    n_filas = n_filas + 1
enddo

allocate(puntos(n_filas))
rewind(10)

do i = 1,n_filas
    read(10,*) puntos(i)%coordenadas
enddo
close(10)

!Calculamos distancia al centro de una esfera de radio 2 situada en el punto (0,0,2)
do i = 1,n_filas
    call distancia(puntos(i)%coordenadas,centro(:),puntos(i)%distancia)
enddo

!Posición relativa a la esfera
do i = 1,n_filas
    call esfrelativ(radio,puntos(i)%distancia,puntos(i)%pos)
enddo    

!Ordenamos cada nodo según su distancia al centro de la esfera en orden ascendente, mostrando luego el resultado por pantalla
call ordennodo(puntos(:))
do i = 1,n_filas
    write(*,*) puntos(i)
enddo

end program esferapos