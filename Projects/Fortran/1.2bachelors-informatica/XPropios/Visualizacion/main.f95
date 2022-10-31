program visualizacion
use funciones

implicit none

integer     ::  x, y, i, j, ini, m
REAL*8      ::  k, escala
character,allocatable  ::  pantalla(:,:)  

ini = -23
x = 23
y = 23
!escala = 0.01
allocate(pantalla(ini:y,ini:x))
pantalla = "_"

do j = ini,x
    k = j
    k = funcion2(k)
    m = ini
    do i = ini,y
        if(abs(k-m)>abs(k-i)) m = i
    enddo
    if((m/=ini.AND.m/=y).OR.abs(k-m)<1.2d0) pantalla(x-m+ini,j) = "."
enddo

do i = ini,y
    !Recuerda poner el número x+1 aquí debajo, x(A1,2X), se podría hacer pasando enteros a caracteres y no descarto terminarlo en un futuro
    write(*,fmt="(47(A1,2X))") pantalla(i,:)
enddo
read(*,*)

end program visualizacion