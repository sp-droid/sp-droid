program politaylor
implicit none
!Declaración de variables
REAL*8      ::  x, sumat
integer     ::  n, i, facto, q
!Inicialización de variables
sumat = 0
i = 0
!Cuerpo del programa
call system("cls")
write(*,*) "Introduzca un numero real entre -1 y +1 y el termino n de la sucesion"
read(*,*) x, n

if(x<=-1.OR.x>=1) then
    write(*,*) "Numero introducido fuera del intervalo (-1,1)"
    STOP
endif

do i = 0,(n-1)
    facto = 1
    q = 0
    do
        q = q + 1
        facto = facto*q
        if(q==(2*i+1)) then
            EXIT
        endif
    enddo
    sumat = sumat + (((-1.)**i)*(x**(2.*i+1))/facto)
enddo
write(*,*) "Seno de",x,"~~", sumat

end program politaylor