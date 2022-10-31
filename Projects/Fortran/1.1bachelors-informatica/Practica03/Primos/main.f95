program primos
implicit none

!Declaración de variables
integer     ::  i, num
!Inicialización de las variables
i = 1
!Cuerpo del programa
call system("cls")
write(*,*) "Introduzca un numero entero positivo"
read(*,*) num
if(num<=0) then
    write(*,*) "Numero introducido incorrecto"
    STOP
endif

do 
    i = i + 1
    if(num==1) then
        write(*,*) "El numero 1 no es primo por definicion"
        EXIT
    else if((MOD(num,i)==0).AND.(i/=num)) then
        write(*,*) "El numero",num, "no es primo"
        write(*,*) "Divisores aparte de 1 y el propio numero:"
        i = 1
        do
            i = i + 1
            if((MOD(num,i)==0).AND.(i/=num)) then
                write(*,*) i,"es un divisor de", num
            else if(i==num) then
                EXIT
            end if
        end do
        EXIT
    else if(i==num) then
        write(*,*) "El numero",num, "es primo"
        EXIT
    endif
enddo

end program primos