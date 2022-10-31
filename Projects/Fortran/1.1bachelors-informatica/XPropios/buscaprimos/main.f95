program buscaprimos
implicit none
!Declaración de variables
integer     ::  n, num, i, orden
!Inicialización de variables
n = 0
num = 0
orden = 0
!Cuerpo del programa
call system("cls")
write(*,*) "Introduzca el numero entero positivo hasta el cual se buscaran numeros primos"
read(*,*) n
if(n<=0) then
    write(*,*) "Numero introducido incorrecto"
    STOP
endif
write(*,*) "Numeros primos hasta",n,":"

do num = 2,n
    i = 1
    do
        i = i + 1
        if((MOD(num,i)==0).AND.((i/=num).OR.(num==1))) then
            EXIT
        else if(i>=num) then
            orden = orden + 1
            write(*,*) "Primo numero",orden,":",num
            EXIT
        endif
    end do
end do

end program buscaprimos