program IntegracionR
use integracion

implicit none

REAL*8      ::  a, b, h, integ
integer     ::  n, resp, metodo

write(*,*) "Recuerde agregar la funcion a integrar en el modulo de funciones que se llame fintR"
write(*,*) "Escriba 1 o 2 si va a aportar la distancia entre los subintervalos o el numero de subintervalos"
read(*,*) resp

write(*,*) "Escriba 0, 1 o 2 si desea una integrar mediante aproximaciones de orden 0, 1 o 2"
read(*,*) metodo

if(resp==1) then
    write(*,*) "Escriba la distancia entre subintervalos"
    read(*,*) h
    n = 0
elseif(resp==2) then
    write(*,*) "Escriba el numero de subintervalos"
    read(*,*) n
    h = 0
else
    write(*,*) "Error al introducir datos"
    STOP
endif

write(*,*) "Escriba a y b ( I=(a,b) )"
read(*,*) a, b

write(*,*) "Valor de la integral de la funcion en el intervalo:", intervalR(a,b,h,n,metodo)

end program IntegracionR