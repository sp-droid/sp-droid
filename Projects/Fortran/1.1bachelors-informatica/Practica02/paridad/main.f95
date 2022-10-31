program paridad

implicit none

!declaración de variables
integer     ::  n

!inicialización
n=0

!Limpieza de consola
call system("cls")
!Cuerpo
write(*,*)"Introduzca un numero entero"
read(*,*) n

if(MOD(n,2)==0) then
    if(MOD(n,4)==0) then
    write(*,*) n,"es un numero par, multiplo de 4"
    else
    write(*,*) n,"es un numero par, no multiplo de 4"
    end if
else
    if(MOD(n,3)==0) then
    write(*,*) n,"es un numero impar, multiplo de 3"
    else
    write(*,*) n,"es un numero impar, no multiplo de 3"
    end if
end if

end program paridad