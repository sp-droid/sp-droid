program ordenabc
implicit none

!Declaración de variables
REAL    ::  a, b, c
!Inicialización de variables
a = 0
b = 0
c = 0

!Cuerpo del programa
call system("cls")
write(*,*)"Teclee 3 numeros reales para ordenarlos"
read(*,*) a, b, c

if(a<b.AND.a<c) then
    write(*,*) a
    if(b<c) then
        write(*,*) b
        write(*,*) c
    else
        write(*,*) c
        write(*,*) b
    endif
else if(b<a.AND.b<c) then
    write(*,*) b
    if(a<c) then
        write(*,*) a
        write(*,*) c
    else
        write(*,*) c
        write(*,*) a
    endif
else if(c<a.AND.c<b) then
    write(*,*) c
    if(a<b) then
        write(*,*) a
        write(*,*) b
    else
        write(*,*) b
        write(*,*) a
    endif
end if


end program ordenabc