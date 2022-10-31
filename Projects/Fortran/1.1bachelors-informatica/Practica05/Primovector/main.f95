program primovector
implicit none

!Declaración e inicialización de variables
integer, allocatable    ::  P(:)
integer     ::  i, j, n, k

k = 0

!Cuerpo del programa
!Leer n y contar primos <n
call system("cls")
write(*,*) "Introduzca numero entero hasta el cual se buscaran primos"
read (*,*) n

do i = 2,n-1
    j = 1
    do
        j = j + 1
        if((MOD(i,j)==0).AND.(j/=i)) then
            EXIT
        elseif(j==i) then
            k = k + 1
            EXIT
        endif
    enddo
enddo
!Crear vector con primos hasta n componentes
allocate(P(k))
P = 0
!Rellenar vector con primos hasta n
k = 1
do i = 2,n-1
    j = 1
    do
        j = j + 1
        if((MOD(i,j)==0).AND.(j/=i)) then
            EXIT
        elseif(j==i) then
            P(k) = j
            k = k + 1
            EXIT
        endif
    enddo
enddo
!Imprimir el vector en pantalla
write(*,*) P(:)

end program primovector