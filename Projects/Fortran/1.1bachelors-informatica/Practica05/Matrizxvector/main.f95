program matrizxvector
implicit none
!Declaración e inicialización de variables
REAL, allocatable    ::  V(:), A(:,:)
REAL        ::  k
integer     ::  i, j, n, m

!Lectura de datos
call system("cls")
write(*,*) "Introduzca m y n, siendo m el numero de filas y n el de columnas de la matriz"
write(*,*) "Esta matriz generada automaticamente se multiplicara por un vector de n componentes"
read(*,*) m, n

!Validación de los mismos
if(m<=0.OR.n<=0) then
    write(*,*) "Ha introducido al menos un numero menor o igual que 0."
    STOP
endif

!Asignación de valores
allocate(A(m,n))
A = 3
do i = 1,m
    do j = 1,n
        if(j==1.OR.j==n) then
            if(MOD(i,2)/=0) then
                A(i,j) = A(i,j) + 2
                if(i==3) then
                    A(i,j) = A(i,j) + 2
                endif
            else
                A(i,j) = A(i,j) + A(i-1,j)*2
            endif
        else
            A(i,j) = A(i,j-1) + A(i,j+1)
        endif
    enddo
enddo


allocate(V(n))
V = 0
V(2) = 1
do j = 3,n
    V(j) = V(j-2) + V(j-1)
enddo

!Mostramos la matriz y el vector de dimensiones deseadas por pantalla
write(*,*) "El vector es igual a:"
write(*,*) V(:)
write(*,*) "La matriz es igual a:"
do i = 1,m
    write(*,*) A(i,:)
enddo

!Multiplicación de A por V
do i = 1,m
    k = 0
    do j = 1,n
        k = k + A(i,j)*V(j)
    enddo
    A(i,1) = k
enddo

deallocate(V)
allocate(V(m))

!Como no he querido usar un segundo vector, he ido dejando los resultados de las operaciones en la primera columna de A, con este pequeño bucle la pasaremos a V.
do i = 1,m
    V(i) = A(i,1)
enddo

!Escribir el resultado en pantalla
write(*,*) "El resultado de su multiplicacion (AxV) es:"
write(*,*) V(:)

!Liberamos memoria
deallocate(A)
deallocate(V)

end program matrizxvector