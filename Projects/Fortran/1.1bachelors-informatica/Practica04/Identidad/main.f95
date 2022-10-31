program Identidad
implicit none

!Declaracion de variables
integer,parameter   ::  n = 10
integer     ::  Id(n,n)
integer     ::  i

!Inicializacion de variables
Id = 0

!Cuerpo del programa
write(*,*) "Matriz nxn"
do i=1,n
    Id(i,i) = 1
    write(*,*) Id(i,:)
enddo

end program Identidad