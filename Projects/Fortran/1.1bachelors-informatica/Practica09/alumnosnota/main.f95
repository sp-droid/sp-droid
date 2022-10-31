program alumnosnota
use estadistica
use opvector

implicit none

!Definición y declaración
integer, parameter :: numnotas = 3

type alumno
character(len=15) :: nombre
character(len=30) :: apellidos
integer :: num_expediente
real :: nota(numnotas)
real :: med
end type alumno

type(alumno), allocatable :: clase(:)
type(alumno)    ::  inter

REAL     :: media_clase
REAL,allocatable    ::  vaux(:)
integer :: n, j, i, Ierr

!Inicialización
write(*,*) "Introducir numero de alumnos"
read(*,*) n
allocate(clase(n), stat=Ierr)
if (Ierr > 0) stop "*Numero introducido incorrecto*"
allocate(vaux(n))

!Cuerpo del programa
do i=1,n
    write(*,*) "Nombre y apellidos del alumno ", i
    read(*,*) clase(i) %nombre, clase(i) %apellidos
    write(*,*) "Expediente del alumno ", i
    read(*,*) clase(i) %num_expediente
    write(*,*) "Introducir notas del alumno", i
    read(*,*) clase(i) %nota(:)
    call media(clase(i)%nota(:),clase(i)%med)
end do

!Cálculo de la nota media
call media(clase%med,media_clase)
write(*,*) "Media de la clase", media_clase

!Ordenar el vector clase por nota media, descendente
vaux = clase(:)%med
call ordenvect(vaux(:))

do i = 1,n
    do j = 1,n
        if(abs(vaux(i)-clase(j)%med)<epsilon(1.d0)) then
            inter = clase(i)
            clase(i) = clase(j)
            clase(j) = inter
            EXIT
        endif
    enddo
enddo

!Muestreo en pantalla de cada alumno con su número de expediente + notamedia personal
write(*,*) "Datos de cada alumno ordenado segun nota media en orden descendente:"
do i = 1,n
    write(*,fmt="(A15,2X,A15,3X,I6,3X,F5.3)") clase(i)%nombre, clase(i)%apellidos, &
    clase(i)%num_expediente, clase(i)%med
enddo

end program alumnosnota