program caso1
implicit none

!Dec.variables
REAL*8,allocatable    ::  Vector(:)
REAL*8      ::  media, dt
integer     ::  n, i

!Ini.variables
n = 4
allocate(Vector(n))
Vector(1) = 0
Vector(2) = 1
media = 0
dt = 0

!Cuerpo del programa
do i = 3,n
   Vector(i) = Vector(i-2) + Vector(i-1)
enddo

call estadistica(Vector,n,media,dt)
write(*,*) Vector, "de dimension",n
write(*,*) "Media:",media,"     Desviacion tipica:", dt

contains
    subroutine estadistica(Vector,n,media,dt)

    integer,intent(in)  :: n
    REAL*8,intent(in)   :: Vector(n)
    REAL*8,intent(inout) :: media, dt
    
    integer     ::  i

    do i = 1,n
        media = media + Vector(i)
    enddo
    media = media/n

    do i = 1,n
        dt = dt + (Vector(i)-media)**2
    enddo
    dt = SQRT(dt*(1./(n-1.)))

    end subroutine estadistica

end program caso1