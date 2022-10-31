program caso2
implicit none

!Dec.variables
REAL*8,allocatable    ::  Vector(:)
integer     ::  n, i

!Ini.variables
n = 10
allocate(Vector(n))
Vector(1) = 0
Vector(2) = 1

!Cuerpo del programa
do i = 3,n
   Vector(i) = Vector(i-2) + Vector(i-1)
enddo

write(*,"(A19,1X,I1,1X,A1)") "Vector de dimension",n,":"
write(*,202) Vector
202 FORMAT(' ',4G10.2)
write(*,*) "Media:",media_f(Vector,n),"     Desviacion tipica:", dt_f(Vector,n)

contains
    !!Media
    function media_f(Vector,n)
    
    integer,intent(in)  :: n
    REAL*8,intent(in)   :: Vector(n)
    REAL*8      ::  media_f

    integer     ::  i

    do i = 1,n
        media_f = media_f + Vector(i)
    enddo
    media_f = media_f/n
      
    end function media_f

    !!Desviación típica
    function dt_f(Vector,n)
    
    integer,intent(in)  :: n
    REAL*8,intent(in)   :: Vector(n)
    REAL*8      ::  dt_f

    integer     ::  i
    REAL*8      ::  sumat
    sumat = 0

    do i = 1,n
        dt_f = dt_f + Vector(i)
    enddo
    dt_f = dt_f/n

    do i = 1,n
        sumat = sumat + (Vector(i)-dt_f)**2
    enddo
    dt_f = SQRT(sumat*(1./(n-1.)))
      
    end function dt_f

end program caso2