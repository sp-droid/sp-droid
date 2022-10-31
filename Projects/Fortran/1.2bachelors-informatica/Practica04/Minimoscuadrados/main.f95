program curvasderegresion
    use algebra_lineal
    
    implicit none
    
    integer ::  n, i, ndot
    REAL*8,allocatable  ::  AB(:,:), R(:)

    open(unit=10,file="Puntos.dat")
    close(10)
    write(*,*) "Introduzca a continuacion el grado de la curva de regresion, y los puntos en el fichero Puntos.dat"
    read(*,*) n
    
    write(*,*) "Indique cuantos puntos ha introducido"
    read(*,*) ndot

    allocate(AB(ndot,2))
    allocate(R(n+1))

    open(unit=10,file="Puntos.dat")
    do i = 1,ndot
        read(10,*) AB(i,:)
    enddo
    close(10)
    
    call regremin(AB,n,R)
    
    do i = 1,n+1
        write(*,*) "x*  de grado",i-1,":",R(i)
    enddo
    
    deallocate(AB)
    deallocate(R)
    
    end program curvasderegresion