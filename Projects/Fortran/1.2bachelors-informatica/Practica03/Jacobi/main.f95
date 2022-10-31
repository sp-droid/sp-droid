program metodojacobi
    use algebra_lineal
    
    implicit none
    
    integer ::  n, i
    REAL*8,allocatable  ::  AB(:,:), R(:)
    
    
    write(*,*) "Introduzca a continuacion el numero de ecuaciones del sistema (La matriz A debe ser cuadrada)"
    read(*,*) n
    
    allocate(AB(n,n+1))
    allocate(R(n))
    
    write(*,*) "Ahora teclee los coeficientes separados por espacios y las ecuaciones por enter (matriz ampliada)"
    do i = 1,n
        read(*,*) AB(i,:)
    enddo

    write(*,*) "Ahora teclee las soluciones que estime aproximadas a las reales"
    read(*,*) R(:)
    
    call metjacobi(AB,R)
    
    do i = 1,n
        write(*,*) "x",i,":",R(i)
    enddo
    
    deallocate(AB)
    
    end program metodojacobi