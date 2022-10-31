program programaquederiva
    use funciones
    use derivacion
    
    implicit none
    
    integer     ::  i
    REAL*8      ::  a, b, h, res(8)

    a = -8
    b = 8
    h = 0.5

    !Este programa pintará los puntos de la derivada de una función en un intervalo [a,b] a distancia h
    
    open(unit=10,file="Puntos.dat")
    do i = 0,int((b-a)/h)
        res(1) = a+i*h             !x
        res(2) = funcion2(res(1))  !f(x)

        res(3) = derivada_progresiva(res(2),funcion2(a+(i+1)*h),h)
        res(4) = abs(res(3)-derivadafuncion2(res(1)))

        res(5) = derivada_regresiva(res(2),funcion2(a+(i-1)*h),h)
        res(6) = abs(res(5)-derivadafuncion2(res(1)))

        res(7) = derivada_centrada(funcion2(a+(i-1)*h),funcion2(a+(i+1)*h),h)
        res(8) = abs(res(7)-derivadafuncion2(res(1)))
        write(10,fmt="(8(F7.3,8X))") res(:)
    enddo

    close(10)

    end program programaquederiva