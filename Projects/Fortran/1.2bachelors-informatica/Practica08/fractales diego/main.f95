program sistemasnolineales
    use algebra

    implicit none
    
    INTEGER, PARAMETER :: NRAICES = 3,NPUNTOS = 500
    REAL*8, PARAMETER:: A = 1000.0,tol=1.d-5
    REAL*8:: RAICES(NRAICES,2) ! Raíces de la ecuación
    INTEGER:: UNIDAD ! Número de unidad para archivos de salida
    INTEGER:: I,J,K
    REAL*8:: Z(2),RAIZ(2) ! Aproximación inicial y solución
    REAL*8:: X0,Y0 ! Parte real e imaginaria de la aprox. inicial
    REAL*8 R,SUMI,SUMJ ! Número al azar entre 0 y 1
    CHARACTER(7):: ARCHIVO
    CHARACTER(2):: NUMRAIZ
    LOGICAL:: SEGUIR
    REAL*8 :: d,b,c
    COMPLEX*8::ZC,RAIZC,RAICESC(3)
    
    !Procedemos a introducir las raices manualmente para con ello analizar a cual de ellas converge el sistema

    d=12.d0**(1/3.d0)
    b=-1.d0/2.d0
    c=(3.d0**(1.d0/2.d0))/2.d0
    
    RAICESC(1)=COMPLEX(d,0)
    RAICESC(2)=COMPLEX(d*b,d*c)
    RAICESC(3)=COMPLEX(d*b,-d*c)

    !Abrimos los archivos donde guardaremos que puntos convergen a que raiz

    do i=1,3
        write(*,*) RAICESC(I)
    enddo
    DO J=1,NRAICES
    UNIDAD = 10+J
    WRITE(NUMRAIZ,'(I2.2)') J
    ARCHIVO = 'raiz-' // NUMRAIZ
    OPEN(UNIDAD,FILE=ARCHIVO)
    ENDDO

    !Para conseguir un mayor numero de puntos procedemos a introducirlos aleatoriamonta, generando una semilla, y despues tomando valores de la musma
    
    !CALL RANDOM_SEED()
    
    x0=-A
    DO I = 1,2*NPUNTOS
    X0=X0+A*(1.d0/REAL(NPUNTOS,8))
    Y0=-A
    DO K = 1,2*NPUNTOS
    Y0=Y0+A*(1.d0/REAL(NPUNTOS,8))
    !Procedemos a establecer unos límites de los puntos para ello multiplicamos el valor máximo que queremos alcanzar por 2 
    !y por el numero generado y le restamos el numero maximo para con ello obtener los negativos


    !Call RANDOM_NUMBER(R)
    !X0=2.d0*A*R-A

    !CALL RANDOM_NUMBER(R)
    !Y0=2.d0*A*R-A


    Z = (/X0,Y0/)

    !Utilizamos los numeros obtenidos aleatoriamente para ver a cual de las raices converge mediante el método de newton raphson

    CALL NEWTON_RAPHSON(Z,RAIZ,TOL)

    !Para facilitar la asignación transformamos todos los valores a numeros complejos
    ZC=COMPLEX(x0,Y0)
    RAIZC=COMPLEX(RAIZ(1),RAIZ(2))

    !write(*,*) ZC,RAIZC

    !Colocamos los valores en los archivos segun a que raiz convergen

    J = 1
    SEGUIR = .TRUE.
    DO WHILE(SEGUIR.EQV..TRUE.)
    IF (ABS(RAIZC-RAICESC(J))<=TOL) THEN
    UNIDAD = 10 + J
    WRITE (UNIDAD,*) X0, Y0
    SEGUIR = .FALSE.
    ENDIF
    J = J + 1
    if (J>NRAICES) EXIT
    ENDDO
    
    END DO
    END DO

    !Cerramos los archivos

    DO J=1,NRAICES
    UNIDAD = 10+J
    CLOSE(UNIDAD)
    END DO
    STOP
end program sistemasnolineales