module calculo_diferencial
use algebra_lineal

contains


!****************************************************************************************************************
!***************************                        PROBLEMA T(X)                     *************************** 
!****************************************************************************************************************
subroutine T_barra_unidimensional(F_resultado,interv_serie,j,Condiciones_contorno,h)

    real*8,allocatable,intent(inout)    :: F_resultado(:)
    real*8,intent(inout)                :: Condiciones_contorno(:,:),h
    integer,intent(in)                  :: interv_serie,j
    real*8,allocatable                  :: D2(:,:),F(:)
    integer                             :: i,convergencia
    real*8                              :: a,b  !variables para el intervalo [a,b]
    real*8                              :: tam_intervalo, n_particiones
    integer                             :: n_filas
    real*8,allocatable                  :: Ampliada(:,:),R(:)
    

    !***********************************************************************INTERVALO
    write(*,*);write(*,*)'Introduce los limites del intervalo cerrado'
    read(*,*)a,b
    tam_intervalo= b-a
    
    !***********************************************************************INCREMENTO DE X
    write(*,*);write(*,*)'Introduce la distancia de las particiones, (h)'
    read(*,*)h
    n_particiones= tam_intervalo/h
    
    !***********************************************************************MATRIZ DE DERIVACION
    n_filas=n_particiones-1
    allocate(D2(n_filas,n_filas))
    D2=0
    do i=1,n_filas
        D2(i,i)=-2.d0/h**2
        if(i<n_filas)then
            D2(i,i+1)=1.d0/h**2
        endif
        if(i>1)then
            D2(i,i-1)=1.d0/h**2
        endif
    enddo
    
    !***********************************************************************VECTOR FUNCION DOBLE DERIVADA
    allocate(F(n_filas))
    F=0
    F(1)= -Condiciones_contorno(j,2)/h**2
    F(n_filas)= -Condiciones_contorno(j+1,2)/h**2
        
    
    !write(*,*);write(*,*)'LA MATRIZ DE DERIVACION ES:'
    !do i=1,n_filas
    !    write(*,*)D2(i,:)
    !enddo
    !write(*,*);write(*,*)'EL VECTOR FUNCION DERIVADA ES:'
    !write(*,*)F
    
    allocate(Ampliada(n_filas,n_filas+1));allocate(R(n_filas))
    Ampliada(:,1:n_filas)= D2
    Ampliada(:,n_filas+1)= F


    call  metodoscalor(Ampliada,R)

    allocate(F_resultado(n_filas+2))
        F_resultado(1)= -F(1)*(h**2)
        F_resultado(n_filas+2)= -F(n_filas)*(h**2)
        do i=1,n_filas
            F_resultado(i+1)=R(i)
        enddo

    deallocate(F,Ampliada,R,D2)
    
end subroutine




!metodos de resolucion del sistema del problema
subroutine metodoscalor(AB,x)
    REAL*8,intent(in)       ::  AB(:,:)
    REAL*8,intent(inout)    ::  x(:)

    integer ::  i, n
    REAL*8,allocatable  ::  A(:,:)

    n = size(AB,1)
    allocate(A(n,n))
    A(:,:) = AB(:,1:n)

    !Métodos iterativos (Gauss-seidel por ejemplo)
    write(*,*) "Resolviendo sistema por el metodo iterativo de Gauss-Seidel"    
    call metgaseidel(AB,x,i)  !Condición de convergencia --> Radio espectral estrictamente menor que 1
    if(i==0) then
        return
    endif
    write(*,*) "Problema de convergencia, intentando con LU sin pivote..."

    !Métodos directos
    call factorLU(AB,x,i)           !LU sin pivote, devuelve i = 1 si ha necesitado hacer uno
    if(i==1) then
        write(*,*) "Necesidad de pivote, resolviendo por Gauss"
        x = 0
        call metgauss(AB,x)      !Y luego por tanto resuelve el sistema por Gauss
    endif
end subroutine
    

!****************************************************************************************************************
!***************************                        DERIVACION                        *************************** 
!****************************************************************************************************************                     
function derivada1_progresivaO1(fx,fx1,h)
    REAL*8  ::  derivada1_progresivaO1, fx, h, fx1

    derivada1_progresivaO1 = (fx1-fx)/h

end function

function derivada1_progresivaO2(fx,fx1,fx2,h)
    REAL*8  ::  derivada1_progresivaO2, fx, h, fx1, fx2

    derivada1_progresivaO2 = (-fx2+4*fx1-3*fx)/(2*h)

end function

function derivada2_progresivaO1(fx,fx1,fx2,h)
    REAL*8  ::  derivada2_progresivaO1, fx, fx1, fx2, h

    derivada2_progresivaO1 = (fx2-2*fx1+fx)/(h**2)

end function

function derivada2_progresivaO2(fx,fx1,fx2,fx3,h)
    REAL*8  ::  derivada2_progresivaO2, fx, fx1, fx2, fx3, h

    derivada2_progresivaO2 = (-fx3+4*fx2-5*fx1+2*fx)/(h**2)

end function

!Regresivas
function derivada1_regresivaO1(fx,f1x,h)
    REAL*8  ::  derivada1_regresivaO1, fx, h, f1x

    derivada1_regresivaO1 = (fx-f1x)/h
end function

function derivada1_regresivaO2(fx,f1x,f2x,h)
    REAL*8  ::  derivada1_regresivaO2, fx, f1x, f2x, h

    derivada1_regresivaO2 = (f2x-4*f1x+3*fx)/(2*h)
end function

function derivada2_regresivaO1(fx,f1x,f2x,h)
    REAL*8  ::  derivada2_regresivaO1, fx, f1x, f2x, h

    derivada2_regresivaO1 = (f2x-2*f1x+fx)/(h**2)
end function

function derivada2_regresivaO2(fx,f1x,f2x,f3x,h)
    REAL*8  ::  derivada2_regresivaO2, fx, f1x, f2x, f3x, h

    derivada2_regresivaO2 = (-f3x+4*f2x-5*f1x+2*fx)/(h**2)
end function

!Centradas
function derivada1_centradaO2(f1x,fx1,h)
    REAL*8  ::  derivada1_centradaO2, f1x, fx1, h

    derivada1_centradaO2 = (fx1-f1x)/(2*h)
end function

function derivada1_centradaO4(f1x,f2x,fx1,fx2,h)
    REAL*8  ::  derivada1_centradaO4, f1x, f2x, fx1, fx2, h

    derivada1_centradaO4 = (-fx2+8*fx1-8*f1x+f2x)/(12*h)
end function

function derivada2_centradaO2(f1x,fx1,fx,h)
    REAL*8  ::  derivada2_centradaO2, f1x, fx1, fx, h

    derivada2_centradaO2 = (fx1-2*fx+f1x)/(h**2)
end function

function derivada2_centradaO4(f1x,f2x,fx,fx1,fx2,h)
    REAL*8  ::  derivada2_centradaO4, f1x, f2x, fx, fx1, fx2, h

    derivada2_centradaO4 = (-fx2+16*fx1-30*fx+16*f1x-f2x)/(12*h**2)
end function

end module