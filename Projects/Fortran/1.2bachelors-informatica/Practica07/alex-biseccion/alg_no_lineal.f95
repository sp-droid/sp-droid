module algebra_nolineal
use funciones
use derivacion

contains

subroutine newton_raphson1D(x0,sol,tol)
    REAL*8,intent(inout)    ::  x0, sol, tol

    integer     ::  i, max_iter
    REAL*8      ::  f1x, fx1, h

    max_iter = 100000
    h = 0.05d0

    do i = 1,max_iter
        if(abs(ecuaciones(x0))<tol) EXIT
        f1x = ecuaciones(x0-h)
        fx1 = ecuaciones(x0+h)
        x0 = x0 -ecuaciones(x0)/derivada1_centradaO2(f1x,fx1,h)
    enddo
    sol = x0

end subroutine


subroutine regula_FALSI(x0,sol,tol)
    REAL*8,intent(inout)    ::  x0, sol, tol

    integer     ::  i, max_iter, fallo!no le llamo error aposta
    REAL*8      ::  f1x, fx1
    REAL*8      ::  a, b

    max_iter = 100000000    
    fallo=1


    do while(fallo/=0)
        write(*,*)'Introduce el intervalo [x1,x2] donde se desea buscar la solucion de la ecuacion'
        read(*,*)a,b

        !valores de la funcion (x^3 +4x^2  +10), en los puntos iniciales
        f1x=(a**3.d0)+4.d0*(a**2.d0)-10.d0              !|
        fx1=(b**3.d0)+4.d0*(b**2.d0)-10.d0              !| Esto hay que cambiarlo segun la funcion

        if(f1x*fx1>0)then
            write(*,*)'los valores de la funcion en [a,b] tienen igual signo'
        else
            fallo=0
        endif
    enddo

    do i = 1,max_iter
        x0= a - f1x*((b-a)/(fx1-f1x))
        valor_funcion= (x0**3.d0)+4.d0*(x0**2.d0)-10     !| Esto hay que cambiarlo segun la funcion
        if(ABS(valor_funcion)<tol) EXIT

        if(valor_funcion*f1x<0)then     !el nuevo punto lo sustituimos por b
            fx1=valor_funcion
        else                            !el nuevo punto lo sustituimos por a
            f1x=valor_funcion
        endif
    enddo

    sol = x0

end subroutine

subroutine biseccion(x0,sol,tol,funcion3)
    real*8, intent(inout) :: x0, sol, tol
    
    integer :: i, max_iter, fallo
    real*8 :: a,b


    interface 
       function funcion3(x)
           real*8 :: x 
           real*8 :: funcion3
          
       end function 
    
    end interface 
    
    max_iter=100000000
    fallo=1

    do while(fallo/=0)
        write(*,*) 'Introduce el intervalo dentro del que se encuentra la solucion a la ecuacion'
        read(*,*) a,b
        if (funcion3(a)*funcion3(b)>0) then
           write (*,*) 'Intervalo incorrecto, en este invervalo la funcion no cambia de signo'
           write (*,*) 'Por favor, introduce un intervalo valido'
        else
            fallo=0
        endif
    enddo
    
    if (funcion3(a)==0) then
        x0=a
 
    elseif (funcion3(b)==0) then
        x0=b 
         
    else
        do i=1, max_iter
        
        x0=(b+a)/2.d0 
        if (abs(funcion3(x0))<tol) EXIT 

        if ((funcion3(x0)*funcion3(a)<0))b=x0
        if ((funcion3(x0))*funcion3(b)<0) a=x0
    
        enddo 
            
    endif
    
    sol=x0

end subroutine

end module