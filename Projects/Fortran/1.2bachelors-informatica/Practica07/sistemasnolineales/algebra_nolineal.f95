module algebra_nolineal
use funciones
use derivacion
use algebra_lineal

contains

subroutine newton_raphson1D(x0,sol,tol,f1)
    REAL*8,intent(inout)    ::  x0, sol, tol

    integer     ::  i, max_iter
    REAL*8      ::  f1x, fx1, h

    interface
        function f1(x)
            REAL*8  ::  f1, x
        end function
    end interface

    max_iter = 100000
    h = 0.01d0

    do i = 1,max_iter
        if(abs(f1(x0))<tol) EXIT
        f1x = f1(x0-h)
        fx1 = f1(x0+h)
        x0 = x0 -f1(x0)/derivada1_centradaO2(f1x,fx1,h)
    enddo
    sol = x0

end subroutine

subroutine newton_raphson(X0,SOL,tol,f1)
    REAL*8,intent(inout)    ::  X0(:), SOL(:), tol

    integer     ::  max_iter, n, i
    REAL*8,allocatable      :: JFA(:,:), Y(:)
	
	interface
        function f1(x,n)
            integer	::	n
			REAL*8	::	f1(n), x(n)
        end function
    end interface
    
    n = size(X0,1)
    max_iter = 100000
    allocate(JFA(n,n+1),Y(n))

    do i = 1,max_iter
        if(norma2(f1(X0,n))<tol) EXIT
        JFA(:,1:n) = jacob(X0,f1,n)
        JFA(:,n+1) = f1(X0,n)
        call metgauss(JFA,Y)
        X0 = X0 - Y
    enddo
    SOL = X0
    
end subroutine newton_raphson

subroutine regula_FALSI(x0,sol,tol,f1)
    REAL*8,intent(inout)    ::  x0, sol, tol

    integer     ::  i, max_iter, fallo!no le llamo error aposta
    REAL*8      ::  f1x, fx1, valor_funcion
    REAL*8      ::  a, b

    interface
        function f1(x)
            REAL*8  ::  f1, x
        end function
    end interface

    max_iter = 100000000    
    fallo=1

    do while(fallo/=0)
        write(*,*)'Introduce el intervalo [x1,x2] donde se desea buscar la solucion de la ecuacion'
        read(*,*)a,b
        f1x=f1(a)
        fx1=f1(b)

        if(f1x*fx1>0)then
            write(*,*)'los valores de la funcion en [a,b] tienen igual signo'
        else
            fallo=0
        endif
    enddo

    do i = 1,max_iter
        x0= a - f1x*((b-a)/(fx1-f1x))
        valor_funcion= f1(x0)    !| Esto hay que cambiarlo segun la funcion
        if(ABS(valor_funcion)<tol) EXIT

        if(valor_funcion*f1x<0)then     !el nuevo punto lo sustituimos por b
            fx1=valor_funcion
        else                            !el nuevo punto lo sustituimos por a
            f1x=valor_funcion
        endif
    enddo

    sol = x0

end subroutine

subroutine biseccion(x0,sol,tol,ecuaciones)
    real*8, intent(inout) :: x0, sol, tol
    
    integer :: i, max_iter, fallo
    real*8 :: a,b

    interface 
       function ecuaciones(x)
           real*8 :: x 
           real*8 :: ecuaciones
          
       end function 
    
    end interface 
    
    max_iter=100000000
    fallo=1

    do while(fallo/=0)
        write(*,*) 'Introduce el intervalo dentro del que se encuentra la solucion a la ecuacion'
        read(*,*) a,b
        if (ecuaciones(a)*ecuaciones(b)>0) then
           write (*,*) 'Intervalo incorrecto, en este invervalo la funcion no cambia de signo'
           write (*,*) 'Por favor, introduce un intervalo valido'
        else
            fallo=0
        endif
    enddo
    
    if (ecuaciones(a)==0) then
        x0=a
 
    elseif (ecuaciones(b)==0) then
        x0=b 
         
    else
        do i=1, max_iter
        
        x0=(b+a)/2.d0 
        if (abs(ecuaciones(x0))<tol) EXIT 

        if ((ecuaciones(x0)*ecuaciones(a)<0))b=x0
        if ((ecuaciones(x0))*ecuaciones(b)<0) a=x0
    
        enddo 
            
    endif
    
    sol=x0

end subroutine

end module