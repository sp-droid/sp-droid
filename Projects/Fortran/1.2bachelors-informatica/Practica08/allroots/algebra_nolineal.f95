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

subroutine all_roots1D(sol,numraices,tol1,f1)
    REAL*8,intent(inout)    ::  sol(:), tol1
    integer,intent(in)      ::  numraices

    integer     ::  i, cont, par, max_interval
    REAL*8      ::  a, b, tol2, xaprox

    interface
        function f1(x)
            REAL*8  ::  f1, x
        end function
    end interface

    max_interval = 6000
    i = 0
    par = 0
    cont = 0
    tol2 = 0.5d0

    do while(i<max_interval)
        if(par==0) then
            a = i+0.01
            b = i+1
            par = 1
        else
            a = -i-0.01
            b = -i-1
            i = i+1
            par = 0
        endif

        if(f1(a)*f1(b)<0.OR.abs(f1(a)*f1(b))<EPSILON(1.d0)) then
            call biseccion(xaprox,a,b,tol2,f1)
            cont = cont+1
            call newton_raphson1D(xaprox,sol(cont),tol1,f1)
        endif
        if(cont==numraices) EXIT
    enddo

end subroutine

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

    max_iter = 1000000  
    fallo=1

    do while(fallo/=0)
        write(*,*)'Introduce el intervalo [x1,x2] donde se desea buscar la solucion de la ecuacion'
        read(*,*) a, b
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

subroutine biseccion(sol,a,b,tol,f1)
    real*8, intent(inout) :: sol, tol, a, b
    
    integer :: i, max_iter
    real*8 :: x0

    interface 
       function f1(x)
           real*8 :: x, f1
       end function 
    end interface 
    
    max_iter=1000000

    if (f1(a)*f1(b)>0.AND.f1(a)*f1(b)>EPSILON(1.d0)) then
        write (*,*) 'Intervalo incorrecto, en este invervalo la funcion no cambia de signo'
        write (*,*) 'Por favor, introduce un intervalo valido'
    endif
    
    if (f1(a)==0) then
        x0=a
    elseif (f1(b)==0) then
        x0=b 
    else
        do i=1, max_iter
            x0=(b+a)/2.d0 
            if (abs(f1(x0))<tol) EXIT 
            if ((f1(x0)*f1(a)<0))b=x0
            if ((f1(x0))*f1(b)<0) a=x0
        enddo   
    endif
    sol=x0

end subroutine

end module