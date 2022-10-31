module integracion
use funciones

implicit none

contains

!Función que llama a uno u otro método y dota de valor a h y n según los parámetros escogidos
function intervalR(a,b,h,n,metodo,f1)
    REAL*8       ::  a, b, h, intervalR
    integer      ::  n, metodo

	interface 
		function f1(x)
           REAL*8 :: x, f1
		end function 
    end interface 
	
    if(n==0) then
        n = int((b-a)/h)
        h = (b-a)/n
    elseif(h<EPSILON(1.d0)) then
        h = (b-a)/n
    endif

    if(metodo==0)   then  !Constante, Riemann
        intervalR = integRconstante(a,h,n,f1)
    elseif(metodo==1) then  !Lineal, Trapecio
        intervalR = integRlineal(a,h,n,f1)
    elseif(metodo==2) then  !Parabólico, Simpson
        !intervalR = 
    endif
    
end function

!Integral en R de orden 0 (rectángulos)
function integRconstante(a,h,n,f1)
    REAL*8      ::  a, h, integRconstante
    integer     ::  n

    integer     ::  i
    REAL*8      ::  x, suma
	
	interface 
		function f1(x)
           REAL*8 :: x, f1
		end function 
    end interface 

    suma = 0

    do i = 0,n-1
        x = a+i*h
        suma = suma + h*fintR(x)
    enddo
    integRconstante = suma

end function

!Integral en R de orden 1 (trapecios)
function integRlineal(a,h,n,f1)
    REAL*8      ::  a, h, integRlineal
    integer     ::  n

    integer     ::  i
    REAL*8      ::  x, suma

	interface 
		function f1(x)
           REAL*8 :: x, f1
		end function 
    end interface 
	
    suma = 0

    do i = 0,n-1
        x = a+i*h
        suma = suma + h*(fintR(x)+fintR(x+h))/2
    enddo
    integRlineal = suma

end function

!Método explícito para calcular puntos que cumplen una ecuación diferencial (que relaciona una funcion con sus derivadas) + 1 condición inicial
function meteuler(a,b,n,u0,f1)
    integer  ::  n
    REAL*8   ::  a, b, u0, meteuler(2,n+1)

    integer     ::  i
    REAL*8      ::  h

    interface 
		function f1(x)
           REAL*8 :: x(2), f1
		end function 
    end interface 

    h = (b-a)/n
    meteuler(1,1) = a
    meteuler(2,1) = u0
    
    do i = 1,n
        meteuler(1,i+1) = a+i*h
        meteuler(2,i+1) = meteuler(2,i)+f1(meteuler(:,i))*h
    enddo

end function

end module integracion