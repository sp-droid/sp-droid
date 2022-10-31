module algebra_nolineal
use funciones
use algebra_lineal
use derivacion

contains

subroutine newton_raphsonmod(X0,SOL,Yi,tol,h,m,f1)
    integer,intent(in)      ::  m
    REAL*8,intent(inout)    ::  X0(m+1), SOL(m), Yi(m), tol, h

    integer     ::  max_iter, i
    REAL*8,allocatable      :: JFA(:,:), Y(:)
	
    
    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface

    X0(2:m+1) = Yi
    max_iter = 100000
    allocate(JFA(m,m+1),Y(m))

    do i = 1,max_iter
        if(norma2(ecuacion(X0,Yi,h,m,f1))<tol) EXIT
        JFA(:,1:m) = jacobmod(X0,Yi,h,m,f1)
        JFA(:,m+1) = ecuacion(X0,Yi,h,m,f1)
        call metgauss(JFA,Y)
        X0(2:m+1) = X0(2:m+1) - Y
    enddo
    SOL = X0(2:m+1)
    write(*,*) i, "numero de iteraciones necesarias"
    
end subroutine

function jacobmod(X0,Yi,hx,m,f1) 
    integer ::  m
    REAL*8  ::  jacobmod(m,m), X0(m+1), Yi(m), hx

    integer     ::  i, j
    REAL*8      ::  h
    REAL*8,allocatable  ::  Xceteris(:), fant(:), fpost(:)

    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface

    h = 0.01d0
    allocate(Xceteris(m+1),fant(m),fpost(m))

    ! 0= Yi+hx*f1(X0,m)-X0(2:m+1)
    do j = 1,m
        Xceteris = X0
        Xceteris(j+1) = Xceteris(j+1)-h
        fant = ecuacion(Xceteris,Yi,hx,m,f1)
        Xceteris(j+1) = Xceteris(j+1)+2*h
        fpost = ecuacion(Xceteris,Yi,hx,m,f1)
        do i = 1,m
            jacobmod(i,j) = derivada1_centradaO2(fant(i),fpost(i),h)
        enddo
    enddo

end function

function ecuacion(x,yi,h,m,f1) ! 0 = Yi +hx*f1(X0,m) -X0(2:m+1)
    integer     ::  m
    REAL*8      ::  ecuacion(m), x(m+1), Yi(m), h

    REAL*8      ::  func(m)

    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface

    func = f1(x,m)
    ecuacion(1) = Yi(1)+h*func(1)-x(2)
    ecuacion(2) = Yi(2)+h*func(2)-x(3)
    ecuacion(3) = Yi(3)+h*func(3)-x(4)

end function

end module