module algebra_nolineal
use funciones_zombies
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
        if(norma2(Yi+h*f1(X0,m)-X0(2:m+1))<tol) EXIT
        JFA(:,1:m) = jacobmod(X0,Yi,h,m,f1)
        JFA(:,m+1) = Yi+h*f1(X0,m)-X0(2:m+1)
        call metgauss(JFA,Y)
        X0(2:m+1) = X0(2:m+1) - Y
    enddo
    SOL = X0(2:m+1)
    
end subroutine

function jacobmod(X0,Yi,hx,m,f1) 
    integer ::  m
    REAL*8  ::  jacobmod(m,m), X0(m+1), Yi(m), hx

    integer     ::  i, j
    REAL*8      ::  h
    REAL*8,allocatable  ::  xcet(:), fant(:), fpost(:)

    interface 
       function f1(x,m)
            integer ::  m
            REAL*8  :: x(m+1), f1(m)
       end function 
    end interface

    h = 0.001d0
    allocate(xcet(m+1),fant(m),fpost(m))

    do j = 1,m
        xcet = X0
        xcet(j+1) = xcet(j+1)-h
        fant = Yi+hx*f1(xcet,m)-xcet(2:m+1)
        xcet(j+1) = xcet(j+1)+2*h
        fpost = Yi+hx*f1(xcet,m)-xcet(2:m+1)
        do i = 1,m
            jacobmod(i,j) = derivada1_centradaO2(fant(i),fpost(i),h)
        enddo
    enddo

end function

end module