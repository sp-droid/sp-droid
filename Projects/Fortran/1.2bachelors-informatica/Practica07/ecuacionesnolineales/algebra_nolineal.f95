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


end module