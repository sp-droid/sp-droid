module funciones

contains

REAL*8 function f(x)
    real*8, intent(inout)  ::  x

    REAL*8,parameter    ::  pi = ACOS(-1.d0)

    if(x<-pi) then
    f = -(x+pi)/(x**2+1)

    else if(x>pi) then
    f = (x-pi)/(x**2+1)

    else
    f = x*sin(x)
    endif
    
end function f







end module