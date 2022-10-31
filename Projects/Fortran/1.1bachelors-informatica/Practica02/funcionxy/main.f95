program funcionxy

    implicit none

    real*8  ::  a, b, ft
    real*8, external    ::  f

    a = 0
    b = 0

    write(*,*) "Funcion f segun los valores x e y, introduzca los 2 valores"
    read (*,*) a, b
    ft = f(a,b)
   
    write(*,*) "Valor de la funcion para: x=",a ,"y=",b
    write(*,*) ft


end program funcionxy


!Definición de la función f(x,y)
real*8 function f(x,y)
    implicit none

    real*8, intent(in)  ::  x,y

    if(x<y) then
        if(x<-2) then
            f = (SIN(x))**2                             !RECORDATORIO, fortran trabaja con radianes de forma predeterminada
        else if((-2<=x).AND.(x<2)) then
            f = (x**2+y**2)**(1./2.)
        else if((2<=x).AND.(x<=4)) then
            f = x/(2*y)
        else
            f = 7*x**(4./3.)
        end if
    else if((x>y).AND.(y>-7.0)) then
        if(x<-2) then
            f = y-x
        else if(((-2<=x).AND.(x<2)).AND.(y/=0)) then
            f = x/y
        else if(((-2<=x).AND.(x<2)).AND.(y==0)) then
            f = 0
        else
            f = abs(y)
        end if
    else
        f = 7*x**3+2*x**2-x+5
    end if
end function f