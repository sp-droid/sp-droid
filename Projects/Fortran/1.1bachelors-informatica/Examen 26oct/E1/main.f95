program raices
implicit none
COMPLEX     ::  a, b, c, x1, x2

write(*,*) "Introduzca a, b, c"
read(*,*) a, b, c

x1 = (-b+(b**2+(-4,0)*a*c)**(1./2.))/(2.*a)
x2 = (-b-(b**2+(-4,0)*a*c)**(1./2.))/(2.*a)

write(*,*) "x1:", x1, "x2:", x2


end program raices