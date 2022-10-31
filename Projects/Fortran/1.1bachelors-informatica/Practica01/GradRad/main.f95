program gradrad

implicit none

REAL    ::  rad, g, m, s, pi

pi = ACOS(-1.d0)
call system("cls")


write(*,*) "Introduzca angulo en sexagesimal (grados minutos segundos)"
read(*,*) g, m, s

rad = (s/360+m/60+g)/360*2


write(*,*)"Angulo en sexagesimal" , g,m,s
write(*,*)"Angulo en radianes", rad,"*pi   =",rad*pi

end program gradrad