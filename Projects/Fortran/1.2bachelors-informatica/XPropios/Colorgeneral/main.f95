program colorgeneral
implicit none
!Este programa cambia el color de todas las letras y el fondo de la consola

character(8 )  command
      command(1:8 )='color 07' !El primer número es para el fondo, el segundo para las letras
      CALL SYSTEM(COMMAND)


! 0 = Black       8 = Gray
! 1 = Blue        9 = Light Blue
! 2 = Green       A = Light Green
! 3 = Aqua        B = Light Aqua
! 4 = Red         C = Light Red
! 5 = Purple      D = Light Purple
! 6 = Yellow      E = Light Yellow
! 7 = White       F = Bright White

end program colorgeneral