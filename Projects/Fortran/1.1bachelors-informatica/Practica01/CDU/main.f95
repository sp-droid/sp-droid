program CDU

implicit none

    integer :: c,d,u,n
    c = 0
    d = 0
    u = 0

    call system("cls")

    write (*,*) "Introduzca un numero entero de 3 cifras"
    read (*,*) n

    c = n/100
    d = n/10-c*10
    u = n-c*100-d*10

    write (*,*) "centenas" ,c
    write (*,*) "decenas " ,d
    write (*,*) "unidades" ,u


end program CDU