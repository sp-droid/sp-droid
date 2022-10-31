program AyVolEsfera

implicit none

    real    ::  radio, area, volumen, pi

    call system("cls")

    radio = 0
    area = 0   
    volumen = 0
    pi = acos(-1.d0)


    write (*,*) "Introduzca un radio"
    read (*,*) radio

    area = 4.*pi*radio**2
    volumen = (4./3.)*pi*radio**3

    write (*,*) "Radio de la esfera" ,radio
    write (*,*) "Area de la esfera" ,area
    write (*,*) "Volumen de la esfera" ,volumen


end program AyVolEsfera