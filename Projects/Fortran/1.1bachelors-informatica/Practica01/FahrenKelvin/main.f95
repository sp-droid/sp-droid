program FahrenKelvin

implicit none

    REAL    ::  tk, tf
    tk = 0
    tf = 0

    call system("cls")

    write (*,*) "Introduzca la temperatura a convertir de grados fahrenheit a kelvin"
    read(*,*) tf

    tk = (5./9.)*(tf-32)+273

    write (*,*) "Temperatura en grados fahrenheit" ,tf
    write (*,*) "Temperatura en grados kelvin" ,tk



end program FahrenKelvin