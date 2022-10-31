program latlong

implicit none

REAL    ::  lat1G, lat1M, lat1S, lg1G, lg1M, lg1S    !Variables de la primera ubicación
REAL    ::  lat2G, lat2M, lat2S, lg2G, lg2M, lg2S    !Variables de la segunda ubicación
REAL    ::  diflg, phi, radioT, a                    !Variables intermedias y a
REAL,parameter    ::  pi = ACOS(-1.d0)
character(3)   :: respuesta



call system("cls")

write(*,*)"Calculadora de distancias en la superficie de un planeta"

write(*,*)"Es su planeta la Tierra? Si/No"
read(*,*) respuesta

if(((respuesta=="Si").OR.(respuesta=="si")).OR.(respuesta=="SI")) then
    radioT = 6371
else if(((respuesta=="no").OR.(respuesta=="No")).OR.(respuesta=="NO")) then
    write(*,*)"Introduzca entonces el radio del planeta (en kilometros)"
    read(*,*) radioT
else
    STOP "Respuesta no valida"
end if


write(*,*)"En grados sexagesimales (grados minutos segundos):"

write(*,*)"Introduzca la latitud de la primera ubicacion"
read(*,*) lat1G, lat1M, lat1S
write(*,*)"Introduzca la longitud de la primera ubicacion"
read(*,*) lg1G, lg1M, lg1S

write(*,*)"Introduzca la latitud de la segunda ubicacion"
read(*,*) lat2G, lat2M, lat2S
write(*,*)"Introduzca la longitud de la segunda ubicacion"
read(*,*) lg2G, lg2M, lg2S


! Pasar los minutos y segundos a grados, y sumarlos a los grados totales
! 1ªUb
lat1G = (lat1G+lat1M/60+lat1S/3600)
lg1G = (lg1G+lg1M/60+lg1S/3600)

!2ªUb
lat2G = (lat2G+lat2M/60+lat2S/3600)
lg2G = (lg2G+lg2M/60+lg2S/3600)

!Diferencia de longitudes
diflg = abs(lg1G-lg2G)
!Cálculo del ángulo que definen los vectores de posición de la 1ª y 2ª ubicación
phi = ACOS(COS(lat1G*2*pi/360)*COS(lat2G*2*pi/360)*COS(diflg*2*pi/360)+SIN(lat1G*2*pi/360)*SIN(lat2G*2*pi/360))
!Cálculo final
a = radioT*phi

call system("cls")

write(*,*)"Primer punto:    Latitud",lat1G,"grados Norte            Longitud",lg1G,"grados Oeste"
write(*,*)"Segundo punto:   Latitud",lat2G,"grados Norte            Longitud",lg2G,"grados Oeste"
write(*,*)"Distancia en kilometros", a

end program latlong