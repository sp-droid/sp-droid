program hundirlaflota

use generador

implicit none

!Declaramos variables
integer     ::  i, j, horz, vert, sumat, cont, can, coordx, coordy
integer,parameter   :: n=10
integer     ::  columnas(n)
character   ::  coordA, agua, barco, dmgbarco
character   ::  tjug(n,n), tprog(0:n+1,0:n+1), filas(n)

character(10)   ::   respuesta

!Inicializamos variables (los caracteres podría haberlos definido en el momento como hice con #, pero he decidido dejarlo así)
agua = "~"
barco = "/"
dmgbarco = "$"

can = 0
tprog = "~"
tjug = "~"

!Partida nueva o continuar donde lo dejamos?
write(*,*) "Escriba Continuar para jugar (tablero personalizado manualmente) o cualquier otra cosa para una &
&nueva partida(tablero aleatorio)"
read(*,*) respuesta
if(respuesta/="Continuar".AND.respuesta/="continuar") then
    call limptablr(n)
    call randogame(n)
endif
!Informamos de las letras que usaremos para representar cada cosa
write(*,*)
write(*,*) "Bienvenido a Hundir la flota! Debajo aparecera el significado de cada simbolo que empleamos."
write(*,*) "Recuerde ademas que es posible abandonar la partida introduciendo una coordenada incorrecta."
write(*,*) "Cuando este listo pulse cualquier tecla para empezar, buena suerte!"
write(*,*)
write(*,*) "~-->Agua    $-->Parte tocada de un barco    #-->Barco destruido     X-->Disparos fallidos"
read(*,*)

!Bucle principal
do
    !Al principio pedimos las coordenadas del disparo
    write(*,*) "Escriba las coordenadas (e.g.: A 8) del disparo"
    read(*,*) coordA, coordx
    !Limpiamos pantalla
    call system("cls")
    !Contador de disparos
    can = can + 1 
    !Leemos la situación actual del juego
    open(unit=10,file="auxiliares/Tablero.txt")
    read(10,*) columnas
    read(10,*)
    do i = 1,n
        read(10,fmt="(1X,A1,8X,12(A1,3X))") filas(i), tprog(i,1:n)
    enddo
    close(10)

    open(unit=10,file="auxiliares/Partida.txt")
    read(10,*) columnas
    read(10,*)
    do i = 1,n
        read(10,fmt="(1X,A1,8X,12(A1,3X))") filas(i), tjug(i,:)
    enddo
    close(10)

    !Pasamos la coordy de carácter a número comparando la letra con la posición de ésta en la matriz filas, e.g.: C-->3
    do i = 1,n
        if(coordA==filas(i)) then
            coordy = i
            EXIT
        endif
    enddo

    !!!Condicionales para los distintos casos
    if(tprog(coordy,coordx)==agua) then
        sumat = 0
        do i = -1,1
            do j = -1,1
                if(tprog(coordy+i,coordx+j)==barco) then
                    sumat = sumat + 1
                endif
            enddo
        enddo
        if(sumat/=0) then
            write(*,*) "CASI!"
        else
            write(*,*) "AGUA!"
            do i = -1,1         !Pequeño bucle para que en la matriz del jugador se actualicen las 8 casillas circundantes como agua
                do j = -1,1
                    if((i+coordy/=0.AND.i+coordy/=n+1).AND.(j+coordx/=0.AND.j+coordx/=n+1)) then
                        if(tjug(coordy+i,coordx+j)=="-") then
                            tjug(coordy+i,coordx+j)="~"
                        endif
                    endif
                enddo
            enddo
        endif
        tjug(coordy,coordx)="X"

    elseif(tprog(coordy,coordx)==barco) then
        !Averiguamos la orientación del barco
        vert = 0
        horz = 0
        do i = -1,1,2
            if(tprog(coordy+i,coordx)==barco.OR.tprog(coordy+i,coordx)==dmgbarco) vert = 1
        enddo
        do i = -1,1,2
            if(tprog(coordy,coordx+i)==barco.OR.tprog(coordy,coordx+i)==dmgbarco) horz = 1
        enddo
        
        !Ahora vemos cuántas partes le quedaban al barco antes del impacto
        sumat = 1
        do i = 1,4
            if(tprog(coordy+i*(vert),coordx+i*horz)==agua.OR.(horz+vert)==0) EXIT
            if(tprog(coordy+i*(vert),coordx+i*horz)==barco) sumat = sumat+1
        enddo
        do i = -1,-4,-1
            if(tprog(coordy+i*(vert),coordx+i*horz)==agua.OR.(horz+vert)==0) EXIT
            if(tprog(coordy+i*(vert),coordx+i*horz)==barco) sumat = sumat+1
        enddo

        if(sumat==1) then
            write(*,*) "TOCADO Y HUNDIDO!"
            tprog(coordy,coordx)="#"
            tjug(coordy,coordx)="#"

            !Actualizamos todo el barco para que se vea como destruido en la matriz del jugador, en la otra da igual
            do i = 0,4
                if(tprog(coordy+i*(vert),coordx+i*horz)==agua.OR.(horz+vert)==0) EXIT
                if(tprog(coordy+i*(vert),coordx+i*horz)/=agua) tjug(coordy+i*(vert),coordx+i*horz)="#"
            enddo
            do i = -1,-4,-1
                if(tprog(coordy+i*(vert),coordx+i*horz)==agua.OR.(horz+vert)==0) EXIT
                if(tprog(coordy+i*(vert),coordx+i*horz)/=agua) tjug(coordy+i*(vert),coordx+i*horz)="#"
            enddo
        else
            write(*,*) "TOCADO!"
            tprog(coordy,coordx)=dmgbarco
            tjug(coordy,coordx)=dmgbarco
        endif
    endif

    !Actualizamos los ficheros correspondientes a cada matriz
    open(unit=10,file="auxiliares/Tablero.txt")
    write(10,fmt="(9X,9(I2,2X),1X,3(I2,2X))") columnas
    write(10,*)
    do i = 1,n
        write(10,fmt="(1X,A1,8X,12(A1,3X))") filas(i), tprog(i,1:n)
    enddo
    close(10)

    open(unit=10,file="auxiliares/Partida.txt")
    write(10,fmt="(9X,9(I2,2X),1X,3(I2,2X))") columnas
    write(10,*)
    do i = 1,n
        write(10,fmt="(1X,A1,8X,12(A1,3X))") filas(i), tjug(i,:)
    enddo
    close(10)

    !Interfaz gráfica
    write(*,*) 
    write(*,fmt="(5X,9(4X,I2),1X,3(4X,I2))") columnas
    write(*,*)                                         
    do i = 1,n
        write(*,fmt="(1X,A1,8X,12(A1,5X))") filas(i), tjug(i,:)
        write(*,*)
    enddo

    !Si ya no hay más barcos el jugador ha ganado, el contador de barcos estará a 0 y el de cañonazos en el número que esté
    cont = 0
    do i=1,n
        do j=1,n
            if(tprog(i,j)==barco) then
                cont = cont + 1
            endif
        enddo
    enddo
    if(cont==0) then
        EXIT
    endif
enddo

write(*,*) "Enhorabuena! Ha conseguido derrotar a todas las naves enemigas. Para ello ha necesitado:"
write(*,*) can,"disparos"
write(*,*)
write(*,*) "Presione cualquier tecla para salir..."
read(*,*)

end program hundirlaflota