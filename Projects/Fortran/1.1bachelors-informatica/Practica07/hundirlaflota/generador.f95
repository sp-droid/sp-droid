module generador
contains

subroutine limptablr(nsize)
    integer,intent(in)  ::  nsize

    integer             :: i
    character           :: agua, letra
    character           :: tablero(nsize,nsize)
    character           :: alfabeto(nsize)
    character(50)       :: formato1, formato2
    character(2)        :: n_c

    agua = '-'
    Tablero = agua

!Lo quiero hacer con letras en las columnas, necesito por ello una base de datos de la que extraerlas, con esto el tablero
!se complica un poco al subir de 26 letras sin contar la ñ asi que si n es mayor de 26 tendría que hacer cambios
    open(unit = 10,file = "Auxiliares/Alfabeto.txt")
        do i = 1,nsize
            read(10,*) letra
            alfabeto(i) = letra
        enddo
    close(10)

    write(n_c,fmt='(I2)') nsize 
    formato1 = '(7x,'//trim(n_c)//'i4'//')'
    formato2 = '(A2,5x,'//trim(n_c)//'a4'//')'

    open(unit = 10, file = 'Auxiliares/Tablero.txt')
    write(10,fmt=formato1)(i, i=1,nsize)
    write(10,*)
    do i = 1,nsize
        write(10,fmt=formato2) alfabeto(i),Tablero(i,:)
enddo
    write(10,fmt="(I2)") nsize
    close(10)

    open(unit = 10, file = 'Auxiliares/Partida.txt')
    write(10,fmt=formato1)(i, i=1,nsize)
    write(10,*)
    do i = 1,nsize
        write(10,fmt=formato2) alfabeto(i),Tablero(i,:)
    enddo
    write(10,fmt="(I2)") nsize
    close(10)

end subroutine


subroutine randogame(nsize)
    integer,intent(in)  ::  nsize

    REAL    ::  random
    integer ::  tme, numbarcos, i, barcsize, j, x, y, horver, outt
    character   ::  tablero(0:nsize+1,0:nsize+1)
    character   ::  alfabeto(nsize)

    tablero = "%"
    outt = 0
    do i = 1,nsize
        do j = 1,nsize
            tablero(i,j) = "-"
        enddo
    enddo

    !Usamos el reloj del sistema para obtener una seed y de ella una cadena de números aleatorios del 0 al
    !1, multiplicaremos por otro número y convertiremos el número a entero para poder trabajar bien con él
    call SYSTEM_CLOCK(tme)
    random = RAND(tme)

    open(unit = 10,file = "auxiliares/Alfabeto.txt")
    do i = 1,nsize
        read(10,*) alfabeto(i)
    enddo
    close(10)

    !Fichero del tablero(jugador)
    open(unit=10,file="auxiliares/Partida.txt")
    read(10,*)
    write(10,*)
    do i = 1,nsize
        write(10,fmt="(1X,A1,8X,12(A1,3X))") alfabeto(i), tablero(i,1:nsize)
    enddo
    close(10)

    do i = 1,nsize
        do j = 1,nsize
            tablero(i,j) = "~"
        enddo
    enddo


    numbarcos = int(random*6)
    if(numbarcos<2) then
        numbarcos = numbarcos + 3
    endif

    i = 0
    do
        i = i + 1
        if(outt==0) then
            random = RAND(0)
            barcsize = int(random*8)
            if(barcsize==0) then
                barcsize = barcsize + 1
            endif
        elseif(outt==1) then
            outt = 0
        endif

        random = RAND(0)
        x = int(random*nsize)
        random = RAND(0)
        y = int(random*nsize)
        random = RAND(0)
        horver = int(random*100)

        do j = 0,barcsize-1
            if(horver>=50) then
                if(tablero(x+j,y)=="%") then
                    outt = 1
                    i = i - 1
                    EXIT
                endif
                tablero(x+j,y) = "/"
            else
                if(tablero(x,y+j)=="%") then
                    outt = 1
                    i = i - 1
                    EXIT
                endif
                tablero(x,y+j) = "/"
            endif
        enddo

        if(i>=numbarcos) then
            EXIT
        endif
    enddo

    !Bucles para arreglar barcos pegados
    do i = 1,nsize
        do j = 1,nsize
            if(tablero(i,j)=="/") outt = outt+1
            if(outt==5) then
                tablero(i,j) = "~"
                outt = 0
            endif
        enddo
        do j = 1,nsize
            if(tablero(j,i)=="/") outt = outt+1
            if(outt==5) then
                tablero(j,i) = "~"
                outt = 0
            endif
        enddo
    enddo
    do i = 1,nsize
        do j = 1,nsize
            outt = 0
            horver = 0
            if(tablero(i+1,j)=="/") outt=outt+1
            if(tablero(i-1,j)=="/") outt=outt+1
            if(tablero(i,j+1)=="/") horver=horver+1
            if(tablero(i,j-1)=="/") horver=horver+1
            if(outt>=1.AND.horver>=1) tablero(i,j)="~"
        enddo
    enddo

    !Fichero del tablero(programa)
    open(unit=10,file="auxiliares/Tablero.txt")
    read(10,*)
    write(10,*)
    do i = 1,nsize
        write(10,fmt="(1X,A1,8X,12(A1,3X))") alfabeto(i), tablero(i,1:nsize)
    enddo
    close(10)


end subroutine randogame

end module