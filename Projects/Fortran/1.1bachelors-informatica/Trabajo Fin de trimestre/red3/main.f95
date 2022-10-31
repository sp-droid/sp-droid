program red3
use movimientos

implicit none

!Declaración
integer     ::  i, j, m, n, iter, vict, sumatx, sumato, time, par, cont, rot, trasp, reverse
integer     ::  pos(9), posdata(9)
character   ::  tablero(3,3)
REAL        ::  a, b, random

a = 0
b = 0
i = 0
cont = 0



!Cuerpo del programa
write(*,*) "Introduzca el numero de partidas para proceder:"
read(*,*) iter

open(unit=12,file="Estadisticas.txt")
write(12,*) cont, a/2500.*100, b/2500.*100

do
    i = i + 1
    cont = cont + 1
    tablero = "-"
    pos = 0
    vict = 0
    call SYSTEM_CLOCK(time)
    random = RAND(time)

    if(mod(i,2)==0) then
        par = 0
    else
        par = 1
    endif
    do j = 1,9
        if(par==1) then
            if(mod(j,2)/=0) call aleatorio(tablero,pos,j)
            if(mod(j,2)==0) call redprograma(tablero,pos,j,par)
        else
            if(mod(j,2)/=0) call redprograma(tablero,pos,j,par)
            if(mod(j,2)==0) call aleatorio(tablero,pos,j)
        endif

        !Vemos si alguien ha ganado
        do m = 1,3
            if(ALL(tablero(m,:)=='X').OR.ALL(tablero(m,:)=='O')) vict = 3
            if(ALL(tablero(:,m)=='X').OR.ALL(tablero(:,m)=='O')) vict = 3
        enddo
        sumatx = 0
        sumato = 0
        do m = 1,3
            if(tablero(m,m)=="X") sumatx = sumatx + 1
            if(tablero(m,m)=="O") sumato = sumato + 1
            if(sumatx==3.OR.sumato==3) vict = 3
        enddo
        sumatx = 0
        sumato = 0
        do m = 1,3
            if(tablero(m,4-m)=="X") sumatx = sumatx + 1
            if(tablero(m,4-m)=="O") sumato = sumato + 1
            if(sumatx==3.OR.sumato==3) vict = 3
        enddo
        !Vemos quien ha ganado
        if(vict==3) then
            if((par==1.AND.mod(j,2)==0).OR.(par==0.AND.mod(j,2)/=0)) then
                vict = 2
            else
                vict = 1
            endif
            EXIT
        endif
    enddo
    
    !Empate si vict=0, victoria de las X si vict=1 y victoria de las O si vict=2
    write(*,*) i
    if(vict==1) then
        write(*,*) "Derrota"
        b = b + 1
    endif
    if(vict==2) then
        a = a + 1
    endif
    if(mod(i,2500)==0.AND.vict/=1) then
        write(*,*) "Porcentaje de victorias recientes:", a/2500.*100
        write(*,*) "Porcentaje de derrotas recientes:", b/2500.*100
        write(*,*) "Partidas reales jugadas:", cont
        write(12,*) cont, a/2500.*100, b/2500.*100
        a = 0
        b = 0
    endif

    reverse = 0
    call simetria(pos,rot,trasp,reverse)

    if(vict/=1) then
        open(unit=10,file="Datos.dat")
        do
            read(10,*) sumatx
            backspace(10)
            if(sumatx==8) then
                write(10,fmt="(I1,1X,I1,10X,9(I1,2X))") vict, par, pos(:)
                write(10,*) "8 0"
                EXIT
            endif
            read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
            n = 0
            do m = 1,9
                if(posdata(m)/=pos(m)) n = n + 1
            enddo
            if(n==0.AND.par==sumato) EXIT
        enddo
        close(10)
    else
        i = i - 1
        !Borramos las partidas que tengan alguna relacion con la que hemos perdido
        open(unit=10,file="Datos.dat")
        open(unit=11,file="temp.dat")
            do 
                read(10,*) sumatx
                if(sumatx==8) then
                    write(11,*) "8 0"
                    EXIT
                endif
                backspace(10)
                read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
                backspace(10)
                n = 2
                do j = 1,2
                    if(posdata(j)==pos(j)) n = n - 1
                enddo
                if(par==sumato.AND.n==0) then
                    read(10,*)
                endif
                read(10,*) sumatx
                if(sumatx==8) then
                    write(11,*) "8 0"
                    EXIT
                endif
                backspace(10)
                read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
                write(11,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
            enddo
            rewind(10)
            rewind(11)
            do
                read(11,*) sumatx
                if(sumatx==8) then
                    write(10,*) "8 0"
                    EXIT
                endif
                backspace(11)
                read(11,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
                write(10,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
            enddo
        close(11)
        close(10)
    endif
    if(vict==1) then
        !Guardamos la partida perdida como si fuera una ganada cambiando el indice par/impar
        open(unit=10,file="Datos.dat")
        do
            read(10,*) sumatx
            backspace(10)
            if(sumatx==8) then
                vict = 2
                if(par==1) then
                    sumato = 0
                else
                    sumato = 1
                endif
                write(10,fmt="(I1,1X,I1,10X,9(I1,2X))") vict, sumato, pos(:)
                write(10,*) "8 0"
                EXIT
            endif
            read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") sumatx, sumato, posdata(:)
            n = 0
            do m = 1,9
                if(posdata(m)/=pos(m)) n = n + 1
            enddo
            if(n==0.AND.par/=sumato) EXIT
        enddo
        close(10)
        posdata(:) = pos(j)
    endif
    if(i==iter) EXIT
enddo

close(12)

end program red3