module modulo_grupo12

contains

subroutine rotacion90(simepos)
    integer,intent(inout)    ::  simepos(9)
    
    integer ::  m
    
    do m = 1,9
        if(simepos(m)==1) then
            simepos(m) = 7
        elseif(simepos(m)==2) then
            simepos(m) = 4
        elseif(simepos(m)==3) then
            simepos(m) = 1
        elseif(simepos(m)==4) then
            simepos(m) = 8
        elseif(simepos(m)==6) then
            simepos(m) = 2
        elseif(simepos(m)==7) then
            simepos(m) = 9
        elseif(simepos(m)==8) then
            simepos(m) = 6
        elseif(simepos(m)==9) then
            simepos(m) = 3
        endif
    enddo
    
end subroutine
    
subroutine traspuestapos1(simepos)
    integer,intent(inout)    ::  simepos(9)
        
    integer ::  m
    
    do m = 1,9
        if(simepos(m)==2) then
            simepos(m) = 4
        elseif(simepos(m)==3) then
            simepos(m) = 7
        elseif(simepos(m)==4) then
            simepos(m) = 2
        elseif(simepos(m)==6) then
            simepos(m) = 8
        elseif(simepos(m)==7) then
            simepos(m) = 3
        elseif(simepos(m)==8) then
            simepos(m) = 6
        endif
    enddo
        
end subroutine
    
subroutine simetria(simepos,rot,trasp,reverse)
    integer,intent(inout)   ::  simepos(9)
    integer,intent(inout)   ::  rot, trasp, reverse
    
    integer ::  m, i
    contador = 0
    
    if(reverse==1) then !Revertir los cambios
        if(trasp==1) then
            call traspuestapos1(simepos)
        endif
    
        do m = 0,3
            if(m==rot) then
                EXIT
            else
                call rotacion90(simepos)
            endif
        enddo
    else    !Conseguir una matriz equivalente
        if(simepos(1)==5.AND.simepos(1)/=0) then
            i = 2
        elseif(simepos(1)/=5) then
            i = 1
        else
            return
        endif
        trasp = 0
    
        do m = 0,3
            if(simepos(i)==1.OR.simepos(i)==2) then
                rot = 4-m
                EXIT
            else
                call rotacion90(simepos)
            endif
        enddo
    
        if(rot==4) rot = 0
        if(simepos(i)==4) then
            call traspuestapos1(simepos)
            trasp = 1
        endif
    endif
    
end subroutine
    
subroutine movimiento_grupo(tablero)
    character,intent(inout) ::  tablero(3,3)
    
    REAL    ::  random
    integer ::  fil, col, vict, numpartida, i, convar, m, n, rot, trasp, reverse, time, par, j
    integer ::  posdata(9), pos(9)
    integer,allocatable ::  fichero(:,:)
    character   ::  tableroviejo(3,3)

    tableroviejo = "-"

    !!Cosas para evitar cambiar el programa del profesor

    convar = 0
    do m = 1,3
        do n = 1,3
            if(tablero(m,n)/=" ") convar = convar + 1
        enddo
    enddo

    if(mod(convar,2)==0) then
        par = 0
    else
        par = 1
    endif

    open(unit=7,file="Tab_antiguo.txt")
        convar = 0
        do m = 1,3
            do n = 1,3
                if(tablero(m,n)==" ") tablero(m,n) = "-"
                if(tablero(m,n)/="-") convar = convar + 1
            enddo
        enddo
        if(convar<=1) then
            tableroviejo = "-"
            pos = 0
            write(7,fmt="(9(I1,2X))") pos(:)
            do m = 1,3
                write(7,fmt="(3(A1,1X))") tableroviejo(m,:)
            enddo
            rewind(7)
        endif

        read(7,*) pos(:)
        do m = 1,3
            read(7,*) tableroviejo(m,:)
        enddo
        rewind(7)
    
    do m = 1,9
        if(pos(m)==0) then
            j = m
            EXIT
        endif
    enddo
    

    do m = 1,3
        do n = 1,3
            if(tableroviejo(m,n)/=tablero(m,n)) then
                pos(j) = n + 3*(m-1)
                j = j + 1
            endif
        enddo
    enddo

    
    !!Vemos si podemos ganar ya
    !Horizontales y verticales
    do m = 1,3
        if(j<=4) EXIT
        convar = 0
        do n = 1,3
            if(tablero(m,n)=="X") convar = -2
            if(tablero(m,n)=="-") then
                fil = m
                col = n
            endif
            if(tablero(m,n)=="O") convar = convar + 1
        enddo
        if(convar==2) EXIT
        convar = 0
        do n = 1,3
            if(tablero(n,m)=="X") convar = -2
            if(tablero(n,m)=="-") then
                fil = n
                col = m
            endif
            if(tablero(n,m)=="O") convar = convar + 1
        enddo
        if(convar==2) EXIT
    enddo
    !Diagonales principal y secundaria
    if(convar/=2.AND.j>=5) then
        convar = 0
        do m = 1,3
            if(tablero(m,m)=="X") convar = -2
            if(tablero(m,m)=="-") then
                fil = m
                col = m
            endif
            if(tablero(m,m)=="O") convar = convar + 1
        enddo
    endif
    if(convar/=2.AND.j>=5) then
        convar = 0
        do m = 1,3
            if(tablero(m,4-m)=="X") convar = -2
            if(tablero(m,4-m)=="-") then
                fil = m
                col = 4-m
            endif
            if(tablero(m,4-m)=="O") convar = convar + 1
        enddo
    endif
    
    if(convar==2.AND.j>=5) then
        tablero(fil,col) = "O"
        pos(j) = col + 3*(fil-1)
        write(7,fmt="(9(I1,2X))") pos(:)
        do m = 1,3
            write(7,fmt="(3(A1,1X))") tablero(m,:)
            do n = 1,3
                if(tablero(m,n)=="-") tablero(m,n) = " "
            enddo
        enddo
        close(7)
        return
    endif

    !!Intentamos evitar que el enemigo gane
    !Horizontales y verticales
    do m = 1,3
        if(j<=3) EXIT
        convar = 0
        do n = 1,3
            if(tablero(m,n)=="O") convar = -2
            if(tablero(m,n)=="-") then
                fil = m
                col = n
            endif
            if(tablero(m,n)=="X") convar = convar + 1
        enddo
        if(convar==2) EXIT
        convar = 0
        do n = 1,3
            if(tablero(n,m)=="O") convar = -2
            if(tablero(n,m)=="-") then
                fil = n
                col = m
            endif
            if(tablero(n,m)=="X") convar = convar + 1
        enddo
        if(convar==2) EXIT
    enddo
    !Diagonales principal y secundaria
    if(convar/=2.AND.j>=4) then
        convar = 0
        do m = 1,3
            if(tablero(m,m)=="O") convar = -2
            if(tablero(m,m)=="-") then
                fil = m
                col = m
            endif
            if(tablero(m,m)=="X") convar = convar + 1
        enddo
    endif
    if(convar/=2.AND.j>=4) then
        convar = 0
        do m = 1,3
            if(tablero(m,4-m)=="O") convar = -2
            if(tablero(m,4-m)=="-") then
                fil = m
                col = 4-m
            endif
            if(tablero(m,4-m)=="X") convar = convar + 1
        enddo
    endif
    
    if(convar==2.AND.j>=4) then
        tablero(fil,col) = "O"
        pos(j) = col + 3*(fil-1)
        write(7,fmt="(9(I1,2X))") pos(:)
        do m = 1,3
            write(7,fmt="(3(A1,1X))") tablero(m,:)
            do n = 1,3
                if(tablero(m,n)=="-") tablero(m,n) = " "
            enddo
        enddo
        close(7)
        return
    endif
    
    !Ahora vemos si se ha jugado antes una partida equivalente, en caso de que no, será movimiento aleatorio, cosa frecuente en las primeras partidas de aprendizaje
    open(unit=10,file="Datos.dat")
    m = 0
    do
        read(10,*) convar, numpartida
        if(convar==8) EXIT
        if(numpartida==par) m = m + 1
    enddo
    rewind(10)
    allocate(fichero(m,11))
    fichero = 0
    n = 0

    do
        n = n + 1
        if(n>m) EXIT
        read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") fichero(n,:)
        if(fichero(n,2)/=par) n = n - 1
    enddo
    close(10)
    n = 0
    vict = 2
    posdata(:) = pos(:)
    reverse = 0
    call simetria(posdata,rot,trasp,reverse)
    
    do
        n = n + 1
        if(m==0) EXIT
        if(fichero(n,1)==vict) then
            convar = 0
            do i = 1,j-1
                if(fichero(n,2+i)==posdata(i)) convar = convar + 1
            enddo 
            if(convar==(j-1)) then
                posdata(:) = fichero(n,3:11)
                EXIT
            endif
        endif
        if(n==m) then
            if(vict==2) then
                vict = 0
            elseif(vict==0) then
                vict = 5
                EXIT
            endif
            n = 0
        endif
    enddo

    if(vict==5.OR.m==0) then
        call SYSTEM_CLOCK(time)
        random = RAND(time)
        do
            random = RAND(0)
            fil = int(random*4)
            if(fil==0) fil = 1
            if(fil==4) fil = 3
            random = RAND(0)
            col = int(random*4)
            if(col==0) col = 1
            if(col==4) col = 3

            if(tablero(fil,col)=="-") then
                tablero(fil,col) = "O"
                pos(j) = col + 3*(fil-1)
                deallocate(fichero)
                write(7,fmt="(9(I1,2X))") pos(:)
                do m = 1,3
                    write(7,fmt="(3(A1,1X))") tablero(m,:)
                    do n = 1,3
                        if(tablero(m,n)=="-") tablero(m,n) = " "
                    enddo
                enddo
                close(7)
                return
            endif
        enddo
    endif

    reverse = 1
    call simetria(posdata,rot,trasp,reverse)
    pos(j) = posdata(j)
    if(pos(j)>=7) then
        fil = 3
    elseif(pos(j)>=4) then
        fil = 2
    else
        fil = 1
    endif
    col = pos(j) - 3*(fil-1)
    tablero(fil,col) = "O"
    deallocate(fichero)

    write(7,fmt="(9(I1,2X))") pos(:)
    do m = 1,3
        write(7,fmt="(3(A1,1X))") tablero(m,:)
        do n = 1,3
            if(tablero(m,n)=="-") tablero(m,n) = " "
        enddo
    enddo
    close(7)
   
end subroutine

end module
