program ordenfich
implicit none

!Declaración de variables
integer     ::  m, n, i, j, z, convar
integer, allocatable    :: fichero(:,:)


!Cuerpo del programa

open(unit=10,file="Datos.dat")
    m = 0
    do
        read(10,*) convar
        if(convar==8) EXIT
        m = m + 1
    enddo
    rewind(10)
    allocate(fichero(m,11))
    fichero = 0
    n = 0
    do
        n = n + 1
        if(n>m) EXIT
        read(10,fmt="(I1,1X,I1,10X,9(I1,2X))") fichero(n,:)
    enddo
close(10)

do i = 2,11
    do j = 1,m
        do z = j,m
            if(fichero(j,i)>fichero(z,i)) then
                convar = 0
                do n = 2,i-1
                    if(i==2) EXIT
                    if(fichero(j,n)==fichero(z,n)) convar = convar + 1
                enddo
                if(convar==(i-2)) then
                    do n = 2,11
                        fichero(j,n) = fichero(j,n) + fichero(z,n)
                        fichero(z,n) = fichero(j,n) - fichero(z,n)
                        fichero(j,n) = fichero(j,n) - fichero(z,n)
                    enddo
                endif
            endif
        enddo
    enddo
enddo

open(unit=10,file="Datos.dat")
    do n = 1,m
        write(10,fmt="(I1,1X,I1,10X,9(I1,2X))") fichero(n,:)
    enddo
    write(10,*) "8 0"
close(10)


end program ordenfich