program bolas
    
    implicit none
    
    !Declaración
    integer     ::  i, j, pos, k
    integer     ::  posA(5), posB(5), values(8)
    integer,allocatable ::  seed(:)
    REAL        ::  random, prob, ct, cf
    
    ct = 0
    cf = 0
    
    !Cuerpo del programa
    write(*,*) "Este programa calcula la probabilidad de que ocurra un suceso"
    write(*,*) "En este caso, repartimos 5 bolas A y 5 bolas B en 5 urnas, P(una urna contenga 3B y 3N)"
    write(*,*) "Presione una tecla para continuar"
    read(*,*)
    
    call date_and_time(VALUES=values)
    call random_seed(size=k)
    allocate(seed(k))
    seed(1:8) = values(:)
    call random_seed(put=seed)

    do i = 1,1000000
        posA = 0
        posB = 0

        do j = 1,5
            call random_number(random)
            pos = int(random*5)
            pos = pos+1
            if(pos==6) pos = 5
            posA(pos) = posA(pos) + 1

            call random_number(random)
            pos = int(random*5)
            pos = pos+1
            if(pos==6) pos = 5
            posB(pos) = posB(pos) + 1
        enddo
        
        ct = ct+1
        do j = 1,5
            if(posA(j)==3.AND.posB(j)==3) cf = cf+1
        enddo
    enddo

    write(*,*) cf
    write(*,*) ct
    prob = cf/ct*100
    write(*,*) prob, "%"
    
end program bolas