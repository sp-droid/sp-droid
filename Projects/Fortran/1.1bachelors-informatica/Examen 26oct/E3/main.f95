program vectoru
implicit none
integer, allocatable    ::  U(:)
integer     ::  i, sum

allocate(U(1003))
U = 0
sum = 0

do i = 2,1003
    if(U(i-1)>=0) then
        U(i) = U(i-1) -i
    else
        U(i) = U(i-1) +i
    endif
    sum = U(i) + sum
enddo

write(*,*) "-Valor de U(997):",U(997) ,"    -Valor de U(994):",U(994) ,"    -Valor de U(991):", U(991)
write(*,*) "Suma de los elementos de U:", sum

end program vectoru