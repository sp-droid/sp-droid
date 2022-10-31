program series
implicit none
integer ::  i
REAL    ::  sum

!Inic var
sum = 0

!Serie a
do i = 0,27
    sum = 2*i +1 +sum
enddo
write(*,*) "Serie a:", sum
sum = 0

!Serie b
do i = 1,27
    sum = 1./(2.*i**2.) +sum
enddo
write(*,*) "Serie b:", sum
sum = 0

!Serie c
do i = 1,27
    sum = ((-1.)**i)/(2.*i-1.) +sum
enddo
write(*,*) "Serie c:", sum

end program series