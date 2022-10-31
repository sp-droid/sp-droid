program tool

implicit none
    
!Variables
integer     ::  i, j, w
character(len=100)   ::  name, largo

i = 0 

!Body
open (1,file ="ilovejohan\print.txt") 
do
    read (1,*, END=10) 
    i = i + 1 
enddo 
10 close (1) 

open(1,file="ilovejohan\print.txt")
    do j = 1,i
        read(1,*) name
        w = len_trim(name)-2
        if(name(w:w+2)=="txt") then
            largo = "ilovejohan\" // name
            open(2,file=largo)
            close(2)
        endif
    enddo
close(1)

end program tool