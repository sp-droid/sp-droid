module estadistica
contains

subroutine media(Vector,med)
    REAL,intent(in)     ::  Vector(:)
    REAL,intent(out)    ::  med

    integer     ::  n, i
    n = size(Vector,1)
    med = 0

    do i = 1,n
        med = Vector(i) + med
    enddo
    med = med/n

end subroutine media

end module