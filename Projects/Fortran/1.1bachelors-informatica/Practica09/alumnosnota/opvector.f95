module opvector
contains

subroutine ordenvect(Vector)        !Usando el método de encontrar el más grande/pequeño e intercambiarlo con el primero, segundo...
    REAL,intent(inout)     :: Vector(:)

    integer     ::  i, n, j
    n = size(Vector,1)

    do i = 1,n
        do j = i,n
            if(Vector(j)>Vector(i)) then
                Vector(j) = Vector(j) + Vector(i)
                Vector(i) = Vector(j) - Vector(i)
                Vector(j) = Vector(j) - Vector(i)
            endif
        enddo
    enddo

end subroutine ordenvect

end module