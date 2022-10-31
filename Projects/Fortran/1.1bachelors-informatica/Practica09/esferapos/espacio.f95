module espacio
use variables

contains

!Distancia entre dos puntos en R3
subroutine distancia(coord1,coord2,dist)
    REAL*8,intent(in)      ::  coord1(3), coord2(3)
    REAL*8,intent(out)     ::  dist

    dist = SQRT((coord1(1)-coord2(1))**2+(coord1(2)-coord2(2))**2+(coord1(3)-coord2(3))**2)

end subroutine distancia

!Posicion relativa a una esfera
subroutine esfrelativ(radio,dist,posicion)
    REAL*8,intent(in)       ::  radio, dist
    character(15),intent(inout)    ::  posicion

    if(abs(dist-radio)<EPSILON(1.d0)) then
       posicion = "Superficial"

    elseif(dist>radio) then
        posicion = "Exterior"

    else
        posicion = "Interior"
    endif

end subroutine esfrelativ

!Orden de nodos
subroutine ordennodo(vectnodo)
    type(nodo),intent(inout)    ::  vectnodo(:)

    integer     ::  i, j, n
    type(nodo)  ::  inter
    n = size(vectnodo,1)

    do i = 1,n
        do j = 1,n
            if(vectnodo(j)%distancia>vectnodo(i)%distancia) then
                inter = vectnodo(i)
                vectnodo(i) = vectnodo(j)
                vectnodo(j) = inter
            endif
        enddo
    enddo

end subroutine ordennodo

end module