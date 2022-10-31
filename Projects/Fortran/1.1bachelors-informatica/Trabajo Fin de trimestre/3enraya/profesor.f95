module modulo_profesor

contains

subroutine movimiento_profesor(tablero)
    ! Esta subrutina solo es una prueba funcional del trabajo.
    ! No es competitiva, tienen errores de programación y copiarla para que sea
    ! la vuestra supone un 0.
    character, intent(inout) :: tablero(3,3)
  
    do i =1,3
      do j = 1,3
        if(tablero(i,j)==' ') then
               tablero(i,j)='X'
            return ! Devuelve el control al programa principal
        endif
      enddo
    enddo
  
end subroutine

end module
