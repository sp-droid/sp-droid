module auxiliar

contains

subroutine mostrar_tablero(tablero)

  character, intent(in) :: tablero(3,3)

  ! Variables locales
  integer :: i
  write(*,*) '____________'
  do i = 1,3
    write(*,*)
    write(*,*)
    write(*,fmt='(a2,3(a1,a3))') '| ',( tablero(i,j),' | ', j=1,3 )
    write(*,*)
    write(*,*) '____________'
  enddo
end subroutine

!****************************************************************************

subroutine espera(x)
! Esta subrutina detiene la ejecucion del programa hasta que pasen x segundos
  implicit none
  real, intent(in) :: x
  real :: t,t1
  CALL CPU_TIME(t)
  do
    CALL CPU_TIME(t1)
    if (t1-t > x) exit
  enddo
end subroutine espera

!*****************************************************************************
logical function lleno(tablero)
! Esta funcion devuelve una variable logica que determina si hay empate
character   :: tablero(3,3)

lleno = .true.

if (any(tablero == ' ')) lleno =.false.

end function

!*****************************************************************************
logical function victoria(tablero)
! Esta funcion devuelve una variable logica que determina si alguien ha ganado
character   :: tablero(3,3)

! Variables locales
character   :: V(3)
integer     :: i

victoria = .false.

! filas y columnas
do i = 1,3
  if(all(tablero(i,:)=='X').or.all(tablero(i,:)=='O')) victoria = .true.
  if(all(tablero(:,i)=='X').or.all(tablero(:,i)=='O')) victoria = .true.
enddo
! diagonal principal
do i = 1,3
  V(i) = tablero(i,i)
enddo
if(all(V=='X').or.all(V=='O')) victoria = .true.
! diagonal secundaria
do i = 1,3
  V(i) = tablero(i,3-i+1)
enddo
if(all(V=='X').or.all(V=='O')) victoria = .true.
end function

end module
