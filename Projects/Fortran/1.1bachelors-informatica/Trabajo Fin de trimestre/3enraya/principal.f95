program tres_en_raya

	use modulo_grupo12
	use modulo_profesor
	use auxiliar

	implicit none

	integer	:: i ! Contador de partida
	integer :: n_partida
	integer :: n_movimientos
  integer :: victorias_javi, victorias_grupo
	character :: tablero(3,3)


	! Comienza el juego
  n_partida = 10
	victorias_javi = 0
	victorias_grupo = 0


	do i = 1, n_partida
    tablero = ' '
		n_movimientos = 0
		write(*,*) 'Comienza la partida ', i

		if (mod(i,2)==0) then ! Las partidas pares las empiezo yo
				call movimiento_profesor(tablero)
				call mostrar_tablero(tablero)
				call espera(1.0)
				n_movimientos = 1
		endif

		do
				call movimiento_grupo(tablero,i)
				call mostrar_tablero(tablero)
				call espera(1.0)
				n_movimientos = n_movimientos + 1
				if (victoria(tablero)) then
           write(*,*) 'Habeis ganado la partida: ',i
					 victorias_grupo = victorias_grupo + 1
					 exit ! Sale del bucle movimiento y empieza una partida nueva
				endif
				if (lleno(tablero)) then
					write(*,*) 'Partida: ',i, ' empate'
					exit
				endif


				call movimiento_profesor(tablero)
				call mostrar_tablero(tablero)
				call espera(1.0)
				n_movimientos = n_movimientos + 1
				if (victoria(tablero)) then
           write(*,*) 'Habeis perdido la partida: ',i
					 victorias_javi = victorias_javi + 1
					 exit ! Sale del bucle movimiento y empieza una partida nueva
				endif
				if (lleno(tablero)) then
					write(*,*) 'Partida: ',i, ' empate'
					exit
				endif

				if (n_movimientos > 9) then
					write(*,*) 'Error'
					exit
				endif

	  enddo

		write(*,*)
    write(*,*) 'Contador tras la partida ',i
		write(*,*) 'Grupo 12: ', victorias_grupo, '  Profesor: ', victorias_javi
		write(*,*)
    call espera(4.0)
	enddo


endprogram
