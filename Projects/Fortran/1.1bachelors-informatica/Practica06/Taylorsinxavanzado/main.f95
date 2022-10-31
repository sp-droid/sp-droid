program politaylor
implicit none
!Declaración de variables
REAL*16      ::  x, sumat, tol
REAL*16, allocatable  ::  V(:)
integer     ::  n, i, q
integer*16   :: facto
!Declaración de variables segunda parte (se reciclan i,n,q,facto,V(:),x y sumat)
character(2)   ::  answer
REAL,allocatable    ::  Xaxis(:), RS(:)
integer ::  cont
REAL    ::  interval
!Inicialización de variables
sumat = 0
q = 0
facto = 1
cont = 0
i = 0
!Lectura de datos
call system("cls")
write(*,*) "Desea indicar el grado del desarrollo del seno (escriba v1) o un error maximo/tolerancia para"
write(*,*) "calcular mediante este programa el desarrollo en un punto x?"
write(*,*) "(Escriba cualquier otra cosa para el segundo caso)"
read(*,*) answer

!!V1
if(answer=="v1".OR.answer=="V1") then
    write(*,*) "Introduzca un numero real para calcular su seno y el grado del desarrollo empleado"
    read(*,*) x, n
    do i = 0,n
        do
            q = q + 1
            facto = facto*q
            if(q==(2*i+1)) then
                EXIT
            endif
        enddo
        sumat = sumat + (((-1.)**i)*(x**(2.*i+1))/facto)
    enddo

    write(*,*) "Desarrollo de Taylor de grado",n,"de la funcion sen(x) en x",sumat
    write(*,*) "Valor real de la funcion:", sin(x)
    write(*,*) "Error:", abs(sumat-sin(x))
    
    open(unit=747,file="Taylor.txt")
    write(747,fmt="(2X,A16,4X,A17,4X,A30)") "Nº de términos", "Valor de la serie", "Diferencia con el valor exacto" 
    write(747,fmt="(8X,I2,14X,F9.6,17X,E12.6)") n, sumat, abs(sumat-sin(x))
    close(747)

    write(*,*) "Se ha generado un archivo en formato txt con estos valores"
    STOP
endif

!!V2
write(*,*) "Introduzca un numero real para calcular su seno y la tolerancia que desea"
read(*,*) x, tol

if(tol<0) then
    write(*,*) "La tolerancia debe tener valor positivo"
    STOP
endif

!Cuerpo del programa
do
    do 
        q = q + 1
        facto = facto*q
        if(q==(2*i+1)) then
            EXIT
        endif
    enddo
    sumat = sumat + (((-1.)**i)*(x**(2.*i+1))/facto)
    i = i + 1
    if((abs(sumat-sin(x)))<tol) then
        EXIT
    endif
enddo
n = i

write(*,*) "Desarrollo de Taylor de la funcion sen(x) en x",sumat
write(*,*) "n terminos empleados en el desarrollo:", n
write(*,*) "Valor real de la funcion:", sin(x)
write(*,*) "Error:", abs(sumat-sin(x))

!Cálculo y volcado de los coeficientes a un vector
allocate(V(n))
V = 0
facto = 1
q = 0

do i = 0,(n-1)
    do
        q = q + 1
        facto = facto*q
        if(q==(2*i+1)) then
            EXIT
        endif
    enddo
    V(i+1) = ((-1.)**i)/facto
enddo

write(*,*) "Vector con los coeficientes del desarrollo:"
write(*,202) V(:)
202 FORMAT(' ',4G10.2)

open(unit=747,file="Taylor.txt")
write(747,fmt="(A33,2X,E12.6)") "Con una tolerancia establecida de",tol
write(747,*)
write(747,fmt="(2X,A16,4X,A17,4X,A30)") "Nº de términos", "Valor de la serie", "Diferencia con el valor exacto"
write(747,fmt="(8X,I2,14X,F9.6,17X,E12.6)") n, sumat, abs(sumat-sin(x))
close(747)

write(*,*) "Se ha generado un archivo con estos valores"

!Parte 2 del programa2, opcional
write(*,*) "Desea exportar puntos definidos con el desarrollo (de n terminos del resultado anterior) de Taylor"
write(*,*) "a un documento? (Si para continuar, cualquier otra palabra/numero para salir)"
read(*,*) answer
if(answer/="si".AND.answer/="Si") then
    write(*,*) "Saliendo..."
    STOP
endif

write(*,*) "Defina el intervalo (positivo real) entre puntos (el intervalo que los engloba es [-15,15] siempre)"
read(*,*) interval

!Contamos cuantos puntos hay entre -10 y 10 con el intervalo definido y establecemos los tamaños de los vectores
deallocate(V)
x = -15
do
    cont = cont + 1
    x = x + interval
    if(x>15) then
        EXIT
    endif
enddo

allocate(V(cont))
allocate(RS(cont))
allocate(Xaxis(cont))

!Inicializamos variables y vectores antes del bucle
V = 0
Xaxis = 0
RS = 0
cont = 0
x = -15

!Cálculo del valor del desarrollo n veces de Taylor en los valores [-15,15] con un salto dependiente del valor "intervalo"
do
    q = 0
    facto = 1
    sumat = 0
    do i = 0,n
        do 
            q = q + 1
            facto = facto*q
            if(q==(2*i+1)) then
                EXIT
            endif
        enddo
        sumat = sumat + (((-1.)**i)*(x**(2.*i+1))/facto)
    enddo
    cont = cont + 1
    Xaxis(cont) = x
    V(cont) = sumat
    RS(cont) = sin(x)
    x = x + interval
    if(x>15) then
        EXIT
    endif
enddo

open(unit=747,file="exceldata.txt")
do i = 1,cont
    write (747,*) Xaxis(i),"     ", V(i),"    ", RS(i)
enddo
close(747)

write(*,*) "Proceso finalizado"

end program politaylor