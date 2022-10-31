program algebra
    
use algebra_lineal
use calculo_diferencial
implicit none
  
        integer             :: tema,subtema, fin
        integer             :: i,j,k
        integer             :: m,n,metodo
        real(8),allocatable :: A(:,:),A1(:,:)

        !variables para el modulo determinante/inversa
        real*8              :: det            
        real(8),allocatable :: IDENTIDAD(:,:)

        !variables para el modulo de sistemas
        real(8),allocatable :: R1(:)             
        integer             :: subtema2,convergencia 

        !variables para el modulo minimos cuadrados
        integer             :: orden,n_datos
        real*8,allocatable  :: X(:)              
        
        !variables para los autovalores
        real*8,allocatable  :: Q(:,:),autovalores(:), R(:,:), autovector(:), autovectores(:,:)
        integer             :: metodo2,metodo3 
        real*8              :: autoval      
 
        !variables para EDO
            real*8,allocatable :: D(:,:),F(:),Condiciones_contorno(:,:)
            integer            :: n_intervalos,interv_serie
                !variables Problema 1
                real*8                :: T_intermedia,h
                real*8,allocatable    :: conductividad(:),L(:)

  
  fin=0
  do while(fin/=1)
  do i=1,4;write(*,*);enddo
  write(*,*)'ALGEBRA LINEAL:'
  write(*,*)'   AUTOVALORES                                                      ------------> 1'     
  write(*,*)'CALCULO DIFERENCIAL:'  
  write(*,*)'   PROBLEMA DE LA TEMPERATURA (CON LOS DATOS DADOS)                 ------------> 2'
  write(*,*)'   PROBLEMA DE LA TEMPERATURA (INTRODUCCION DE DATOS DINAMICA)      ------------> 3'   
  
  read(*,*)tema
  do i=1,4;write(*,*);enddo        
  
  
  if(tema==1)then
    
            !           ******************************************************************************
            !           *****                             AUTOVALORES                            *****
            !           ******************************************************************************

            write(*,*)'Introduce las dimensiones de la matriz A:'
            read(*,*)n,m

            allocate(A(n,m)); allocate(Q(n,m)); allocate(R(m,m));allocate(autovalores(n))
            allocate(autovector(n));allocate(autovectores(n,n))
            write(*,*)
            write(*,*)'Introduce la matriz A:'
            do i=1,n
                write(*,*)'Introduce la fila',i
                read(*,*)A(i,:)
            enddo
            write(*,*)
            write(*,*)'LA MATRIZ INTRODUCIDA ES:'
            do i=1,n
                write(*,*)A(i,:)
            enddo
            write(*,*)
            write(*,*)'CALCULAR SOLO EL MAYOR  AUTOVALOR    --> 1'
            write(*,*)'CALCULAR SOLO EL MENOR  AUTOVALOR    --> 2'
            write(*,*)'CALCULAR TODOS LOS AUTOVALORES       --> 3'
            read(*,*)metodo
            
            if (metodo==1) then
                call metpotencia(A,autoval,autovector)
                write(*,*)'El autovalor dominante en valor absoluto es:',autoval
                write(*,*)'Su autovector correspondiente es:',autovector
            elseif(metodo==2) then
                call metpotenciainv(A,autoval,autovector)
                write(*,*)'El autovalor mas cercano a 0 es:',autoval
                write(*,*)'Su autovector correspondiente es:',autovector
            else
                write(*,*)
                write(*,*)'QR         --> 1'
                write(*,*)'POTENCIA   --> 2'
                read(*,*)metodo2
                if (metodo2==1)then
                    write(*,*)'GRAHMM SMITH --> 1'
                    write(*,*)'HOUSEHOLDER  --> 2'
                    read(*,*)metodo3
                    Call algoritmoQR(A,autovalores,metodo3)
                    write(*,*)
                    write(*,*)autovalores

                else 
                    call metshift(A,autovalores,autovectores)
                    write(*,*)
                    do i=1,n
                        write(*,*)'Autovalor',i,autovalores(i),'    :',autovectores(i,:)
                    enddo
                endif
            endif
            deallocate(A,Q,R,autovalores,autovectores,autovector)
            

  elseif(tema==2)then


    allocate(Condiciones_contorno(3,2))
    allocate(conductividad(2));allocate(L(2))

    !DATOS
    n_intervalos=2
    Condiciones_contorno(1,1)=0; Condiciones_contorno(1,2)=500
    Condiciones_contorno(3,1)=1; Condiciones_contorno(3,2)=298.15
    L=0.5
    conductividad(1)=16.3; conductividad(2)=209.3
 



        T_intermedia= (conductividad(1)/L(1))*Condiciones_contorno(1,2)
        T_intermedia= T_intermedia  +  (conductividad(2)/L(2))*Condiciones_contorno(3,2)
        T_intermedia= T_intermedia  / ((conductividad(2)/L(2))+(conductividad(1)/L(1)))
        Condiciones_contorno(2,2)=T_intermedia
        Condiciones_contorno(2,1)=Condiciones_contorno(1,1) + L(1)





        interv_serie=1
        do j=1,n_intervalos
            write(*,*);write(*,*)'**************   INTERVALO NUMERO',j,' **************'
            write(*,*)
            call T_barra_unidimensional(F,interv_serie,j,Condiciones_contorno,h)
            m=size(F)
            write(*,*);write(*,*)'**********  SOLUCION DEL INTERVALO',j,':'
                do i=1,m
                    write(*,'(f8.6,5X,f10.6)')((i-1.d0)+((j-1.d0)*1.d0/(h*n_intervalos)))*h,F(i)
                enddo  
            deallocate(F)  
        enddo
    deallocate(Condiciones_contorno,conductividad,L)





  elseif(tema==3)then
    

        write(*,*);write(*,*)
            !METODO DE DIFERENCIAS FINITAS

                !ESTE PROGRAMA ESTA DESTINADO SOLO A LA RESOLUCION DE UN PROBLEMA
                !LUEGO SE INTENTARA GENERALIZAL LO MAS POSIBLE
                write(*,*)'INTRODUCE EL NUMERO DE INTERVALOS:'
                read(*,*)n_intervalos

                    !***********************************************************************CONDICIONES DE CONTORNO
                    if(interv_serie==1)then
                        allocate(Condiciones_contorno(2,2))
                        Condiciones_contorno=0
                    write(*,*)
                    write(*,*)'Introduce las condiciones de contorno, (de los extremos del intervalo), de la forma(x,F(x)) :'
                        do i=1,2
                            write(*,*)'Condicion de contorno',i,':'
                            read(*,*)Condiciones_contorno(i,1)
                            read(*,*)Condiciones_contorno(i,2)
                        enddo
                    else
                        allocate(Condiciones_contorno(n_intervalos+1,2))
                        allocate(conductividad(n_intervalos));allocate(L(n_intervalos))
                        Condiciones_contorno=0;conductividad=0;L=0
                        write(*,*)
                        do i=1,n_intervalos+1,n_intervalos
                            write(*,*)'Condicion de contorno',i,':'
                            read(*,*)Condiciones_contorno(i,1)
                            read(*,*)Condiciones_contorno(i,2)
                        enddo
                            do i=1,n_intervalos
                                write(*,*)'Introduce el coeficientes de conductividad',i,':'                         
                                read(*,*)conductividad(i)
                                write(*,*)'Introduce la longitudes de la barra',i,':'                                 
                                read(*,*)L(i)
                            enddo

                            do i=1,n_intervalos-1
                                T_intermedia= (conductividad(i)/L(i))*Condiciones_contorno(i,2)
                                T_intermedia= T_intermedia  +  (conductividad(i+1)/L(i+1))*Condiciones_contorno(i+2,2)
                                T_intermedia= T_intermedia  / ((conductividad(i+1)/L(i+1))+(conductividad(i)/L(i)))
                                Condiciones_contorno(i+1,2)=T_intermedia
                                Condiciones_contorno(i+1,1)=Condiciones_contorno(1,1) + i*L(i)
                            enddo
                    endif

                if(n_intervalos>1)then
                    write(*,*)'intervalos en serie     -->1'
                    write(*,*)'intervalos por separado -->2'
                    read(*,*)interv_serie
                endif
                write(*,*)

                do j=1,n_intervalos
                    write(*,*);write(*,*)'**************   INTERVALO NUMERO',j,' **************'
                    write(*,*)
                    call T_barra_unidimensional(F,interv_serie,j,Condiciones_contorno,h)
                    m=size(F)
                    write(*,*);write(*,*)'**********  SOLUCION DEL INTERVALO',j,':'
                        do i=1,m
                            write(*,*)F(i)
                        enddo  
                    deallocate(F)  
                enddo
            deallocate(Condiciones_contorno,conductividad,L)


  endif


do i=1,3;write(*,*);enddo
write(*,*)'realizar mas calculos  ---->0'
write(*,*)'salir del programa     ---->1'
read(*,*)fin
if(fin==1)then; exit; endif
                  

  enddo
    
    
  end program
