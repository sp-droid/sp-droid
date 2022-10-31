module algebra_lineal

contains

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de Gauss
subroutine metgauss(A,x)
    !Argumentos de la subrutina
    real(8), intent(in) :: A(:,:)
    real(8), intent(inout) :: x(:)

    !Variables locales
    integer :: m !Dimension del problema A(m,m) b(m) X(m)
    real(8), allocatable :: Ab(:,:) !Matriz ampliada. Dimension depende de m
    real(8) :: h 
    integer :: i,j,k,l,maxi
    real(8),allocatable :: V(:), aux(:)
    integer :: maximo(1)

    m=size(A,1)
    allocate(Ab(m,m+1))

    Ab(:,:) = A(:,:)
    allocate(aux(m))

    !Etapa triangulacion 
       do i=1,m-1
         if (abs(Ab(i,i))<epsilon(1.d0)) then
            allocate(V(m-i))
                do l=1,(m-i)
                V(l)=Ab(l+i,i)
                enddo   
                V=(abs(V))
                maximo=maxloc(V)
                maxi=maxval(maximo)
                aux=Ab(maxi+i,:)
                Ab(maxi+i,:)=Ab(i,:)
                Ab(i,:)=aux
                deallocate(V) 

         endif


         do k=i+1,m                         !Filas por debajo
            h=(Ab(k,i))/(Ab(i,i))              !Factor que multiplica la fila i
            Ab(k,:)=Ab(k,:)-(h*Ab(i,:))
         enddo
       enddo
    !Fin triangulacion
    
    !Etapa sustitucion 
    do i=m,1,-1 
        h=Ab(i,m+1)     !Guardo en h el valor de la columna ampliada
        do j=i+1,m 
            h=h-(Ab(i,j)*x(j))   !Resto los productos de x´s ya calculados
        enddo 
        
        x(i)=h/(Ab(i,i))   !Divido por la diagonal 
    enddo
end subroutine

    
!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de Gauss-Jordan
subroutine metgaussjordan(A,R)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)

    integer ::  fil, col, i, j, m
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:)

    !AB es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    fil = size(A,1)
    col = size(A,2) - 1
    allocate(AB(fil,col+1))
    AB(:,:) = A(:,:)

    if(col>fil) then
        write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
        R = 0
        return
    endif

    !Triangulación inferior    
    do j = 1,col
        do m = j,fil
            if(abs(AB(j,j))<EPSILON(1.d0)) AB(j,j) = 0
            if(abs(AB(j,j))<abs(AB(m,j))) then
                AB(j,:) = AB(j,:) + AB(m,:)
                AB(m,:) = AB(j,:) - AB(m,:)
                AB(j,:) = AB(j,:) - AB(m,:)
            endif
        enddo
        if(abs(AB(j,j))<EPSILON(1.d0)) then
            write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
            R = 0
            return
        endif
        do i = j+1,fil
            if(j==fil) EXIT
            h = AB(i,j)/AB(j,j)
            AB(i,:) = AB(i,:) -h*AB(j,:)
        enddo
    enddo

    !Triangulación superior y obtención de soluciones
    R = 0
    do j = col,1,-1
        do i = j-1,1,-1
            if(j==1) EXIT
            h = AB(i,j)/AB(j,j)
            AB(i,j) = AB(i,j) -h*AB(j,j)
            AB(i,col+1) = AB(i,col+1) -h*AB(j,col+1)
        enddo
        R(j) = AB(j,col+1)/AB(j,j)
    enddo

end subroutine

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de LU (SIN PIVOTE)
subroutine metlu(A,R,pivote)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)
    integer,intent(inout)   ::  pivote

    integer ::  n, i, j, m, z
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:), L(:,:), U(:,:), P(:,:), Y(:)

    !AB es la ampliada, R es el vector con los resultados, L(lower), U(upper), P(permutación) y el resto son variables locales auxiliares
    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(L(n,n))
    allocate(U(n,n))
    allocate(P(n,n))
    allocate(Y(n))

    !Inicialización
    pivote = 0
    AB(:,:) = A(:,:)
    U(:,:) = A(:,1:n)
    R(:) = AB(:,n+1)
    P = 0
    L = 0
    Y = 0
    do i = 1,n
        P(i,i) = 1
        L(i,i) = 1
    enddo
    
    !Cálculo de U, L y P
    do j = 1,n
        do m = j,n
            if(abs(U(j,j))<EPSILON(1.d0)) then
                pivote = 1
                return
            endif
        enddo
        do i = j+1,n
            if(j==n) EXIT
            h = U(i,j)/U(j,j)
            U(i,:) = U(i,:) -h*U(j,:)
            L(i,j) = h
        enddo
    enddo
    
    R = matmul(P,R)

    !Resolvemos Ly = Pb
    do m = 1,n
        do z = 1,m-1
            if(m==1) EXIT
            Y(m) = Y(m) + Y(z)*L(m,z)
        enddo
        Y(m) = (R(m)-Y(m))/L(m,m)
    enddo
    R = 0
    !Resolvemos Ux = y
    do m = n,1,-1
        do z = n,m+1,-1
            if(m==n) EXIT
            R(m) = R(m) + R(z)*U(m,z)
        enddo
        R(m) = (Y(m)-R(m))/U(m,m)
    enddo
end subroutine

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método iterativo de Jacobi
subroutine metjacobi(A,R,convergencia)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)
    integer,intent(inout)   ::  convergencia

    integer ::  n, i, j, iter
    REAL*8  ::  h
    REAL*8,allocatable  ::  Raux(:), Raux2(:), AB(:,:), U(:,:), GS(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(Raux(n))
    allocate(Raux2(n))
    AB(:,:) = A(:,:)
    Raux(:) = R(:)
    convergencia = 0

    !Vemos si la matriz del problema es convergente
    allocate(U(n,n))
    allocate(GS(n,n))
    do i = 1,n
        do j = 1,n
            if(i<=j) then
                GS(i,j) = A(i,j)
            else
                U(i,j) = A(i,j)
            endif
        enddo
    enddo
    
    call inversa(GS)
    GS = matmul(GS,U)
    if(radio_espectral(GS)>1) then
        convergencia = 1
        return
    endif
    !Iteraciones hasta encontrar una solución suficientemente aproximada
    iter = 0
    do
        iter = iter + 1
        do i = 1,n
            h = AB(i,n+1)
            do j = 1,n
                if(j/=i) then  
                    h = h - Raux(j)*AB(i,j)
                endif
            enddo
            R(i) = h/AB(i,i)
        enddo
        !Criterio de parada
        Raux2(:) = R(:) - Raux(:)
        if(iter/=1) h = norma2(Raux2)/norma2(R)
        if(h<EPSILON(1.d0).AND.iter/=1) then
            return
        endif
        Raux(:) = R(:)
    enddo

end subroutine

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método iterativo de Gauss-Seidel
subroutine metgaseidel(A,R,convergencia)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)
    integer,intent(inout)   ::  convergencia

    integer ::  n, i, j, iter
    REAL*8  ::  h
    REAL*8,allocatable  ::  Raux(:), Raux2(:), AB(:,:), U(:,:), GS(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(Raux(n))
    allocate(Raux2(n))
    AB(:,:) = A(:,:)
    Raux(:) = R(:)
    convergencia = 0

    !Vemos si la matriz del problema es convergente
    allocate(U(n,n))
    allocate(GS(n,n))
    do i = 1,n
        do j = 1,n
            if(i<=j) then
                GS(i,j) = A(i,j)
            else
                U(i,j) = A(i,j)
            endif
        enddo
    enddo
    
    call inversa(GS)
    GS = matmul(GS,U)
    if(radio_espectral(GS)>1) then
        convergencia = 1
        return
    endif
    !Iteraciones hasta encontrar una solución suficientemente aproximada
    iter = 0
    do
        iter = iter + 1
        do i = 1,n
            h = AB(i,n+1)
            do j = 1,n
                if(j/=i) then  
                    h = h - R(j)*AB(i,j)
                endif
            enddo
            R(i) = h/AB(i,i)
        enddo
        !Criterio de parada
        Raux2(:) = R(:) - Raux(:)
        if(iter/=1) h = norma2(Raux2)/norma2(R)
        if(h<EPSILON(1.d0).AND.iter/=1) then
            return
        endif
        Raux(:) = R(:)
    enddo

end subroutine

!!!Subrutina que factoriza una matriz por el método de Gram-Schmidt
subroutine factorQRGramSchmidt(A,Q,R)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  Q(:,:), R(:,:)

    integer     ::  i, j, fil, col
    REAL*8      ::  h

    fil = size(A,1)
    col = size(A,2)
    Q = 0
    R = 0

    !Mediante Gram-Schmidt, ortogonalizamos los vectores columna de A
    do j = 1,col
        Q(:,j) = A(:,j)
        R(j,j) = 1
        do i = 2,j
            if(norma2(Q(:,i-1))>EPSILON(1.d0)) h = dot_product(Q(:,i-1),A(:,j))/(norma2(Q(:,i-1)))**2
            Q(:,j) = Q(:,j) - h*Q(:,i-1)
        enddo
        if(norma2(Q(:,j))>EPSILON(1.d0)) Q(:,j) = Q(:,j)/norma2(Q(:,j))
    enddo
    R = matmul(TRANSPOSE(Q),A)
    
end subroutine

!!!Subrutina que factoriza una matriz por el método de las reflexiones de Householder
subroutine factorQRHouseholder(A,Q,R)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  Q(:,:), R(:,:)

    integer     ::  i, m, n
    REAL*8,allocatable  ::  v(:,:)
    REAL*8      ::  beta
    
    m = size(A,1)
    n = size(A,2)

    Q = 0
    do i = 1,m
        Q(i,i) = 1
    enddo
    R = A

    do i = 1,n
        allocate(v(m-i+1,1))

        v(:,1) = R(i:m,i)
        v(1,1) = R(i,i) + sign(1.d0,R(i,i))*norma2(R(i:m,i))
        if(abs(v(1,1))>EPSILON(1.d0)) then
            v = v/v(1,1)
            beta = 2/dot_product(v(:,1),v(:,1))
        else
            beta = 0
        endif
        R(i:m,i:n) = R(i:m,i:n) -beta*matmul(v,matmul(transpose(v),R(i:m,i:n)))
        Q(:,i:m) = Q(:,i:m) -beta*matmul(matmul(Q(:,i:m),v),transpose(v))
        
        deallocate(v)
    enddo
    
end subroutine

!Subrutina que calcula los autovalores de una matriz cuadrada por mediante el algoritmo QR
subroutine algoritmoQR(A,autoval,metodo)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  autoval(:)
    integer,intent(in)      ::  metodo
    
    integer     ::  i, j, n, d, ajuste
    REAL*8,allocatable  ::  Q(:,:), R(:,:), Aux(:,:)
    
    n = size(A,1)
    allocate(Aux(n,n))
    allocate(Q(n,n))
    allocate(R(n,n))
    R = 0
    Q = 0
    ajuste = 0
    Aux = A


    !Iteramos con A(k+1) = RQ hasta obtener una matriz triangular
    do
        !Paramos si obtenemos una matriz triangular
        d = 0
        do i = 1,n
            do j = 1,n
                if(j<i.AND.abs(Aux(i,j))>10.d0**(-7)) d = d + 1
            enddo
        enddo
        
        if(d==0) then
            do i = 1,n
                autoval(i) = Aux(i,i)
            enddo
            deallocate(Q)
            deallocate(R)
            deallocate(Aux)
            return
        endif

        if(metodo==1) call factorQRGramSchmidt(Aux,Q,R)
        if(metodo==2) call factorQRHouseholder(Aux,Q,R)
        Aux = matmul(R,Q)
    enddo
end subroutine

!!!Subrutina que mediante el método de la potencia calcula el autovalor de mayor valor absoluto, y el autovector asociado
subroutine metpotencia(A,autoval,autovector)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  autoval, autovector(:)

    integer             ::  i, n
    integer,parameter   ::  max_iter = 100000
    REAL*8,allocatable  ::  aux(:)
    REAL*8              ::  error
    REAL*8,parameter    ::  tol = 10.d-7

    n = size(A,1)
    allocate(aux(n))

    autovector=1.d0/sqrt(real(n,8))
    aux = autovector

    do i=1,max_iter
        autovector=matmul(A,autovector)
        autovector=autovector/norma2(autovector)
        error = norma2(autovector-aux)

        if (error<tol) then
            EXIT
        else
            aux=autovector
        endif
    enddo

    autoval=(dot_product(autovector,(matmul(A,autovector))))/(dot_product(autovector,autovector))
end subroutine

!!!Subrutina que mediante el método de la potencia calcula el autovalor más cercano a 0, y el autovector asociado
subroutine metpotenciainv(A,autoval,autovector)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  autoval, autovector(:)

    integer             ::  i, n
    integer,parameter   ::  max_iter = 100000
    REAL*8,allocatable  ::  AB(:,:), aux(:)
    REAL*8              ::  error
    REAL*8,parameter    ::  tol = 10.d-7

    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(aux(n))

    autovector=1.d0/sqrt(real(n,8))
    AB(:,1:n) = A(:,:)

    do i=1,max_iter
        AB(:,n+1) = autovector(:)
        call metgauss(AB,aux)
        aux=aux/norma2(aux)
        error = norma2(aux-autovector)
        if (error<tol.AND.mod(i,500)==0) then
            EXIT
        else
            autovector=aux
        endif
    enddo

    autoval=(dot_product(autovector,(matmul(A,autovector))))/(dot_product(autovector,autovector))
end subroutine

!!!Subrutina que calcula los autovalores de una matriz. Altamente optimizable...
subroutine metshift(A,autovalores,autovectores)
    REAL*8,intent(in)       ::  A(:,:)
    REAL*8,intent(inout)    ::  autovalores(:), autovectores(:,:)

    integer     ::  i, j, n, m
    REAL*8,allocatable  ::  Aux(:,:)
    REAL*8      ::  beta, k1, k2

    n = size(A,1)
    allocate(Aux(n,n))

    call metpotencia(A,autovalores(n),autovectores(n,:))
    autovalores(:) = autovalores(n)
    call metpotenciainv(A,autovalores(1),autovectores(1,:))
    beta = abs(autovalores(n))
    i = n

    do  
        i = i-1
        if(i==1) EXIT
        Aux = A
        beta = beta-0.1         !0.1 es un valor arbitrario, pequeño para poder recoger todos los autovalores, no demasiado para hacer menos operaciones
        do j = 1,n
            Aux(j,j) = Aux(j,j) -beta
        enddo  
        call metpotenciainv(Aux,autovalores(i),autovectores(i,:))
        autovalores(i) = autovalores(i)+beta

        do j = 1,n
            if(abs(autovalores(i))>abs(autovalores(n)).OR.abs(autovalores(i))<abs(autovalores(1))) then
                i = i + 1
                EXIT
            endif
            if(abs(abs(autovalores(i))-abs(autovalores(j)))<10.d0**(-4).AND.j/=i) then
                i = i+1
                EXIT
            endif
        enddo
        if(beta<-abs(autovalores(n))) EXIT
    enddo

end subroutine


!!!Subrutina que ajusta curvas de regresión de grado n por mínimos cuadrados
subroutine regremin(Puntos,n,R)
    REAL*8,intent(in)       ::  Puntos(:,:)
    REAL*8,intent(inout)    ::  R(:)
    integer,intent(in)      ::  n

    integer     ::  i, j, ndots
    REAL*8,allocatable  ::  A(:,:), B(:), At(:,:), Ap(:,:), Bp(:)

    ndots = size(Puntos,1)
    allocate(A(ndots,n+1))
    allocate(At(ndots,n+1))
    allocate(B(ndots))
    allocate(Ap(n+1,n+1))
    allocate(Bp(n+1))

    !Rellenamos las matrices apartir de los datos que obtenemos de los puntos
    do j = 1,n+1
        do i = 1,ndots
            A(i,j) = Puntos(i,1)**(j-1)
        enddo
    enddo
    B(:) = Puntos(:,2)
    At(:,:) = A(:,:)

    call trasp(At)

    !Calculamos y resolvemos (AtA)X*=AtB
    Ap = matmul(At,A)
    Bp = matmul(At,B)
    deallocate(At)
    allocate(At(n+1,n+2))
    At(:,1:n+1) = Ap(:,:)
    At(:,n+2) = Bp(:)

    call metgauss(At,R)
end subroutine

!!!Subrutina que calcula el determinante de una matriz cuadrada
subroutine determinante(A,det)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  det

    integer ::  n, i, j, m
    REAL*8  ::  h
    REAL*8,allocatable  ::  C(:,:)

    !A es la matriz original, det el determinante y el resto son variables locales auxiliares
    det = 1
    n = size(A,1)
    if(n/=size(A,2)) then
        write(*,*) "Error: La matriz introducida no es cuadrada"
        return
    endif
    allocate(C(n,n))
    C(:,:) = A(:,:)

    !Triangulación inferior    
    do j = 1,n
        do m = j,n
            if(abs(C(j,j))<abs(C(m,j))) then
                C(j,:) = C(j,:) + C(m,:)
                C(m,:) = C(j,:) - C(m,:)
                C(j,:) = C(j,:) - C(m,:)
                det = det*(-1)
            endif
        enddo
        do i = j+1,n
            if(j==n) EXIT
            if(abs(C(j,j))<EPSILON(1.d0)) EXIT
            h = C(i,j)/C(j,j)
            C(i,:) = C(i,:) -h*C(j,:)
        enddo
    enddo

    !Obtención del determinante (producto de los elementos de la diagonal principal)
    do j = 1,n
        det = det*C(j,j)
    enddo
    if(abs(det)<EPSILON(1.d0)) det = 0

end subroutine


!!!Subrutina que calcula la inversa de una matriz cuadrada invertible
subroutine inversa(A)
    REAL*8,intent(inout)    ::  A(:,:)

    integer ::  n, i, j, m
    REAL*8  ::  h, det
    REAL*8,allocatable  ::  AI(:,:)

    !A es la matriz original, AI es A acoplada con una matriz identidad y el resto son variables locales auxiliares
    n = size(A,1)
    if(n/=size(A,2)) then
        write(*,*) "Error: La matriz introducida no es cuadrada"
        return
    endif
    call determinante(A,det)
    if(det==0) then
        write(*,*) "Error: La matriz introducida no es invertible"
        return
    endif

    allocate(AI(n,2*n))
    AI = 0
    AI(:,1:n) = A(:,:)
    do i = 1,n
        AI(i,n+i) = 1
    enddo

    !Triangulación inferior    
    do j = 1,n
        do m = j,n
            if(abs(A(j,j))<abs(A(m,j))) then
                AI(j,:) = AI(j,:) + AI(m,:)
                AI(m,:) = AI(j,:) - AI(m,:)
                AI(j,:) = AI(j,:) - AI(m,:)
            endif
        enddo
        do i = j+1,n
            if(j==n) EXIT
            if(abs(AI(j,j))<EPSILON(1.d0)) EXIT
            h = AI(i,j)/AI(j,j)
            AI(i,:) = AI(i,:) -h*AI(j,:)
        enddo
    enddo

    !Triangulación superior
    do j = n,1,-1
        do i = j-1,1,-1
            if(j==1) EXIT
            h = AI(i,j)/AI(j,j)
            AI(i,:) = AI(i,:) -h*AI(j,:)
        enddo
    enddo
    !Hacemos unos en la diagonal principal y extraemos A^-1 de la parte derecha de AI
    do i = 1,n
        AI(i,:) = AI(i,:)/AI(i,i)
    enddo
    A(:,:) = AI(:,n+1:2*n)

end subroutine

!!!Subrutina específica para el problema del calor
subroutine metodoscalor(AB,x)
    REAL*8,intent(in)       ::  AB(:,:)
    REAL*8,intent(inout)    ::  x(:)

    integer ::  i, n
    REAL*8,allocatable  ::  A(:,:)

    n = size(AB,1)
    allocate(A(n,n))
    A(:,:) = AB(:,1:n)

    !Métodos iterativos (Gauss-seidel por ejemplo)
    write(*,*) "Resolviendo sistema por el metodo iterativo de Gauss-Seidel"    
    call metgaseidel(AB,x,i)  !Condición de convergencia --> Radio espectral estrictamente menor que 1
    if(i==0) then
        return
    endif
    write(*,*) "Problema de convergencia, intentando con LU sin pivote..."

    !Métodos directos
    call metlu(AB,x,i)           !LU sin pivote, devuelve i = 1 si ha necesitado hacer uno
    if(i==1) then
        write(*,*) "Necesidad de pivote, resolviendo por Gauss"
        x = 0
        call metgauss(AB,x)      !Y luego por tanto resuelve el sistema por Gauss
    endif
end subroutine


!!!Herramientas auxiliares
subroutine trasp(A) !Traspuesta
    REAL*8,allocatable,intent(inout)    ::  A(:,:)

    integer ::  fil, col, i
    REAL*8,allocatable  ::  Aux(:,:)

    fil = size(A,1)
    col = size(A,2)
    allocate(Aux(col,fil))

    do i = 1,fil
        Aux(:,i) = A(i,:)
    enddo
    
    deallocate(A)
    allocate(A(col,fil))
    A(:,:) = Aux(:,:)
    deallocate(Aux)
end subroutine

function radio_espectral(A) !Función que calcula el radio espectral de una matriz
    REAL*8,intent(in)   ::  A(:,:)
    REAL*8  ::  radio_espectral

    REAL*8,allocatable  ::  autovalores(:)
    integer             ::  i, n

    n = size(A,1)
    allocate(autovalores(n))

    call algoritmoQR(A,autovalores,2)
    radio_espectral = abs(autovalores(1))
    do i = 2,n
        if(abs(autovalores(i))>radio_espectral) radio_espectral = abs(autovalores(i)) 
    enddo
end function

function norma2(vector) !Norma euclídea de un vector
    REAL*8,intent(in)   ::  vector(:)
    REAL*8  ::  norma2

    norma2 = 0.d0
    norma2 = dot_product(vector,vector)
    norma2 = sqrt(norma2)
end function

subroutine espera(x) !Esta subrutina detiene la ejecucion del programa hasta que pasen x segundos
    implicit none
    real, intent(in) :: x
    real :: t,t1
    CALL CPU_TIME(t)
    do
        CALL CPU_TIME(t1)
        if (t1-t > x) exit
    enddo
end subroutine espera

end module