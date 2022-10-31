module algebra_lineal

contains

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de Gauss
subroutine metgauss(A,R)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)

    integer ::  fil, col, i, j, m, n
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:)

    !AB es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    fil = size(A,1)
    col = size(A,2) - 1
    allocate(AB(fil,col+1))
    R = 0
    AB(:,:) = A(:,:)

    if(col>fil) then
        write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
        return
    endif

    !Triangulación inferior    
    do j = 1,col
        do m = j,fil
            if(abs(AB(j,j))<EPSILON(1.d0)) AB(j,j) = 0
            if(abs(AB(j,j))<abs(AB(m,j)).AND.abs(maxval(abs(AB(j:fil,j)))-abs(AB(m,j)))<EPSILON(1.d0)) then
                AB(j,:) = AB(j,:) + AB(m,:)
                AB(m,:) = AB(j,:) - AB(m,:)
                AB(j,:) = AB(j,:) - AB(m,:)
            endif
        enddo
        if(abs(AB(j,j))<EPSILON(1.d0)) then
            write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
            return
        endif
        do i = j+1,fil
            if(j==fil) EXIT
            h = AB(i,j)/AB(j,j)
            AB(i,:) = AB(i,:) -h*AB(j,:)
        enddo
    enddo

    !Obtención de las soluciones del sistema
    R = 0
    do m = col,1,-1
        do n = col,m+1,-1
            if(m==col) EXIT
            R(m) = R(m) + R(n)*AB(m,n)
        enddo
        R(m) = (AB(m,col+1)-R(m))/AB(m,m)
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

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de LU
subroutine metLU(A,R)
    implicit none
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,allocatable,intent(inout)    ::  R(:)
    
    real(8),allocatable ::  L(:,:),U(:,:),LINV(:,:),UINV(:,:),A1(:,:),B(:)
    integer             ::  i, j, k, n, maxi, h
    REAL*8              ::  SUM1, SUM2, SUM3
    REAL*8,allocatable  ::  Y(:),C(:),V(:),AB(:,:)
    integer :: maximo(1)
        
    n=size(A,1)
        
    allocate(A1(n,n)); allocate(AB(n,n+1));allocate(V(n+1))
    allocate(L(n,n)); allocate(U(n,n)); allocate(Y(n))
    allocate(LINV(n,n)); allocate(UINV(n,n)); allocate(B(n))
        
    AB=A
        
    do i=1,n
        allocate(C(n-i+1))
        do h=i,n
            C(h-i+1)=AB(h,i)
        enddo
        C=abs(C)
        maximo=maxloc(C)
        maxi=maximo(1)-1
        deallocate(C)
        V=AB(i+maxi,:)
        AB(i+maxi,:)=AB(i,:)
        AB(i,:)=V
    enddo
        
        
    do i=1,n
        do j=1,n
            A1(i,j)=AB(i,j)
        enddo
            B(i)=AB(i,n+1)
    enddo
        
        !factorizacion
       L=0
       U=0
           
       L(1,1)=A1(1,1)
       U(1,1)=1.d0
    
            
        do k=1,n-1
            U(1,k+1)=A1(1,k+1)/L(1,1)
            do i=2,k
                SUM1=0
               do j=1,i-1
                   SUM1=SUM1+(L(i,j)*U(j,k+1))
               enddo
               U(i,k+1)=(A1(i,k+1)-SUM1)/L(i,i)
            enddo
            L(k+1,1)=A1(k+1,1)   
            do i=2,k
               SUM2=0
                do j=1,i-1
                   SUM2=SUM2+(U(j,i)*L(k+1,j))
               enddo
               L(k+1,i)=A1(k+1,i)-SUM2
            enddo
            U(k+1,k+1)=1    
            SUM3=0
            do i=1,k
                SUM3=SUM3+(L(k+1,i)*U(i,k+1))
            enddo
            L(K+1,k+1)=(A1(k+1,k+1))-SUM3
        enddo
            
    LINV=L
    UINV=U
    call INVERSA(LINV)
    call INVERSA(UINV)
        
    do i=1,n
        do j=1,n
            if (abs(LINV(i,j))<epsilon(1.0)) LINV(i,j)=0.d0
            if (abs(UINV(i,j))<epsilon(1.0)) UINV(i,j)=0.d0
        enddo
    enddo
        
    Y=matmul(LINV,B)
    R=matmul(UINV,Y)
end subroutine

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método iterativo de Jacobi
subroutine metjacobi(A,R)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)

    integer ::  n, i, j, iter
    REAL*8  ::  h
    REAL*8,allocatable  ::  Raux(:), Raux2(:), AB(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(Raux(n))
    allocate(Raux2(n))
    AB(:,:) = A(:,:)
    Raux(:) = R(:)

    !Vemos si la matriz tiene 0 en la diagonal o si no es diagonalmente dominante, condición suficiente que usaremos este curso para determinar
    !si el método iterativo va a ser convergente o no, en lugar de calcular el radio espectral/autovalores
    do i = 1,n
        h = 0
        do j = 1,n
            if(j/=i) then
                h = h + abs(AB(i,j))
            endif
        enddo
        if(h>abs(AB(i,i))) then
            write(*,*) "La matriz introducida no es diagonalmente dominante, saliendo..."
            return
        endif
    enddo

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
            write(*,*) "Iteraciones:", iter
            return
        endif
        Raux(:) = R(:)
    enddo

end subroutine

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método iterativo de Gauss-Seidel
subroutine metgaseidel(A,R)
    REAL*8,intent(in)    ::  A(:,:)
    REAL*8,intent(inout)    ::  R(:)

    integer ::  n, i, j, iter
    REAL*8  ::  h
    REAL*8,allocatable  ::  Raux(:), Raux2(:), AB(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    n = size(A,1)
    allocate(AB(n,n+1))
    allocate(Raux(n))
    allocate(Raux2(n))
    AB(:,:) = A(:,:)
    Raux(:) = R(:)

    !Vemos si la matriz tiene 0 en la diagonal o si no es diagonalmente dominante, condición suficiente que usaremos este curso para determinar
    !si el método iterativo va a ser convergente o no, en lugar de calcular el radio espectral/autovalores
    do i = 1,n
        h = 0
        do j = 1,n
            if(j/=i) then
                h = h + abs(AB(i,j))
            endif
        enddo
        if(h>abs(AB(i,i))) then
            write(*,*) "La matriz introducida no es diagonalmente dominante, saliendo..."
            return
        endif
    enddo

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
            write(*,*) "Iteraciones:", iter
            return
        endif
        write(*,*) h
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
    
    integer     ::  i, j, n, d
    REAL*8,allocatable  ::  Q(:,:), R(:,:), Aux(:,:)
    
    n = size(A,1)
    allocate(Aux(n,n))
    allocate(Q(n,n))
    allocate(R(n,n))
    R = 0
    Q = 0
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
            if(abs(C(j,j))<EPSILON(1.d0)) C(j,j) = 0
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
            if(abs(A(j,j))<EPSILON(1.d0)) AI(j,j) = 0
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

function norma2(vector) !Norma euclídea de un vector
    REAL*8,intent(in)   ::  vector(:)
    REAL*8  ::  norma2

    norma2 = 0.d0
    norma2 = dot_product(vector,vector)
    norma2 = sqrt(norma2)
end function

subroutine espera(x)
    !Esta subrutina detiene la ejecucion del programa hasta que pasen x segundos
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