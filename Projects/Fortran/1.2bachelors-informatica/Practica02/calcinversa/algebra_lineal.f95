module algebra_lineal

contains

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de Gauss
subroutine metgauss(A,R)
    REAL*8,allocatable,intent(in)    ::  A(:,:)
    REAL*8,allocatable,intent(inout)    ::  R(:)

    integer ::  fil, col, i, j, m, n
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    fil = size(A,1)
    col = size(A,2) - 1
    allocate(AB(fil,col+1))
    AB(:,:) = A(:,:)

    if(col>fil) then
        write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
        allocate(R(1))
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
            allocate(R(1))
            R = 0
            return
        endif
        do i = j+1,fil
            if(j==fil) EXIT
            h = AB(i,j)/AB(j,j)
            AB(i,:) = AB(i,:) -h*AB(j,:)
        enddo
    enddo

    !Obtención de las soluciones del sistema
    allocate(R(col))
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
    REAL*8,allocatable,intent(in)    ::  A(:,:)
    REAL*8,allocatable,intent(inout)    ::  R(:)

    integer ::  fil, col, i, j, m
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    fil = size(A,1)
    col = size(A,2) - 1
    allocate(AB(fil,col+1))
    AB(:,:) = A(:,:)

    if(col>fil) then
        write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
        allocate(R(1))
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
            allocate(R(1))
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
    allocate(R(col))
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

!!!Subrutina que resuelve sistemas de ecuaciones lineales de solución única por el método de factorización LU
subroutine factorLU(A,R)
    REAL*8,allocatable,intent(in)    ::  A(:,:)
    REAL*8,allocatable,intent(inout)    ::  R(:)

    integer ::  fil, col, i, j, m, n
    REAL*8  ::  h
    REAL*8,allocatable  ::  AB(:,:)

    !A es la ampliada, R es el vector con los resultados y el resto son variables locales auxiliares
    fil = size(A,1)
    col = size(A,2) - 1
    allocate(AB(fil,col+1))
    AB(:,:) = A(:,:)

    if(col>fil) then
        write(*,*) "Infinitas soluciones, se trata de un sistema compatible indeterminado"
        allocate(R(1))
        R = 0
        return
    endif


end subroutine


!!!Subrutina que calcula el determinante de una matriz cuadrada
subroutine determinante(A,det)
    REAL*8,allocatable,intent(in)    ::  A(:,:)
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
    REAL*8,allocatable,intent(inout)    ::  A(:,:)

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

end module