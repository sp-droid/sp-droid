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
    !Etapa sustitucion 
    do i=m,1,-1 
        h=Ab(i,m+1)     !Guardo en h el valor de la columna ampliada
        do j=i+1,m 
            h=h-(Ab(i,j)*x(j))   !Resto los productos de x´s ya calculados
        enddo 
        
        x(i)=h/(Ab(i,i))   !Divido por la diagonal 
    enddo
end subroutine

function norma2(vector) !Norma euclídea de un vector
    REAL*8,intent(in)   ::  vector(:)
    REAL*8  ::  norma2

    norma2 = 0.d0
    norma2 = dot_product(vector,vector)
    norma2 = sqrt(norma2)
end function

end module