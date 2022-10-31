module auxiliar

contains

subroutine factorial(n,facto)
integer,intent(in)  ::  n
REAL*8,intent(out)  ::  facto

integer     ::  j

facto = 1
do j = 1,n
    facto = facto*j
enddo

end subroutine factorial


end module