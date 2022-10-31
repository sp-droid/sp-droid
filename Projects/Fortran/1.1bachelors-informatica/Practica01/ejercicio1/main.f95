program ejercicio1

implicit none

    integer :: a
    integer :: b
    real :: c

    call system("cls")

    a = 1
    b = 2
    c = a + b

    write (* ,*) a * b
    write (* ,*) a / b
    write (* ,*) b * c
    write (* ,*) a / c
    write (* ,*)
    write (* ,*) c **( a / b )
    write (* ,*) c ** a / b
    write (* ,*)
    write (* ,*) c /( b -2* a)
    write (* ,*)
    write (* ,*) c /( b - a /2)
    write (* ,*) c /( b - a /2.0)
    write (* ,*)
    
end program ejercicio1