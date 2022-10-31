module funciones_zombies

implicit none

contains

function zombies1(x,m) !Sistema de ecuaciones diferenciales, x=x(1),y1=x(2),y2=x(3)
    integer     ::  m
    REAL*8      ::  zombies1(m), x(m+1), a, b, g, i, o

    open(10,file="Parametros\Parametros1.txt")
        read(10,*) a
        read(10,*) b
        read(10,*) g
        read(10,*) i
        read(10,*) o
    close(10)

    zombies1(1) = a-g*x(2)*x(3)-b*x(2)
    zombies1(2) = g*x(2)*x(3)+o*x(4)-i*x(2)*x(3)
    zombies1(3) = b*x(2)+i*x(2)*x(3)-o*x(4)

end function

function zombies2(x,m) !Sistema de ecuaciones diferenciales, x=x(1),y1=x(2),y2=x(3)
    integer     ::  m
    REAL*8      ::  zombies2(m), x(m+1), a, g, i, b, o, f, k, l, p
    
    open(10,file="Parametros\Parametros2.txt")
        read(10,*) a
        read(10,*) g
        read(10,*) i
        read(10,*) b
        read(10,*) o
        read(10,*) f
        read(10,*) p
        read(10,*) l
        read(10,*) k
    close(10)

    !a=Tasa de natalidad
    !b=Tasa de mortalidad natural
    !f=Tasa de conversión infectados a zombis
    !g=Tasa de conversión de humanos a zombis
    !i=Tasa de muerte de zombis
    !k=Infectados en cuarentena que intentan huir y mueren
    !l=Tasa de infectados que entran en cuarentena
    !o=Tasa de cinversión de muertos a zombis
    !p=Tasa de zombis que entran en cuarentena

    zombies2(1) = a-g*x(2)*x(3)-b*x(2) !S'
    zombies2(2) = f*x(5)+o*x(4)-i*x(2)*x(3)-k*x(3)!Z'
    zombies2(3) = b*x(2)+b*x(5)+i*x(2)*x(3)-o*x(4)+p*x(6)!R'
    zombies2(4) = g*x(2)*x(3)-(f+b+l)*x(5) !I'
    zombies2(5) = l*x(5)+k*x(3)-p*x(6) !Q'

end function

function zombies3(x,m) !Sistema de ecuaciones diferenciales, x=x(1),y1=x(2),y2=x(3)
    integer     ::  m
    REAL*8      ::  zombies3(m), x(m+1), a, b, d, e, f, g, i, k, l, n, o
    
    REAL*8      ::  c

    c = 1.2**(x(1))
    open(10,file="Parametros\Parametros3.txt")
        read(10,*) a
        read(10,*) b
        read(10,*) d
        read(10,*) e
        read(10,*) f
        read(10,*) g
        read(10,*) i
        read(10,*) k
        read(10,*) l
        read(10,*) n
        read(10,*) o
    close(10)

    !a=Tasa de natalidad
    !b=Tasa de mortalidad natural
    !c=efectividad de la vacuna
    !d=tasa de vacunacion humanos
    !e=tasa de vacunacion infectados
    !f=Tasa de conversión infectados a zombis
    !g=Tasa de asesinatos de humanos por zombis
    !i=Tasa de muerte de zombis
    !k=Zombis puestos en cuarentena
    !l=Tasa de infectados que entran en cuarentena
    !n=Tasa de infeccion
    !o=Tasa de conversión de muertos a zombis
    !p=Tasa de zombis que entran en cuarentena

    zombies3(1)=o*x(5)+g*x(3)*x(2)+f*x(6)+k*x(7)-i*x(2)*x(3)-i*x(2)*x(4)
    zombies3(2)=a-b*x(3)-c*d*x(3)-g*x(3)*x(2)-n*x(3)
    zombies3(3)=-g*x(4)*x(2)-b*x(4)+c*d*x(7)+c*d*x(3)+c*e*x(6)
    zombies3(4)=-o*x(5)+g*x(4)*x(2)+b*x(3)+b*x(4)+i*x(2)*x(3)+i*x(2)*x(4)
    zombies3(5)=-c*e*x(6)+n*x(3)-l*x(6)-f*x(6)
    zombies3(6)=-c*d*x(7)+l*x(6)-k*x(7)

end function

end module funciones_zombies