import pyautogui
import time

pyautogui.PAUSE = 0.01

#Declaración de variables
pos = [[] for i in range (2)]
sumargb0 = int(141+3+123)
npuntos = 0

#Construir la lista de puntos
while True:
    clave = input('')
    x, y = pyautogui.position()
    pos[0].append(x)
    pos[1].append(y)
    print(pos[0][npuntos],pos[1][npuntos])
    npuntos = npuntos + 1
    if clave == 'A':
        break

#Conseguir los colores y pintar donde no haya nada
input('Pon el cursor fuera de los países')
#for j in range(100):
while True:
    for i in range(npuntos):
        start_time = time.time()
        r, g, b = pyautogui.pixel(pos[0][i], pos[1][i])
        sumargb = int(r + g + b)
        print(sumargb)
        if sumargb != sumargb0:
            pyautogui.doubleClick(pos[0][i], pos[1][i])
        print("--- %s seconds ---" % (time.time() - start_time))
    
input('')