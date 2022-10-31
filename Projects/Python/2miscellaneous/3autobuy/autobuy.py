import pydirectinput as pd
import time

pd.Pause = 0.01

input('Enter para empezar')
time.sleep(0.5)
pd.click(965, 312)
time.sleep(1)


#Abrir el shop
pd.press('enter')
pd.keyDown('shift')
pd.keyDown('7')
time.sleep(0.1)
pd.keyUp('shift','7')
pd.typewrite('shop')
pd.press('enter')

#Comprar madera
pd.click(923, 468)
time.sleep(0.1)
pd.click(962, 397)
time.sleep(0.1)
pd.click(924, 452)
time.sleep(0.1)
pd.click(924, 452)
time.sleep(0.1)
pd.click(1105, 522)
time.sleep(0.1)

while True:
    #Hacer pressure plates de madera
    pd.press('i')
    pd.click(1006, 666)
    pd.click(1148, 455)
    pd.click(1042, 664)
    pd.click(1184, 455)

    for j in range(32):
        pd.doubleClick(1262,440)

    time.sleep(0.1)
    pd.click(1076, 670)
    pd.press('i')

    #Venderlas y comprar madera
    time.sleep(0.1)
    pd.press('enter')
    pd.keyDown('shift')
    pd.keyDown('7')
    time.sleep(0.1)
    pd.keyUp('shift','7')
    pd.typewrite('shop')
    pd.press('enter')
    pd.click(923, 468)
    time.sleep(0.1)
    pd.click(962, 397)
    time.sleep(0.1)
    pd.click(924, 452)
    time.sleep(0.1)
    pd.click(924, 452)
    time.sleep(0.1)
    pd.click(817, 527)
    time.sleep(0.1)
    pd.click(1105, 545)
    time.sleep(0.1)
    pd.click(1105, 545)
    time.sleep(0.1)
    pd.click(1105, 545)
    time.sleep(0.1)
    pd.click(814, 434)
    time.sleep(0.1)
    pd.click(997, 453)
    time.sleep(0.1)
    pd.click(1103, 526)
    time.sleep(0.1)
