import pyautogui
import time

#Declaración de variables
x = 1287
y = 380
sumargb0 = int(86)

start = input('')
pyautogui.click(x, y)

for i in range(0,10000000000000000):
    r, g, b = pyautogui.pixel(x, y)
    if r == sumargb0:
        pyautogui.click(x, y)
        break
