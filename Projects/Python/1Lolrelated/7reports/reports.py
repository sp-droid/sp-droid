import pydirectinput as pyd
import pyautogui as pya
import pyperclip
import time

pyd.PAUSE = 0.000
pya.PAUSE = 0.000
interval = 0.02

message = 'This player was inting, possibly cheating, using scripts, saying racial slurs, homophobic comments, transphobic comments, misogynistic comments. This player ran it down mid, top and bot. This player intentionally died multiple times.'
pyperclip.copy(message)

time.sleep(8)
pos = pyd.position()
#print(pos[0]-1080)

xPos = [0,0,0,0,0,0,0,90,200]
yPos = [0,42,102,148,192,238,280,344,406]
playerXpos = -160
playerYpos = [-6,30,72,112,190,232,272,312,356]

for player in range(10):
    if player!=0:
        pyd.click(*(pos[0]+playerXpos, pos[1]+playerYpos[player-1]))
        time.sleep(interval*10)
    for i in range(8):
        pyd.click(*(pos[0]+xPos[i], pos[1]+yPos[i]))
        time.sleep(interval)

    time.sleep(interval*5)
    pya.hotkey('ctrl', 'v')
    time.sleep(interval*5)

    pyd.click(*(pos[0]+xPos[8], pos[1]+yPos[8]))
    time.sleep(interval*10)