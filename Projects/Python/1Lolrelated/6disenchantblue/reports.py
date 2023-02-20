import pydirectinput as pyd
import time

interval = 0.02

time.sleep(4)
pos = pyd.position()
for i in range(1,10):
    if i>4: k = 33
    else: k = 0

    time.sleep(interval)
    pyd.click(*(pos[0], i*42+k+pos[1]))
    time.sleep(interval)
    pyd.click(*(202+pos[0], 77+pos[1]))
    time.sleep(interval)
    pyd.click(*(202+pos[0], 125+pos[1]))
    time.sleep(interval)
    pyd.click(*(202+pos[0], 331+pos[1]))
    time.sleep(interval)
    pyd.click(*(358+pos[0], 440+pos[1]))
