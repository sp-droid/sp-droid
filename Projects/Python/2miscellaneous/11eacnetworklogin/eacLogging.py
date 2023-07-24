import pydirectinput as pyd
import pyautogui as pya
import pyperclip
import time

pyd.PAUSE = 0.000
pya.PAUSE = 0.000
interval = 0.02

time.sleep(1)

# Open browser
pya.hotkey('super')
time.sleep(1)
pyperclip.copy('Google Chrome')
time.sleep(0.5)
pya.hotkey('ctrl', 'v')
time.sleep(0.5)
pya.hotkey('enter')
time.sleep(2)
pya.hotkey('super','up')
time.sleep(1)

# Enter logging address
pyd.click(222, 65)
pyperclip.copy('http://www.msftconnecttest.com/redirect')
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)
pya.hotkey('enter')
time.sleep(6)

# Enter credentials
pyd.click(836, 367)
time.sleep(0.1)
pyperclip.copy('6n611cdp')
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)
pyd.click(845, 449)
time.sleep(0.1)
pyperclip.copy('33vm5uq2')
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)

# Accept conditions and log in
pyd.click(798, 909)
time.sleep(0.15)
pyd.click(949, 965)
time.sleep(0.1)