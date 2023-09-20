import json
import time
from pathlib import Path

import pyperclip
import pyautogui as pya
import pydirectinput as pyd

pyd.PAUSE = 0.000
pya.PAUSE = 0.000
interval = 0.02

jsonPath = Path(__file__).parent.resolve() / 'accountDetails.json'

# One time detail input
if Path(jsonPath).exists():
    with open(jsonPath, mode='r') as file:
        details = json.load(file)
else:
    print('(One time only) please input your account id and password to access ESA Public')
    details = {}
    details['id'] = input('Account id:\n')
    details['password'] = input('Account password:\n')

    if details['id'] == '' or details['password'] == '': raise ValueError("Fields can't be empty")

    with open(jsonPath, mode='w') as file:
        json.dump(details, file, indent=2)
        print(f'File saved to: {jsonPath}')
    print('If you want to change them, edit the new accountDetails.json file or delete it and restart')

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
time.sleep(0.3)
pya.hotkey('super','up')
time.sleep(0.3)
pya.hotkey('super','up')
time.sleep(0.3)

# Enter logging address
pyd.click(222, 65)
pyperclip.copy('http://www.msftconnecttest.com/redirect')
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)
pya.hotkey('enter')

time.sleep(2)
condition = True
while condition:
    time.sleep(0.5)
    r, g, b = pya.pixel(155, 185)
    if b > 110 and b < 200 and g > 40 and g < 130 and r > 20 and r < 80: condition = False

# Enter credentials
pyd.click(836, 367)
time.sleep(0.1)
pyperclip.copy(details['id'])
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)
pyd.click(845, 449)
time.sleep(0.1)
pyperclip.copy(details['password'])
time.sleep(0.1)
pya.hotkey('ctrl', 'v')
time.sleep(0.1)

# Accept conditions and log in
pyd.click(798, 909)
time.sleep(0.15)
pyd.click(949, 965)
time.sleep(0.1)