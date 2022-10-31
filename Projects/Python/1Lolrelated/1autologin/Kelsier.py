import time, subprocess
from pynput.keyboard import Key, Controller

keyboard = Controller()

subprocess.call(['Q:\Programas\Juegos\League of Legends\LeagueClient.exe'])

time.sleep(3)

keyboard.type('BF3spxd')
keyboard.press(Key.tab)
keyboard.release(Key.tab)
keyboard.type('gozilla1500')
keyboard.press(Key.enter)
keyboard.release(Key.enter)