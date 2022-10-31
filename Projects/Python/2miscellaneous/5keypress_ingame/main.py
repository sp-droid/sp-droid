import pydirectinput
import time

def hold_key (hold_time, key):
    print('Holding key "'+key+'"')
    
    pydirectinput.keyDown(key)
    time.sleep(hold_time)
    pydirectinput.keyUp(key)
    
    print('Released')

print('Starting program test')

time.sleep(4)
hold_key(2, 'w')


input()