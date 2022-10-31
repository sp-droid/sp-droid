from pygame import mixer
import time

from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

# Chromedriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
service = Service(ChromeDriverManager().install())

# Headless Chrome
options = webdriver.chrome.options.Options()
options.headless = True

# Alarm rute, volume and playtime (seconds)
volume = 0.8
maxtime = 30
alarmrute = 'war_alarm.mp3'

# Time between searches, in minutes
timeinterval = int(input('Specify time between searches in minutes:\n'))

# URL of the dynmap
url = 'http://geographica.xyz/map/'
print('Dynmap: '+url)

huntedplayer = input('Type the exact username you are hunting:\n')

while True:
    print('Looking for '+huntedplayer+' ...')
    # Open URL in HEADLESS browser
    browser = webdriver.Chrome(service=service, options=options)
    browser.get(url)

    # Switch to the first (and only) iframe that hides information
    browser.switch_to.frame(0)

    # Wait for the page to fully load (i dont think its possible since it's updating all the time) or 10 seconds
    try:
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.CLASS_NAME, 'clips-cards ')))
    except TimeoutException:
        print('Finished')

    # Save HTML
    html = browser.page_source
    browser.quit()
    soup = BeautifulSoup(html, 'html.parser')

    playerlist = []
    for player in soup.find_all('li', class_='player'):
        try:
            string = str(player.find('a')).split('</a>')[0].split('">')[1]
            if ' ' in string: string = string.split(' ')[1]
            playerlist.append(string)
        except:
            print('An exception occurred:\n'+str(print(player)))

    if huntedplayer in playerlist:
        print(huntedplayer+' found')
        mixer.init()
        alarm = mixer.Sound(alarmrute)
        alarm.set_volume(volume)
        alarm.play()
        time.sleep(maxtime)
        break
    else: 
        print('Next search in '+str(timeinterval)+' minutes')
        time.sleep(int(60*timeinterval))
