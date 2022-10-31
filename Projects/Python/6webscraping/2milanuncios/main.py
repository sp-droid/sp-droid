from pygame import mixer
import time
from bs4 import BeautifulSoup
import requests

# Alarm rute, volume and playtime (seconds)
def play_sound():
    mixer.init()
    alarm = mixer.Sound('alert_sound.mp3')
    alarm.set_volume(0.6)
    alarm.play()

# Time between searches, in minutes
#timeinterval = int(input('Specify time between searches in minutes:\n'))
timeinterval = 3

# URL of the website
url = 'https://www.milanuncios.com/tarjetas-graficas/?desde=100&hasta=220&demanda=n&orden=date&fromSearch=1'
print('Looking for deals at:\n'+url)

offerlist = []
while True:
    # Webscraping (since the page has no javascript we don't need to render it with a browser)
    page = requests.get(url)

    # Save HTML
    html = page.content
    soup = BeautifulSoup(html, 'html.parser')

    for elem in soup.find_all('article', class_='ma-AdCard'):
        name = elem.find_all('a', class_='ma-AdCard-titleLink')[0].text
        if name not in offerlist:
            time.sleep(1)
            print('NEW OFFER:\n\t'+name)
            print('\t'+elem.find_all('span', class_='ma-AdPrice-value ma-AdPrice-value--default ma-AdPrice-value--heading--m')[0].text)
            print('\t'+time.ctime())
            play_sound()
            offerlist.append(name)

    time.sleep(int(60*timeinterval))