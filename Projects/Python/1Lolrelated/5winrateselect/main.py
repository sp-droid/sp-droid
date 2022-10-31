#endpoints from http://www.mingweisamuel.com/lcu-schema/tool/#/
#tool from https://lcu-driver.readthedocs.io/en/latest/examples.html#change-summoner-icon

from lcu_driver import Connector
import lol_library
import time
import os

connector = Connector()
@connector.ready
async def connect(connection):
    print('LCU API is ready to be used.')

@connector.close
async def disconnect(_):
    print('The client has been closed!')
    await connector.stop()

positions = [None]*5
once = False

@connector.ws.register('/lol-champ-select/v1/session', event_types=('UPDATE',))
async def champ_select_change(connection, event):
    global once, players, playerspecific, matchhistory, positions, champs
    champs = [None]*5
    _ = os.system('cls')

    if once==False:
        players = [None]*5
        print('Retrieving summoner IDs and positions')
        for i in range(5):
            players[i] = event.data['myTeam'][i]['summonerId']
            data = await connection.request('GET', '/lol-summoner/v1/summoners/'+str(players[i]))
            summoner = await data.json()
            players[i] = summoner['displayName']

            positions[i] = event.data['myTeam'][i]['assignedPosition']
            if positions[i] == 'utility': positions[i] = 'supp'
            elif positions[i] == 'jungle': positions[i] = 'jung'
            elif positions[i] == 'middle': positions[i] = 'mid'
            elif positions[i] == 'bottom': positions[i] = 'adc'

        print('Player list:')
        print(players)
        print('Recovering history')
        playerspecific, matchhistory = lol_library.leaguecollect(players, 12, targetstamp=0)
        once = True

    print('Champ select change detected')
    for i in range(5):
        champs[i] = event.data['myTeam'][i]['championPickIntent']
        if champs[i] == 0: champs[i] = event.data['myTeam'][i]['championId']
    champs = lol_library.transformchamps(champs)
    print('Champs list: ')
    print(champs)

    X = lol_library.inftreatment(playerspecific, matchhistory, positions, champs)
    Y = lol_library.estimate(X)

    print('Estimated chance of winning: '+str(Y)+'%')
    time.sleep(2)

@connector.ws.register('/lol-matchmaking/v1/search', event_types=('UPDATE',))
async def queue_listener(connection, event):
    temp = int(event.data['timeInQueue'])
    _ = os.system('cls')

    print('In queue for '+str(temp)+' seconds')
    if temp>180: print('bruh')

@connector.ws.register('/lol-matchmaking/v1/ready-check', event_types=('UPDATE',))
async def ready_schheck(connection, event):
    global once
    _ = os.system('cls')
    once = False
    temp = int(event.data['timer'])

    print('Ready schhecking for '+str(temp)+' seconds')

@connector.ws.register('/lol-end-of-game/v1/champion-mastery-updates', event_types=('UPDATE',))
async def endgame_listener(connection, event):

    print('Game ended')
    connector.close()

connector.start()

lol_library.aftergame(players, champs, positions, playerspecific, matchhistory)