from requests import api
from riotwatcher import LolWatcher, ApiError
import myML
import pickle
import numpy as np
import time
import os

api_key = 'RGAPI-4b8c11c1-47a0-4097-b59c-9f10dcf69bc1'
my_region = 'euw1'
date = round(time.time())
features = 13

############################################################# Transform player ids into names
def transformplayers(players):
    global api_key, my_region
    watcher = LolWatcher(api_key)

    temp = [None]*5
    for i in range(5):
        time.sleep(1)
        for k in range(10):
            try:
                temp[i] = watcher.summoner.by_id(my_region, players[i])['name']
                break
            except:
                print("Request rate too high")

    return temp

def transformchamps(champs):
    global api_key, my_region
    watcher = LolWatcher(api_key)
    for k in range(10):
        try:
            latest = watcher.data_dragon.versions_for_region(my_region)['n']['champion']
            break
        except:
            print("Request rate too high")
    for k in range(10):
        try:
            static_champ_list = watcher.data_dragon.champions(latest, False, 'en_US')
            break
        except:
            print("Request rate too high")

    temp = [None]*5
    for i in range (5):
        for key in static_champ_list['data']:
            if int(static_champ_list['data'][key]['key']) == champs[i]:
                temp[i] = static_champ_list['data'][key]['id']
                if temp[i]=='MonkeyKing':
                    temp[i] = 'Wukong'
    return temp

############################################################# Save data for better training after game, required to input the win or loss
def aftergame(players, champs, positions, playerspecific, matchhistory):
    global date
    year = date/3600/24/365+1970
    day = (year - int(year))*365+1
    hour = (day - int(day))*24+1
    minn = int((hour - int(hour))*60)

    #Dump results after game to train the network
    Y = input ('Win = 1, Lose = 0 \n')
    mygameid = str(int(year))+'_'+str(int(day))+'_'+str(int(hour))+'-'+str(minn)
    with open('dataset/'+mygameid+'.pkl', 'wb') as f: #wb is necessary, write-binary
        pickle.dump([players, champs, positions, playerspecific, matchhistory, Y], f)
    return

############################################################# Pass the X vector through the neural network, obtaining an estimated chance of winning
def estimate(X):
    nodes = [X.shape[0], 2, 1]
    activation = ['ReLU', 'sigmoid']
    loss = 'logloss'
    model = myML.ANN(nodes, activation, loss)
    DIR = 'myML/NNdata.pkl'

    model.loadlayers(DIR)
    Y = round(model.test(X)[0,0]*100,2)
    return Y

############################################################# Data treatment of the recorded games, producing the X and Y arrays that are used in the neural network
def processgames():
    global features
    DIR = 'Q:/Pablo/Proyectos/2Lolrelated/5winrateselect/dataset'
    m=0
    with os.scandir(DIR) as folder:
        for path in folder:
            m+=1

    X_train = np.zeros((features*5,m))
    Y_train = np.zeros((1,m))
    k=0
    with os.scandir(DIR) as folder:
        for filepath in folder:
            with open(filepath, 'rb') as f: #rb is necessary, read-binary
                _, champs, positions, playerspecific, matchhistory, Y_train[0,k] = pickle.load(f)

            X_train[:,k] = inftreatment(playerspecific, matchhistory, positions, champs)[:,0]
            k+=1

    return X_train, Y_train


############################################################# Automatic game gathering from Master tier players or similar. Tied to the API request limit, very slow
def gathergames():
    global api_key, my_region, date
    watcher = LolWatcher(api_key)
    for k in range(10):
        try:
            latest = watcher.data_dragon.versions_for_region(my_region)['n']['champion']
            break
        except:
            print("Request rate too high")
    for k in range(10):
        try:
            static_champ_list = watcher.data_dragon.champions(latest, False, 'en_US')
            break
        except:
            print("Request rate too high")
    list = watcher.league.masters_by_queue(my_region, 'RANKED_SOLO_5x5')['entries']
    
    for n in range(len(list)):
        #Get an active summoner in Master tier or whatever league we want --> Match info and timestamp
        summoner = list[n]['summonerName']
        if list[n]['inactive'] == True: 
            print('is inactive')
            continue
        print(summoner)
        for k in range(10):
            try:
                summonerid = watcher.summoner.by_name(my_region,summoner)['accountId']
                break
            except:
                print("Request rate too high")
        for k in range(10):
            try:
                matchlist = watcher.match.matchlist_by_account(my_region,summonerid)['matches']
                break
            except:
                print("Request rate too high")   

        targetmatch = matchlist[0]
        match_detail = watcher.match.by_id(my_region, targetmatch['gameId'])

        if match_detail['gameMode'] != 'CLASSIC':   #Continue if it's not on summoner's rift
            print(match_detail['gameMode'])
            continue

        targetstamp = targetmatch['timestamp']
        temp = round((date-targetstamp/1000)/3600)
        if temp>48: continue                        #Continue if it didn't happen 2 days ago or less
        print(str(temp)+' hour/s ago')

        #Check if it's already in the files
        DIR = 'Q:/Pablo/Proyectos/2Lolrelated/winrateselect/dataset'
        with os.scandir(DIR) as folder:
            for filepath in folder:
                temp = os.path.splitext(filepath)[0].split('\\')[1]
                if temp == str(match_detail['gameId']):
                    print(temp)
                    break
        if temp == str(match_detail['gameId']):  continue

        #Get information from the game --> Team names, roles, champs and result
        names = [None]*10
        role = [None]*10
        lane = [None]*10
        ids = [[None]*10 for k in range (2)]
        for i in range (10):
            names[i] = match_detail['participantIdentities'][i]['player']['summonerName']
            ids[0][i] = match_detail['participants'][i]['championId']
            for key in static_champ_list['data']:
                if int(static_champ_list['data'][key]['key']) == ids[0][i]:
                    ids[1][i] = static_champ_list['data'][key]['id']
                    if ids[1][i]=='MonkeyKing':
                        ids[1][i] = 'Wukong'
            role[i] = match_detail['participants'][i]['timeline']['role']
            lane[i] = match_detail['participants'][i]['timeline']['lane']

            if lane[i] == 'TOP': lane[i] = 'top'
            elif lane[i] == 'JUNGLE': lane[i] = 'jung'
            elif lane[i] == 'MID': lane[i] = 'mid'
            elif role[i] == 'DUO_CARRY': lane[i] = 'adc'
            elif role[i] == 'DUO_SUPPORT': lane[i] = 'supp'
            else: lane[i] = 'undefined'

            if summoner==names[i]:
                player = i
        
        if player <= 4:
            players = names[0:5]
            champs = ids[1][0:5]
            positions = lane[0:5]
        else:
            players = names[5:10]
            champs = ids[1][5:10]
            positions = lane[5:10]

        Y = 1 if match_detail['participants'][player]['stats']['win'] == True else 0

        #Get information from previous games
        playerspecific, matchhistory = leaguecollect(players, 12, targetstamp)

        #Save data
        with open('dataset/'+str(match_detail['gameId'])+'.pkl', 'wb') as f: #wb is necessary, write-binary
            pickle.dump([players, champs, positions, playerspecific, matchhistory, Y], f)
    return

############################################################# Statistical analysis of the N games analyzed, producing a vector of features that the neural network can read
def inftreatment(playerspecific, matchhistory, positions, champs):
    global features
    nmatches = len(matchhistory[0])
    matchfeatures = len(matchhistory[0][0])
    X = np.zeros((5*features,1))

    #Some values are rescaled to 0-1 using nmatches to improve numerical stability in the neural network step
    #Summoner level is capped at 1000
    #As the score is capped at 1, it's maximum variance takes place when having games with 0, 1, 0, 1,etc scores, with value --0.25--
    for i in range(5):
        X[i*features,0] = min(playerspecific[i][0]/1000,1)            #Summoner level
        X[i*features+1,0] = playerspecific[i][1]                      #Season winrate

        temp = [[None]*nmatches for k in range(matchfeatures)]
        for j in range(nmatches):
            temp[0][j] = matchhistory[i][j][0]
            temp[1][j] = matchhistory[i][j][1]
            temp[2][j] = 1 if matchhistory[i][j][2] == champs[i] else 0
            temp[3][j] = matchhistory[i][j][3]
            temp[4][j] = 1 if matchhistory[i][j][4] == positions[i] or matchhistory[i][j][4] == 'undefined' else 0
            temp[5][j] = matchhistory[i][j][5]
        
        X[i*features+2,0] = np.mean(temp[0])                          #Averaged relative team score
        X[i*features+3,0] = np.var(temp[0])/0.25                      #Variance of the relative team score
        X[i*features+4,0] = np.mean(temp[1])                          #Averaged relative total score
        X[i*features+5,0] = np.var(temp[1])/0.25                      #Variance of the relative total score
        X[i*features+6,0] = sum(temp[2])/nmatches                     #Number of recently played games on the champion
        X[i*features+7,0] = np.mean(temp[3])                          #Recent winrate
        X[i*features+8,0] = sum(temp[4])/nmatches                     #Number of recently played games on the lane (undefined assumed to be the proper lane too)
        X[i*features+9,0] = sum(k < 8 for k in temp[5])/nmatches      #Number of games played less than 8 hours ago
        X[i*features+10,0] = sum(k < 72 for k in temp[5])/nmatches    #Number of games played less than 3 days ago
        X[i*features+11,0] = sum(k < 168 for k in temp[5])/nmatches   #Number of games played less than a week ago
        for j in range(nmatches):
            if temp[3][j] == 1: X[i*features+12,0] += 1                  #Winning streak
            else: break
        for j in range(nmatches):
            if temp[3][j] == 0: X[i*features+12,0] += -1                 #Losing streak
            else: break
        X[i*features+12,0] = X[i*features+12,0]/nmatches
    return X

############################################################# 
def champfinder(champ):
    global api_key, my_region
    watcher = LolWatcher(api_key)
    for k in range(10):
        try:
            latest = watcher.data_dragon.versions_for_region(my_region)['n']['champion']
            break
        except:
            print("Request rate too high")
    for k in range(10):
        try:
            static_champ_list = watcher.data_dragon.champions(latest, False, 'en_US')
            break
        except:
            print("Request rate too high")

    if champ == 'Wukong': return True
    for key in static_champ_list['data']:
        if static_champ_list['data'][key]['id'] == champ: return True

    temp = []
    for key in static_champ_list['data']:
        if static_champ_list['data'][key]['id'][0] == champ[0] or static_champ_list['data'][key]['id'][0] == champ[0].upper(): temp.append(static_champ_list['data'][key]['id'])

    print(temp)
    return False

############################################################# Gather raw data from N games that happened before the targetstamp from a number of players
def leaguecollect(players, ngames, targetstamp):
    model = leaguedata()

    i=0
    playerspecific = [[None]*2 for k in range(len(players))]                        #Summoner level, season winrate
    matchhistory = [[[None]*6 for k in range(ngames)] for p in range(len(players))] #Relative_team_score, relative_total_score, champion, winlose, lanerol, timestamp
    for player in players:
        playerspecific[i][0], playerspecific[i][1], temp = model.defineplayer(player)

        k = 0
        while targetstamp!=0: #We use only training data from games before the champ select takes place. Set it to 0 for non training
            if temp['matches'][k]['timestamp'] > targetstamp:
                k+=1
            else:
                break

        for j in range(ngames):
            matchhistory[i][j][:] = model.execute_match(j+k)
        i+=1
        print('Completed extraction for player '+str(i))

    return playerspecific, matchhistory

############################################################# Generate game score, roles, summoner level and all kind of information to be collected from 1 game 
class leaguedata:
    def __init__(self):
        global date, api_key, my_region
        self.watcher = LolWatcher(api_key)

        # League's dragon database latest version
        latest = self.watcher.data_dragon.versions_for_region(my_region)['n']['champion']

        #Champions static information
        self.static_champ_list = self.watcher.data_dragon.champions(latest, False, 'en_US')
    
    def defineplayer(self, playerid):
        for k in range(10):
            try:
                self.me = self.watcher.summoner.by_name(my_region, playerid)
                break
            except:
                print("Request rate too high")
        for k in range(10):
            try:
                self.my_matches = self.watcher.match.matchlist_by_account(my_region,self.me['accountId'])
                break
            except:
                print("Request rate too high")
        summonerlevel = self.me['summonerLevel']

        for k in range(10):
            try:
                temp = self.watcher.league.by_summoner(my_region, self.me['id'])
                break
            except:
                print("Request rate too high")
        if len(temp) == 2: temp = temp[1]
        elif len(temp) != 1: return summonerlevel, 0.5, self.my_matches
        else: temp = temp[0]
        winrate = round(temp['wins']/(temp['losses']+temp['wins']),3)

        return summonerlevel, winrate, self.my_matches

    def execute_match(self, gamenumber):
        matchid = self.my_matches['matches'][gamenumber]['gameId']
        timestamp = round((date - self.my_matches['matches'][gamenumber]['timestamp']/1000)/3600) # In hours. Original timestamp is in ms
        for k in range(10):
            try:
                match_detail = self.watcher.match.by_id(my_region, matchid)
                break
            except:
                print("Request rate too high")

        #Vars
        names = [None]*10
        ids = [[None]*10 for i in range (2)]
        winlose = [None]*10
        goldearned = [None]*10
        dmgturrets = [None]*10
        visionscore = [None]*10
        kda = [[None]*10 for i in range (3)]
        kdascore = [None]*10
        cctime = [None]*10
        dmgdealt = [None]*10
        hpheal = [None]*10
        cspermin = [None]*10
        role = [None]*10
        lane = [None]*10
        kp = [None]*10
        puntuacion = [None]*10

        gametime = match_detail['gameDuration']/60

        #Names and champions
        for i in range (10):
            names[i] = match_detail['participantIdentities'][i]['player']['summonerName']
            ids[0][i] = match_detail['participants'][i]['championId']
            for key in self.static_champ_list['data']:
                if int(self.static_champ_list['data'][key]['key']) == ids[0][i]:
                    ids[1][i] = self.static_champ_list['data'][key]['id']
                    if ids[1][i]=='MonkeyKing':
                        ids[1][i] = 'Wukong'

        ### Stats relativizadas (usando el tiempo de juego en minutos o el valor máximo en la partida)
        for i in range (10):
            winlose[i] = match_detail['participants'][i]['stats']['win']
            goldearned[i] = round(match_detail['participants'][i]['stats']['goldEarned']/gametime,1)
            visionscore[i] = round(match_detail['participants'][i]['stats']['visionScore']/gametime,1)
            dmgturrets[i] = match_detail['participants'][i]['stats']['damageDealtToTurrets']

            kda[0][i] = match_detail['participants'][i]['stats']['kills']
            kda[1][i] = match_detail['participants'][i]['stats']['deaths']
            kda[2][i] = match_detail['participants'][i]['stats']['assists']

            cctime[i] = match_detail['participants'][i]['stats']['timeCCingOthers']
            dmgdealt[i] = match_detail['participants'][i]['stats']['totalDamageDealtToChampions']
            
            temp = match_detail['participants'][i]['stats']['totalUnitsHealed']
            if temp > 5:
                temp = 5
            hpheal[i] = int(match_detail['participants'][i]['stats']['totalHeal']/(6-temp))

            cspermin[i] = round((match_detail['participants'][i]['stats']['totalMinionsKilled']+match_detail['participants'][i]['stats']['neutralMinionsKilled'])/gametime,1)
            role[i] = match_detail['participants'][i]['timeline']['role']
            lane[i] = match_detail['participants'][i]['timeline']['lane']

        temp = [max(dmgturrets),max(cctime),max(dmgdealt),max(hpheal)]
        for i in range (10):
            dmgturrets[i] = round(dmgturrets[i]/max(temp[0],1),4)
            cctime[i] = round(cctime[i]/max(temp[1],1),4)
            dmgdealt[i] = round(dmgdealt[i]/max(temp[2],1),4)
            hpheal[i] = round(hpheal[i]/max(temp[3],1),4)

        #Kill participation
        totalkillsF = 0
        totalkillsW = 0
        for i in range(10):
            if winlose[i] == False:
                winlose[i] = 1.02
                totalkillsF = kda[0][i] + totalkillsF
            else:
                winlose[i] = 0.98
                totalkillsW = kda[0][i] + totalkillsW
        for i in range(10):
            if winlose[i] == 1.02:
                kp[i] = (kda[0][i]+kda[2][i])/max(totalkillsF,1)
            else:
                kp[i] = (kda[0][i]+kda[2][i])/max(totalkillsW,1)

        for i in range (10):
            if kda[1][i] == 0:
                kdascore[i] = round(kda[0][i]+kda[2][i],2)
            else:
                kdascore[i] = round((kda[0][i]+kda[2][i])/kda[1][i],2)

        ### Cálculo de la score de cada jugador
        maxval1 = min(gametime,25)
        player = 0
        for i in range (10):
            puntuacion[i] = 5*maxval1*np.exp(0.6*np.log(1+visionscore[i])+kp[i])*(3*dmgdealt[i]+hpheal[i]+cctime[i])
            puntuacion[i] = puntuacion[i]+12*maxval1*dmgturrets[i]+0.9*goldearned[i]*np.log(kdascore[i]+1)/(kda[1][i]+1)
            puntuacion[i] = int(puntuacion[i]*winlose[i])

            if self.me['name']==names[i]:
                player = i

        total = sum(puntuacion)
        if player <= 4:
            team = sum(puntuacion[0:5])
        else:
            team = sum(puntuacion[5:10])

        #Player's role
        if lane[player] == 'TOP': lanerol = 'top'
        elif lane[player] == 'JUNGLE': lanerol = 'jung'
        elif lane[player] == 'MID': lanerol = 'mid'
        elif role[player] == 'DUO_CARRY': lanerol = 'adc'
        elif role[player] == 'DUO_SUPPORT': lanerol = 'supp'
        else: lanerol = 'undefined'

        ## OUT DATA
        relative_team_score = puntuacion[player]/team
        relative_total_score = puntuacion[player]/total
        champion = ids[1][player]

        winlose = 0
        if match_detail['participants'][player]['stats']['win'] == True:
            winlose = 1
        ## OUT DATA

        return relative_team_score, relative_total_score, champion, winlose, lanerol, timestamp
