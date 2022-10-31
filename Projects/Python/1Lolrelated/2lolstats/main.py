from riotwatcher import LolWatcher, ApiError
import matplotlib.pyplot as plt
import numpy as np


api_key = 'RGAPI-4b8c11c1-47a0-4097-b59c-9f10dcf69bc1'
watcher = LolWatcher(api_key)
my_region = 'euw1'

# League's dragon database latest version
latest = watcher.data_dragon.versions_for_region(my_region)['n']['champion']
# Lets get some champions static information
static_champ_list = watcher.data_dragon.champions(latest, False, 'es_ES')

gamenumber = int(input ('Introducir hace cuantas partidas se jugó: '))
playerid = input ('Introducir nombre de invocador: ')
me = watcher.summoner.by_name(my_region,playerid)
my_matches = watcher.match.matchlist_by_account(my_region,me['accountId'])
matchid = my_matches['matches'][gamenumber]['gameId']
match_detail = watcher.match.by_id(my_region,matchid)

#Número de jugadores
njug = 10

#Declaración de variables
names = [None]*njug
ids = [[None]*njug for i in range (2)]
winlose = [None]*njug
goldearned = [None]*njug
dmgturrets = [None]*njug
visionscore = [None]*njug
kda = [[None]*njug for i in range (3)]
kdascore = [None]*njug
multiplekills = [[None]*4 for i in range (njug)]
cctime = [None]*njug
dmgdealt = [None]*njug
hpheal = [None]*njug
cspermin = [None]*njug
goldmin10 = [None]*njug
role = [None]*njug
lane = [None]*njug
kp = [None]*njug
puntuacion = [None]*njug
posgold10 = [[None]*njug for i in range (5)]

#Duración de la partida
gametime = match_detail['gameDuration']/60

#Nombres y champs
equipoazul = 'Equipo azul'
equiporojo = 'Equipo rojo'
for i in range (njug):
    names[i] = match_detail['participantIdentities'][i]['player']['summonerName']
    ids[0][i] = match_detail['participants'][i]['championId']
    for key in static_champ_list['data']:
        if int(static_champ_list['data'][key]['key']) == ids[0][i]:
            ids[1][i] = static_champ_list['data'][key]['id']
            if ids[1][i]=='MonkeyKing':
                ids[1][i] = 'Wukong'

### Stats relativizadas (usando el tiempo de juego en minutos o el valor máximo en la partida)
for i in range (njug):
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
    goldmin10[i] = round(match_detail['participants'][i]['timeline']['goldPerMinDeltas']['0-10']/100,1)
    role[i] = match_detail['participants'][i]['timeline']['role']
    lane[i] = match_detail['participants'][i]['timeline']['lane']

temp = [max(dmgturrets),max(cctime),max(dmgdealt),max(hpheal)]
for i in range (njug):
    dmgturrets[i] = round(dmgturrets[i]/temp[0],4)
    cctime[i] = round(cctime[i]/temp[1],4)
    dmgdealt[i] = round(dmgdealt[i]/temp[2],4)
    hpheal[i] = round(hpheal[i]/temp[3],4)

#Kill participation
totalkillsF = 0
totalkillsW = 0
for i in range(njug):
    if winlose[i] == False:
        winlose[i] = 1.02
        totalkillsF = kda[0][i] + totalkillsF
    else:
        winlose[i] = 0.98
        totalkillsW = kda[0][i] + totalkillsW
for i in range(njug):
    if winlose[i] == 1.02:
        kp[i] = (kda[0][i]+kda[2][i])/totalkillsF
    else:
        kp[i] = (kda[0][i]+kda[2][i])/totalkillsW

for i in range (njug):
    if kda[1][i] == 0:
        kdascore[i] = round(kda[0][i]+kda[2][i],2)
    else:
        kdascore[i] = round((kda[0][i]+kda[2][i])/kda[1][i],2)

### Cálculo de la score de cada jugador
maxval1 = min(gametime,25)
for i in range (njug):
    puntuacion[i] = 5*maxval1*np.exp(0.6*np.log(1+visionscore[i])+kp[i])*(3*dmgdealt[i]+hpheal[i]+cctime[i])
    puntuacion[i] = puntuacion[i]+12*maxval1*dmgturrets[i]+0.9*goldearned[i]*np.log(kdascore[i]+1)/(kda[1][i]+1)
    puntuacion[i] = int(puntuacion[i]*winlose[i])

ylin = np.arange(len(names))
plt.bar(ylin[0:5],puntuacion[0:5],label=equipoazul)
plt.bar(ylin[5:10],puntuacion[5:10],label=equiporojo)
plt.xticks(ylin,names,fontsize=9)
for i, v in enumerate(puntuacion):
    plt.text(i, v+10, str(v), ha="center", va = 'center', fontweight='bold',fontsize=10)
for i, v in enumerate(ids[1]):
    plt.text(i, 16, str(v), ha="center", va = 'center', fontweight='bold',fontsize=10)
plt.legend()
plt.show()

### Determinación de la posición de cada uno y oro a min10
maxg = max(goldmin10)
oro1 = 0
oro2 = 0
for i in range (int(njug/2)):
    if lane[i]=='TOP':
        posgold10[0][0] = 'Top'
        posgold10[1][0] = str(goldmin10[i])+'k'
        posgold10[2][0] = goldmin10[i]/maxg
        posgold10[3][0] = names[i]
        posgold10[4][0] = ids[1][i]
    elif role[i]=='NONE' or lane[i]=='JUNGLE':
        posgold10[0][1] = 'Jungler'
        posgold10[1][1] = str(goldmin10[i])+'k'
        posgold10[2][1] = goldmin10[i]/maxg
        posgold10[3][1] = names[i]
        posgold10[4][1] = ids[1][i]
    elif lane[i]=='MIDDLE':
        posgold10[0][2] = 'Mid'
        posgold10[1][2] = str(goldmin10[i])+'k'
        posgold10[2][2] = goldmin10[i]/maxg
        posgold10[3][2] = names[i]
        posgold10[4][2] = ids[1][i]
    elif role[i]=='DUO_CARRY':
        posgold10[0][3] = 'ADC'
        posgold10[1][3] = str(goldmin10[i])+'k'
        posgold10[2][3] = goldmin10[i]/maxg
        posgold10[3][3] = names[i]
        posgold10[4][3] = ids[1][i]
    elif role[i]=='DUO_SUPPORT':
        posgold10[0][4] = 'Supp'
        posgold10[1][4] = str(goldmin10[i])+'k'
        posgold10[2][4] = goldmin10[i]/maxg
        posgold10[3][4] = names[i]
        posgold10[4][4] = ids[1][i]
    oro1 = oro1 + goldmin10[i]
for i in range (5,njug):
    if lane[i]=='TOP':
        posgold10[0][5] = 'Top'
        posgold10[1][5] = str(goldmin10[i])+'k'
        posgold10[2][5] = goldmin10[i]/maxg
        posgold10[3][5] = names[i]
        posgold10[4][5] = ids[1][i]
    elif role[i]=='NONE' or lane[i]=='JUNGLE':
        posgold10[0][6] = 'Jungler'
        posgold10[1][6] = str(goldmin10[i])+'k'
        posgold10[2][6] = goldmin10[i]/maxg
        posgold10[3][6] = names[i]
        posgold10[4][6] = ids[1][i]
    elif lane[i]=='MIDDLE':
        posgold10[0][7] = 'Mid'
        posgold10[1][7] = str(goldmin10[i])+'k'
        posgold10[2][7] = goldmin10[i]/maxg
        posgold10[3][7] = names[i]
        posgold10[4][7] = ids[1][i]
    elif role[i]=='DUO_CARRY':
        posgold10[0][8] = 'ADC'
        posgold10[1][8] = str(goldmin10[i])+'k'
        posgold10[2][8] = goldmin10[i]/maxg
        posgold10[3][8] = names[i]
        posgold10[4][8] = ids[1][i]
    elif role[i]=='DUO_SUPPORT':
        posgold10[0][9] = 'Supp'
        posgold10[1][9] = str(goldmin10[i])+'k'
        posgold10[2][9] = goldmin10[i]/maxg
        posgold10[3][9] = names[i]
        posgold10[4][9] = ids[1][i]
    oro2 = oro2 + goldmin10[i]

temp = [None]*5
temp2 = [None]*5
for i in range(5):
    temp[i] = 2-posgold10[2][i]-posgold10[2][i+5]
    temp2[i] = temp[i]+posgold10[2][i]

ylin = [0,1,2,3,4]
plt.barh(ylin,posgold10[2][0:5],label=equipoazul)
plt.barh(ylin,temp,left=posgold10[2][0:5],color=[0.9,0.9,0.9])
plt.barh(ylin,posgold10[2][5:njug],left=temp2,label=equiporojo)

for i in range(5):
    plt.text(0.05, i, posgold10[1][i], fontweight='bold',fontsize=14,ha='center', va='center')
    plt.text(0.2, i+0.15, posgold10[3][i],fontsize=14,ha='center', va='center')
    plt.text(0.2, i-0.15, posgold10[4][i],fontsize=14,ha='center', va='center')
    plt.text(1.95, i, posgold10[1][i+5], fontweight='bold',fontsize=14,ha='center', va='center')
    plt.text(1.8, i+0.15, posgold10[3][i+5],fontsize=14,ha='center', va='center')
    plt.text(1.8, i-0.15, posgold10[4][i+5],fontsize=14,ha='center', va='center')
plt.text(0.8, 4.4, str(round(oro1,2))+'k', fontweight='bold',fontsize=16,ha='center', va='center')
plt.text(1.2, 4.4, str(round(oro2,2))+'k', fontweight='bold',fontsize=16,ha='center', va='center')

plt.xlim(0, 2)
plt.xticks([], [])
plt.yticks(ylin, posgold10[0][0:5],fontsize=16)
plt.title('Gold diff @10min',fontweight='bold',fontsize=16)
plt.legend(loc='upper center')
plt.show()