import json
import time
from riotwatcher import LolWatcher, ApiError

#Initialization
champids = []
ids = [None]*10
outputvector = []

api_key = 'RGAPI-4b8c11c1-47a0-4097-b59c-9f10dcf69bc1'
watcher = LolWatcher(api_key)
region = 'euw1'

# League's dragon database latest version
latest = watcher.data_dragon.versions_for_region(region)['n']['champion']
# Lets get some champions static information
static_champ_list = watcher.data_dragon.champions(latest, False, 'es_ES')

## Championdata
f= open("Data/ChampionIDs.txt","w")
k = 0
for key in static_champ_list['data']:
    k += 1
    temp = [int(static_champ_list['data'][key]['key']),static_champ_list['data'][key]['id'],k]
    champids.append(temp)

    f.write(str(temp[2])+' '+str(temp[1])+'\n')

f.close()

## MatchIDs
with open('Data/matchlist_euw1.json') as json_file:
    data = json.load(json_file)
datasize = len(data)
print(datasize)

## Iterate through the matches, building both the inputs and outputs
f= open("Data/Inputs.txt","w")

for i in range(datasize):
    matchid = data[i]
    
    for j in range(10):
        try:
            print(i)
            match_detail = watcher.match.by_id(region,matchid)
            break
        except:
            print("Ooops! I did it again")
            print()


    outputwin = 0
    if match_detail['participants'][0]['stats']['win']:
        outputwin = 1
    outputvector.append(outputwin)

    entry = [0]*len(champids)
    for j in range (0,5):
        ids[j] = match_detail['participants'][j]['championId']
        for champ in champids:
            if champ[0]==ids[j]:
                entry[champ[2]-1] = 1
                break
    for j in range (5,10):
        ids[j] = match_detail['participants'][j]['championId']
        for champ in champids:
            if champ[0]==ids[j]:
                entry[champ[2]-1] = -1
                break

    for j in range(len(entry)):
        if j==0:
            f.write(str(entry[j]))
            continue
        f.write(' '+str(entry[j]))
    f.write('\n')

    if i>9995:
        break
    time.sleep(1)



f.close()
f= open("Data/Outputs.txt","w")
for j in range(len(outputvector)):
    if j==0:
        f.write(str(outputvector[j]))
        continue
    f.write(' '+str(outputvector[j]))
f.close()