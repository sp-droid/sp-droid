import json

sorted_games = []
id = 0
nombreworlds = "World Championship 2019"

with open('parsing/S9.json') as json_file:
    data = json.load(json_file)['tournaments']

for tournament in data:
    name = tournament['name']
    region = tournament['region']

    for game in tournament['game']:
        if game['result'] == '-':
            continue

        id = id+1
        blueside = game['blueside']
        redside = game['redside']

        YYYYMMDD = game['date']

        temp = game['result']
        temp = [int(s) for s in temp.split() if s.isdigit()]

        if temp[0]>temp[1]:
            result = 1
        elif temp[1]<temp[0]:
            result = 0
        else:
            continue
        #New structure of each game: DATE-REGION-TOURNAMENT-BLUESIDE-REDSIDE-RESULT
        vector = [YYYYMMDD, region, name, blueside, redside, result]
        sorted_games.append(vector)
        if region=="WR":
            vector = [YYYYMMDD, region, name, blueside, redside, 1]
            for i in range(temp[0]-1):
                sorted_games.append(vector)
            vector = [YYYYMMDD, region, name, blueside, redside, 0]
            for i in range(temp[1]-1):
                sorted_games.append(vector)
        

sorted_games.sort(key=lambda r: r[0])

#ELO# Definición de la constante k y del elo básico
k = 44
base_options = [700,800] #Otras, Main regions

elo = {}
matrix = []
teamregion = []

npartidos = int(len(sorted_games))
ready = 0

for i in range (npartidos):
    equipoA = sorted_games[i][3]
    equipoB = sorted_games[i][4]
    #G vale 1 si gana el equipo A, 0 si gana el B
    G = sorted_games[i][5]

    fecha = sorted_games[i][0]
    torneo = sorted_games[i][2]
    region = sorted_games[i][1]

    #Eliminar partidos de diciembre
    if ready==0:
        if region=='WR':
            continue
        else:
            ready=1

    #K y base
    if torneo==nombreworlds:
        base = base_options[1]
    elif region=='NA' or region=='EUW' or region=='CN' or region=='KR':
        base = base_options[1]
    else:
        base = base_options[0]

    if equipoA in elo:
        eloAold = elo[equipoA]
    else:
        eloAold = base
        if region == "WR":
            region = "Others"
        teamregion.append(region)
    if equipoB in elo:
        eloBold = elo[equipoB]
    else:
        eloBold = base
        if region == "WR":
            region = "Others"
        teamregion.append(region)
    
    #Cada partida de worlds
    if torneo==nombreworlds:
        eloAold = eloAold+10
        eloBold = eloBold+10

    pA = 1/(1+10**((eloBold-eloAold)/400))
    elo[equipoA] = eloAold + k*(G-pA)
    elo[equipoB] = eloBold + k*(pA-G)

    dia = []
    for key in elo:
        dia.append(elo[key])
    vector = [fecha, torneo, dia]
    if region == "WR":
        matrix.append(vector)
    elif i % 20 == 0:
        matrix.append(vector)

#Volcado al archivo para bar races
f = open("export.txt","w")
f.write('Team, Region')
for i in range(len(matrix)):
    f.write(','+str(matrix[i][1]))
j = 0
for key in elo:
    reg = str(teamregion[j])
    if reg=="CN":
        reg="China"
    elif reg=="KR":
        reg="South Korea"
    elif reg=="NA":
        reg="North America"
    elif reg=="EUW":
        reg="Europe West"
    elif reg=="BR":
        reg="Brazil"
    elif reg=="JP":
        reg="Japan"
    elif reg=="TR":
        reg="Turkey"
    else:
        reg="Others"
    f.write('\n'+key+','+reg)
    for i in range(len(matrix)):
        if len(matrix[i][2])<(j+1):
            f.write(',0')
            continue
        f.write(','+str(matrix[i][2][j]))
    j = j+1
f.close()