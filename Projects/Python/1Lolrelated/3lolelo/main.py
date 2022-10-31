import numpy as np
import matplotlib.pyplot as plt

#Definición de la constante k y del elo básico
k = 80
base = 400
factorR = 0.05 #Decay
#Número de equipos inactivos
nic = 1

elo = {}
matrix = []

#Extracción de los nombres y resultados
f = open("partidos.txt","r")
contents = f.read().splitlines()
f.close()

npartidos = int(len(contents)/3)

for i in range (npartidos):
    equipoA = contents[3*i]
    equipoB = contents[3*i+1]
    #G vale 1 si gana el equipo A, 0 si gana el B
    G = int(contents[3*i+2])

    if equipoA in elo:
        eloAold = elo[equipoA]
    else:
        eloAold = base
    if equipoB in elo:
        eloBold = elo[equipoB]
    else:
        eloBold = base

    pA = 1/(1+10**((eloBold-eloAold)/400))
    elo[equipoA] = eloAold + k*(G-pA)
    elo[equipoB] = eloBold + k*(pA-G)

    dia = []
    for key in elo:
        if elo[key]>1500:
            elo[key] = round(elo[key]-(elo[key]-base)*factorR)
        else:
            elo[key] = round(elo[key]+(base-elo[key])*factorR)
        dia.append(elo[key])
    matrix.append(dia)

#Volcado al archivo para bar races
f = open("data.txt","w")
f.write('Equipo')
for i in range(len(matrix)):
    f.write(',Partido '+str(i))
j = 0
for key in elo:
    f.write('\n'+key)
    for i in range(len(matrix)):
        if len(matrix[i])<(j+1):
            f.write(',0')
            continue
        f.write(','+str(matrix[i][j]))
    j = j+1
f.close()

elo = sorted(elo.items(), key = lambda x : x[1],reverse=False)

ylin = np.arange(len(elo)-nic)
temp = [0]*(len(elo)-nic)
i = 0
for key in elo:
    if key[0] == 'QPC Originals':
        plt.barh(key[0],key[1],color=[0,0.5,0.9])
    elif key[0] == 'G2 Fecas':
        plt.barh(key[0],key[1],color=[0.5,0.1,0.5])
    elif key[0] == 'QLASH Crew':
        plt.barh(key[0],key[1],color=[0.6,0.5,0.1])
    elif key[0] == 'MataespartacosXAida':
        plt.barh(key[0],key[1],color=[0,0.5,0.2])
    elif key[0] == 'Mataespartacos':
        plt.barh(key[0],key[1],color=[0.6,0.3,0.1])
    elif key[0] == 'Viva el Staff':
        plt.barh(key[0],key[1],color=[0.9,0.6,0.1])
    elif key[0] == 'F/A Sociedad Loliense':
        plt.barh(key[0],key[1],color=[0.3,0.3,0.6])
    elif key[0] == 'Esparta':
        plt.barh(key[0],key[1],color=[0.6,0,0.1])
    elif key[0] == 'Iron Qlash':
        plt.barh(key[0],key[1],color=[0,0.6,0.5])
    else:
        continue
    temp[i] = key[1]
    i = i+1

for i, v in enumerate(sorted(temp)):
    plt.text(v+0.2, i, str(round(v, 2)), fontweight='bold',fontsize=10)

plt.title('QPC LoL ELO',fontweight='bold',fontsize=18)
plt.show()