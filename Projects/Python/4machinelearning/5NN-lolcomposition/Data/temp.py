writet=[]
f = open("Data/Inputs.txt","r")
contents = f.read().splitlines()
f.close()

for entry in contents:
    temp = [int(x) for x in entry.split(' ')]
    temp2 = [0]*len(temp)
    for i in range(len(temp)):
        if temp[i]==0:
            temp2[i] = 0
        else:
            temp2[i] = temp[i]*-1
    writet.append(temp)
    writet.append(temp2)

f= open("Data/TEMP1.txt","w")
for i in range(len(writet)):
    for j in range(len(temp)):
        if j==0:
            f.write(str(writet[i][j]))
            continue
        f.write(' '+str(writet[i][j]))
    f.write('\n')
f.close()

temp=[]
f = open("Data/Outputs.txt","r")
contents = f.read().splitlines()
f.close()

f = open("Data/TEMP2.txt","w")
temp.append([int(x) for x in contents[0].split(' ')])

for i in range(len(temp[0])):
    kk = temp[0][i]
    if i!=0:
        f.write(' ')
    f.write(str(kk))

    if kk == 0:
        f.write(' 1')
    else:
        f.write(' 0')

f.close()