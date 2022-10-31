import mymachinelearning as ml
import numpy as np
import matplotlib.pyplot as plt

## NN Training
temp=[]
f = open("Data/Inputs.txt","r")
contents = f.read().splitlines()
f.close()

m = len(contents)                                       #Number of samples

for entry in contents:
    temp.append([int(x) for x in entry.split(' ')])

n_inputs = len(temp[0])                                 #Number of inputs
x_train = np.array(temp).T                              #ROWS = 2 features o inputs, COLUMNS = dataset of size m

temp=[]
f = open("Data/Outputs.txt","r")
contents = f.read().splitlines()
f.close()

n_outputs = len(contents)                                #Number of outputs

for entry in contents:
    temp.append([int(x) for x in entry.split(' ')])

y_train = np.array(temp)                                #ROWS = 1 outputs, COLUMNS = dataset of size m
        
epochs = 600    #Times that the entire dataset is used to train the NN
lr = 1           #Initial learning rate

#This will be a NN with 1 hidden layer and a number of hidden nodes, determined by the next expression.
alfa = 2 #Value between 1 and 10
#n_hiddennodes = int(round(m/(alfa*(n_inputs+n_outputs))))
n_hiddennodes = 20
actF = ['leakyReLU','sigmoid']

layers = [ml.Layer(n_inputs,n_hiddennodes,actF[0]), ml.Layer(n_hiddennodes,n_outputs,actF[1])]
costs = []
lrvisual = []
costbefore = 10

for epoch in range(epochs):
    if epoch % 10 ==0: print(epoch)
    for i in range(int(m/2)):
        x_batch = x_train[:,2*i:2*i+2]
        y_batch = y_train[:,2*i:2*i+2]

        x = x_batch
        for layer in layers:
            y = layer.feedforward(x)
            x = y #The inputs of the current layer will be the output of the previous layer

        cdy = ml.d_logloss(y_batch, y)
        for layer in reversed(layers):
            cdx = layer.backprop(cdy,lr)
            cdy = cdx #The dC/dx of the last layer becomes the dC/dy of the first layer

    x = x_train
    for layer in layers:
        y = layer.feedforward(x)
        x = y
    cost = 1/m *np.sum(ml.logloss(y_train,y))
    costs.append(cost)

    if costbefore<cost:
        lr = lr*14/15

    costbefore = cost
    lrvisual.append(lr)



## Plots
plt.plot(range(epochs),costs)
plt.xlabel('Epoch')
plt.ylabel('Cost function error')
plt.show()

plt.plot(range(epochs),lrvisual)
plt.xlabel('Epoch')
plt.ylabel('Learning rate')
plt.show()

f= open("NNparameters.txt","w")
i=0
for layer in layers:
    i+=1
    name='NNlayer'+str(i)
    layer.saveNN(name)
    f.write(name+' arguments:'+'\n')
    f.write(str(layer.w.T.shape)+'\n')
    f.write(actF[i-1]+'\n')
f.close()