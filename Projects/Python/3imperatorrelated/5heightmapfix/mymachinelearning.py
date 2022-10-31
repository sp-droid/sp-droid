import numpy as np
import matplotlib.pyplot as plt
import networkx as nx

### VISUALIZATION
def neuralplot(nodes, condition, W, A):                      #nodes = list with the number of neurons in each layer, condition=True when giving a valid dict of weights W
    g = nx.Graph()

    L = len(nodes)
    nmax = max(nodes)
    nodesize = 20000/nmax
    dic = {
        'pos' : {},
        'labels' : {},
        'weights' : [],
        'colors' : []
    }
    for l in range(L):
        for j in range(nodes[l]):
            nindex = (l+1)*100+j+1                                  #Neuron index
            g.add_node(nindex)

            y = ((nodes[l]-1)/2-j)                                  #Fixed position
            if nmax>11: y =  y*nmax/nodes[l]
            x = (l+1)
            dic['pos'][nindex] = (x,y)
            dic['labels'][nindex] = nindex-100

            if l>0:                                                 #Synapses
                for k in range(nodes[l-1]):
                    nindexprev = l*100+k+1
                    g.add_edge(nindexprev, nindex)

    for j in range(nodes[0]):                                       #Input labels
        nindex = 100+j+1
        dic['labels'][nindex] = 'Input '+str(j+1)
    for j in range(nodes[L-1]):                                     #Output labels
        nindex = 100*L+j+1
        dic['labels'][nindex] = 'Output '+str(j+1)

    if condition==True:
        param = nodesize
        nodesize = []
        for l in range(1,L):
            maxm = np.max(np.absolute(W[l]))
            minm = np.min(np.absolute(W[l]))
            for j in range(nodes[l]):
                nindex = (l+1)*100+j+1
                for k in range(nodes[l-1]):
                    value = (abs(W[l][j,k])-minm)/(maxm-minm)
                    dic['weights'].append(value*4+1)
                    if W[l][j,k]>0:
                        dic['colors'].append((0, value*0.8+0.2, 0.2))
                    if W[l][j,k]<0:
                        dic['colors'].append((value*0.8+0.2, 0, 0.2))
        for l in range(L):
            maxm = np.max(np.absolute(A[l]))
            minm = np.min(np.absolute(A[l]))
            for j in range(nodes[l]):
                nindex = (l+1)*100+j+1
                dic['labels'][nindex] = "{:.3f}".format(A[l][j][0])
                value = (abs(A[l][j][0])-minm)/(maxm-minm)
                if 1==nodes[l]: value=1
                nodesize.append(value*0.7*param+0.5*param)
                    

    nx.draw(g, dic['pos'], labels=dic['labels'], with_labels=True, node_size=nodesize, font_size=8)
    
    if condition==True: nx.draw_networkx_edges(g,dic['pos'], width=dic['weights'], edge_color=dic['colors'])

    aux = nx.Graph()                                                #Figure vertical size fix
    aux.add_node('Aux1')
    aux.add_node('Aux2')
    posaux = {}
    posaux['Aux1'], posaux['Aux2'] = [(0.9,nmax/2), (L+0.1,-nmax/2)]
    nx.draw(aux, posaux, node_size=0)

    plt.show()
    return

### MACHINE LEARNING
#Activation functions
def tanh(x):
    return np.tanh(x)
def d_tanh(x):
    return 1-np.square(np.tanh(x))

def sigmoid(x):
    return 1/(1 + np.exp(-x))
def d_sigmoid(x):
    return (1 - sigmoid(x)) * sigmoid(x)

def leakyReLU(x, alpha=0.01):
    return np.maximum(alpha*x,x)
def d_leakyReLU(x, alpha=0.01):
    temp = np.sign(x)
    return np.maximum(alpha*temp,temp)

def ReLU(x):
    return np.maximum(0,x)
def d_ReLU(x):
    temp = np.sign(x)
    return np.maximum(0,temp)

def linear(x):                                          #Only used usually as output layer activation function
    return x
def d_linear(x):
    return np.sign(x)

def softmax(x):                                         #DONT USE, WORK IN PROGRESS
    return np.exp(x)/np.sum(np.exp(x))

#Loss functions
def logloss(y,a):                                       #Cross-entropy, log loss --> Classification problems
    m = y.shape[1]
    return -1/m*(y*np.log(a) + (1-y)*np.log(1-a))
def d_logloss(y,a):
    m = y.shape[1]
    return 1/m*(a - y)/(a*(1 - a))

def MSE(y,a):                                           #Mean squared error  --> Regression without outliers
    m = y.shape[1]
    return 2/m*(y-a)**2
def d_MSE(y,a):
    m = y.shape[1]
    return -1/m*(y-a)

class Layer:

    activationFunctions = {
        'tanh': (tanh, d_tanh),
        'sigmoid': (sigmoid, d_sigmoid),
        'ReLU': (ReLU, d_ReLU),
        'leakyReLU': (leakyReLU, d_leakyReLU)
    }

    def __init__(self, inputs, neurons, activationF):                   #Number of inputs, neurons & type of activation function
        self.w = np.random.randn(neurons, inputs) * np.sqrt(2./inputs)  #Initial weights
        self.b = np.zeros((neurons, 1))                                 #Initial biases
        self.act, self.d_act = self.activationFunctions.get(activationF)

        self.m_dw = np.zeros((neurons, inputs))                         #First and second moments, mean and uncentered variance, weights
        self.v_dw = self.m_dw
        self.m_db = np.zeros((neurons, 1))                              #First and second moments, mean and uncentered variance, biases
        self.v_db = self.m_db

    def feedforward(self, x):
        self.x = x                                                      #Input from the previous layer
        self.m = x.shape[1]                                             #Size of the batch
        self.z = self.w @ self.x + self.b @ np.ones((1, self.m))        #Inputs times weights plus biases
        self.y = self.act(self.z)                                       #Output from the current layer
        return self.y
    
    def backprop(self, dJdy, learning_rate, lambd):
        dJdz = np.multiply(dJdy, self.d_act(self.z))                    #dJdyl+1 * g'l = dJdz
        dJdw = dJdz @ self.x.T                                          #dJdz * al-1 = dJdw
        dJdb = dJdz @ np.ones((1, self.m)).T                            #dJdz * 1 = dJdb
        dJdx = self.w.T @ dJdz                                          #Information for the next layer

        reg = lambd/self.m*self.w                                       #Regularization term, only applied to the weights
        dJdw += reg

        self.w -= learning_rate * dJdw
        self.b -= learning_rate * dJdb
        return dJdx
    
    def Adam_backprop(self, dJdy, learning_rate, lambd, beta1, beta2, epsilon, epoch):
        dJdz = np.multiply(dJdy, self.d_act(self.z))                    #dJdyl+1 * g'l = dJdz
        dJdw = dJdz @ self.x.T                                          #dJdz * al-1 = dJdw
        dJdb = dJdz @ np.ones((1, self.m)).T                            #dJdz * 1 = dJdb
        dJdx = self.w.T @ dJdz                                          #Information for the next layer

        reg = lambd/self.m*self.w                                       #Regularization term, only applied to the weights
        dJdw += reg

        self.m_dw = beta1*self.m_dw + (1-beta1)*dJdw                    #Mean
        self.m_db = beta1*self.m_db + (1-beta1)*dJdb

        self.v_dw = beta2*self.v_dw + (1-beta2)*np.power(dJdw, 2)       #Variance
        self.v_db = beta2*self.v_db + (1-beta2)*np.power(dJdb, 2)

        m_corr = 1-beta1**epoch                                         #Bias corrector terms
        v_corr = 1-beta2**epoch

        self.w -= learning_rate * np.divide(self.m_dw/m_corr, np.power(self.v_dw/v_corr,0.5)+epsilon)
        self.b -= learning_rate * np.divide(self.m_db/m_corr, np.power(self.v_db/v_corr,0.5)+epsilon)
        return dJdx

class ANN:                                                              #X and Y must be in the matrix form (nºinputs or outputs,dataset size)

    lossfunctions = {
        'logloss': (logloss, d_logloss),
        'MSE': (MSE, d_MSE)
    }

    def __init__(self, nodes, activation, lossname):
        self.nlayers = len(activation)
        if len(activation)!=(len(nodes)-1): print('Error, check activation vector length = node array length')
        self.layers = [None]*self.nlayers                               #Layer creation
        for i in range(self.nlayers):
            self.layers[i] = Layer(nodes[i],nodes[i+1],activation[i])
        self.loss, self.d_loss = self.lossfunctions.get(lossname)

    def test(self, x):
        for layer in self.layers:
            x = layer.feedforward(x)
        return x
    
    def testwithplot(self, x, mtest, nodes):   #mtest has to be 1
        l = 0
        A = {}
        for layer in self.layers:
            A[l] = x
            l += 1
            x = layer.feedforward(x)
        A[l] = x

        if mtest==1:
            W = {}
            for l in range(self.nlayers):
                W[l+1] = self.layers[l].w
            neuralplot(nodes, True, W, A)
        return x
    
    def train(self, x_train, y_train, epochs, optimizer, lr, lambd):
        costs = []
        if optimizer == 'GD':                                           #Gradient descent
            for epoch in range(epochs):
                #if epoch % 150 == 0: print('Epoch nº'+str(epoch))
                y = self.test(x_train)

                dJdy = self.d_loss(y_train, y)
                for layer in reversed(self.layers):
                    dJdy = layer.backprop(dJdy, lr, lambd)

                J = np.sum(self.loss(y_train, y))
                costs.append(J)

        elif optimizer == 'SGD':                                        #Stochastic GD
            for epoch in range(epochs):
                if epoch % 50 == 0: print('Epoch nº '+str(epoch))
                for i in range(y_train.shape[1]):
                    y = self.test(x_train.T[i].reshape(-1,1))

                    dJdy = self.d_loss(y_train.T[i].reshape(-1,1), y)
                    for layer in reversed(self.layers):
                        dJdy = layer.backprop(dJdy, lr, lambd)
                y = self.test(x_train)
                J = np.sum(self.loss(y_train, y))
                costs.append(J)

        elif optimizer == 'Adam':                                       #Adam stochastic optimizer, recommended values: alfa = 0.001, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-08
            beta1 = 0.9
            beta2 = 0.999
            epsilon = 1e-08
            for epoch in range(epochs):
                if epoch % 50 == 0: print('Epoch nº '+str(epoch))
                for i in range(y_train.shape[1]):
                    y = self.test(x_train.T[i].reshape(-1,1))

                    dJdy = self.d_loss(y_train.T[i].reshape(-1,1), y)
                    for layer in reversed(self.layers):
                        dJdy = layer.Adam_backprop(dJdy, lr, lambd, beta1, beta2, epsilon, epoch+1)
                
                y = self.test(x_train)
                J = np.sum(self.loss(y_train, y))
                costs.append(J)
        else:
            print('No optimizer found. This library offers GD, SGD and Adam')

        return costs