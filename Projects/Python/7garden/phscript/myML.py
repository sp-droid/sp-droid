import numpy as np
import matplotlib.pyplot as plt
import networkx as nx
import pickle

#############################################################
"""
Feed Forward Neural Networks (neuralplot, ANN)
"""
#############################################################

#Feed Forward NN visualizer ([list with nodes per layer], condition, weights, activations)
#The condition is False when we just want to draw a NN without real weights, True otherwise
def neuralplot(nodes, condition, W, A):
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


## Activation functions
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

def softmax(x):                                         #Sum of outputs = 1, pseudoprobability
    e_x = np.exp(x-np.max(x))
    return e_x / e_x.sum(axis=0)

## Loss functions, y==true value, y_p==predicted value
def logloss(y,y_p):                                       #Cross-entropy, log loss --> Classification problems
    m = y.shape[1]
    return -1/m*(y*np.log(y_p) + (1-y)*np.log(1-y_p))
def d_logloss(y,y_p):
    m = y.shape[1]
    return 1/m*(y_p - y)/(y_p*(1 - y_p))

def MSE(y,y_p):                                           #Mean squared error  --> Regression without outliers
    m = y.shape[1]
    return 1/2/m*(y-y_p)**2
def d_MSE(y,y_p):
    m = y.shape[1]
    return -1/m*(y-y_p)

def MAE(y,y_p):                                           #Mean absolute error  --> Regression. Bad results so far
    m = y.shape[1]
    return 1/m*np.abs(y-y_p)
def d_MAE(y,y_p):
    m = y.shape[1]
    return -1/m*np.sign(y-y_p)

class Layer:
    activationFunctions = {
        'tanh': (tanh, d_tanh),
        'sigmoid': (sigmoid, d_sigmoid),
        'ReLU': (ReLU, d_ReLU),
        'leakyReLU': (leakyReLU, d_leakyReLU),
        'linear': (linear, d_linear)
    }

    def __init__(self, ninputs, neurons, activationF):                   #Number of inputs, neurons & type of activation function
        self.w = np.random.uniform(-1,1,(neurons, ninputs))             #Initial weights
        self.b = np.zeros((neurons, 1))                                 #Initial biases
        self.act, self.d_act = self.activationFunctions.get(activationF)

        self.m_dw = np.zeros((neurons, ninputs))                         #First and second moments, mean and uncentered variance, weights
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

#Feed Forward Neural Network ([list with nodes per layer], [list with the name of the desired activation function in each non-input layer], [loss function name])
#@test (X[d,m])
#@testwithplot (X[d,m], numberoftestingsamples=1 (always, this is a failsafe), [list with nodes per layer])
#@train (X[dx,m], Y[dy,m], nºepochs, type of optimizer, learning rate, regularization term)
class ANN:
    lossfunctions = {
        'logloss': (logloss, d_logloss),
        'MSE': (MSE, d_MSE),
        'MAE': (MAE, d_MAE)
    }

    def __init__(self, nodes, activation, lossname):
        self.nlayers = len(activation)
        if len(activation)!=(len(nodes)-1): print('Error, check activation vector length = node array length')
        self.layers = [None]*self.nlayers                               #Layer creation
        for i in range(self.nlayers):
            self.layers[i] = Layer(nodes[i],nodes[i+1],activation[i])
        self.loss, self.d_loss = self.lossfunctions.get(lossname)

    ## STORAGE
    def storelayers(self):
        with open('myML/NNdata.pkl', 'wb') as f: #wb is necessary, write-binary
            pickle.dump([self.layers, self.loss, self.d_loss], f)
        print('Stored in myML/NNdata.pkl')

    def loadlayers(self, DIR): #Dir like 'Q:/Pablo/Proyectos/2Lolrelated/winrateselect/dataset/NNdata.pkl' with forward slashes
        with open(DIR, 'rb') as f: #rb is necessary, read-binary
            self.layers, self.loss, self.d_loss = pickle.load(f)
    ## STORAGE

    def test(self, x):
        self.typealert(x)
        for layer in self.layers:
            x = layer.feedforward(x)
        return x
    
    def testwithplot(self, x, mtest, nodes):   #mtest has to be 1
        self.typealert(x)
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
    
    def train(self, x_train: np.ndarray, y_train: np.ndarray, epochs, optimizer, lr, lambd, printepochs=2000):
        self.typealert(x_train, y_train)
        costs = []
        for epoch in range(epochs):
            if printepochs != False: 
                if (epoch+1) % printepochs == 0: print('Epoch nº'+str(epoch))
            if optimizer == 'GD':                                           #Gradient descent
                y = self.test(x_train)

                dJdy = self.d_loss(y_train, y)
                for layer in reversed(self.layers):
                    dJdy = layer.backprop(dJdy, lr, lambd)

                J = np.sum(self.loss(y_train, y))
                costs.append(J)

            elif optimizer == 'SGD':                                        #Stochastic GD
                for i in range(y_train.shape[1]):
                    y = self.test(x_train.T[i].reshape(-1,1))

                    dJdy = self.d_loss(y_train.T[i].reshape(-1,1), y)
                    for layer in reversed(self.layers):
                        dJdy = layer.backprop(dJdy, lr, lambd)
                y = self.test(x_train)
                J = np.sum(self.loss(y_train, y))
                costs.append(J)

            elif optimizer == 'Adam':                                       #Adam stochastic optimizer, recommended values: alfa = 0.001, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-08
                beta1, beta2, epsilon = 0.9, 0.999, 1e-08
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
                break

        return costs
    
    def typealert(self, *arguments):
        for entry in arguments:
            if type(entry) != np.ndarray: raise ValueError('X and Y must be ndarrays, not np.matrix or similar')
        

#############################################################
"""
Gaussian Process
"""
#############################################################
##Kernels
def exponentiatedquadratic(deltax, theta):
    Kij = np.exp(-theta*np.sum(deltax**2, axis=0))
    return Kij

#Gaussian Process Regression (kernel's name)
#@fit (X[d,m], y[1,m]) (Introduce training data)
#@predict (X[d,m]) (Request predictions on a test set)
#@score (X[d,m], y[1,m]) (Predicts y* based on a test set X and compares to the true y)
class GaussianProcess:
    kernel_list = {
        'gaussian': exponentiatedquadratic
    }

    def __init__(self, kernel):
        self.kernel = self.kernel_list.get(kernel)

    #Makes the correlation matrix from the feature matrix (n features, m samples)
    def Corr(self, X1, X2, theta):
        K = np.zeros((X1.shape[1],X2.shape[1]))
        for i in range(X1.shape[1]):
            for j in range(X2.shape[1]):
                K[i,j] = self.kernel(X1[:,i] - X2[:,j], theta)

        return K
    
    def likelihood(self, theta):
        theta = 10**(theta)
        m_train = self.X.shape[1]
        one = np.ones((m_train, 1))

        K = self.Corr(self.X, self.X, theta) + np.eye(m_train)*1e-10
        K_inv = np.linalg.pinv(K)

        #Optimum mean for maxL
        mu = (one.T @ K_inv @ self.y.T) / (one.T @ K_inv @ one)
        #Optimum variance for maxL
        var = (self.y.T-mu*one).T @ K_inv @ (self.y.T - mu*one) / m_train

        #Log likelihood
        lnL = -(m_train/2)*np.log(var) - 0.5*np.log( np.linalg.det(K) )
        self.K, self.K_inv, self.mu, self.var = K, K_inv, mu, var
        return lnL.flatten()

    def fit(self, X, y, iters):
        self.X, self.y = X, y
        theta_upper = [2]
        theta_lower = [-3]

        #Optimize
        self.theta = SA(theta_upper, theta_lower, self.likelihood, iters)
        self.lnL = self.likelihood(self.theta)
        self.theta = 10**(self.theta)
        print(self.theta)

    def predict(self, X_test):
        m_train = self.X.shape[1]
        one = np.ones((m_train, 1))

        #Correlation matrix between test and train samples
        K_test = self.Corr(self.X, X_test, self.theta)

        #Mean prediction and variance
        mu_test = self.mu + (K_test.T @ self.K_inv @ (self.y.T-self.mu*one)).T
        var_test = self.var * (1 - np.diag(K_test.T @ self.K_inv @ K_test).T)
        return mu_test, var_test

    def score(self, X_test, y_test):
        y_pred, var = self.predict(X_test)
        RMSE = np.sqrt(np.mean((y_pred - y_test)**2))
        return RMSE


#############################################################
"""
Kriging
"""
#############################################################
def basisfunc(x, p, theta):
    phi = np.exp(-theta*np.sum(np.abs(x)**p))
    return phi

#Kriging (TrainingInput[d,m], TrainingOutput[1,m])
#@interpol (X[d,m])
class kriging:
    def __init__(self, trainx, trainy):
        self.n = trainx.shape[1]
        self.tx = trainx
        self.ty = trainy
        self.mean = np.mean(trainy)

        self.optimizeptheta(iters=3000)
        print(self.p)
        print(self.theta)
        self.calculateweights()
    
    def lnL(self, vector):
        p = vector[0,0]
        theta = vector[1,0]
        R = np.zeros((self.n,self.n))
        for i in range(self.n):
            for j in range(self.n):
                R[i,j] = basisfunc(self.tx[0,j]-self.tx[0,i],p,theta)

        # Maximum likelihood estimate of the variance MLv
        MLv = (self.ty-self.mean) @ np.linalg.pinv(R) @ (self.ty-self.mean).T
        if MLv[0,0] < 1e-14: return float("NaN")
        lnL = (-self.n/2*np.log(MLv)-0.5*np.linalg.det(R))[0,0]
        return lnL

    def optimizeptheta(self, iters):
        lim_max = [2, 15]
        lim_min = [0, 0]
        x1 = SA(lim_max, lim_min, self.lnL, iters)
        self.p = x1[0,0]
        self.theta = x1[1,0]
        print(self.lnL(x1))
        return

    def calculateweights(self):
        R = np.zeros((self.n,self.n))
        for i in range(self.n):
            for j in range(self.n):
                R[i,j] = basisfunc(self.tx[0,j]-self.tx[0,i],self.p,self.theta)
        #Maximum likelihood estimate of the mean
        self.Rinv = np.linalg.pinv(R)
        self.MLm = np.sum(self.Rinv @ self.ty.T)/np.sum(self.Rinv)
        self.weights = (self.ty-self.MLm) @ self.Rinv
        return
    
    def interpol(self, points):
        m = points.shape[1]
        y = np.zeros((1,m))
        deviation = np.zeros((1,m))
        phinm = np.zeros((self.n,1))
        MLv = (self.ty-self.mean) @ self.Rinv @ (self.ty-self.mean).T
        
        for j in range(m):
            for i in range(self.n):
                phinm[i,0] = basisfunc(points[0,j]-self.tx[0,i],self.p,self.theta)
            y[0,j] = self.MLm + self.weights @ phinm
            deviation[0,j] = MLv*(1-phinm.T @ self.Rinv @ phinm + (1 - np.sum(self.Rinv @ phinm))/np.sum(self.Rinv))
            if deviation[0,j]<0: deviation[0,j] = deviation[0,j]*-1

        return y, deviation**0.5

    
#############################################################
"""
Global Optimization [continuous variables]
"""
#############################################################
#Simulated Annealing (maxlim=[1,10,-2...], minlim=[2,11,0...], @func to maximize, iters=1000)
def SA(lmax, lmin, func, iters):
    dim = len(lmax)
    norm = norm0to1_minmax(lmax, lmin)
    x = np.random.normal(0,1,(dim,1))
    y = func(norm.recover(x))
    Temperature = 4
    xnew = x*1
    for i in range(iters):
        c = (iters-i)/iters
        for d in range(dim):
            value = x[d,0]*(1-c)
            xnew[d,0] = np.random.uniform(value, value+c)
        ynew = func(norm.recover(xnew))
        Temperature = Temperature*0.9
        if np.isnan(ynew) or np.log(np.random.rand())*Temperature > (ynew-y): continue
        else: 
            x = xnew*1
            y = ynew*1
    return norm.recover(x)

#Particle Swarm Optimization (maxlim=[1,10,-2...], minlim=[2,11,0...], @func to maximize, iters=1000)
def PSO(lmax, lmin, func, iters):
    #Initial locations through LHS sampling
    d = len(lmax)
    norm = norm0to1_minmax(lmax, lmin)
    if d==1:
        npart = 2
        X = np.array([[0, 1]])
    else:
        npart = min(2**(2*d), 100)
        X = staticSampling('LHS', d, npart)

    #Initialization
    swarm = {
        'p': {},        #Individual particle
        'sb': []        #Swarm best
    }
    for p in range(npart):
        swarm['p'][p] = {
            'st': {},
            'pb': {},
            've': {},
        }
        swarm['p'][p]['st'][0] = np.array([X[:,p]]).T
        swarm['p'][p]['st'][1] = func(norm.recover(swarm['p'][p]['st'][0]))
        swarm['p'][p]['pb'] = swarm['p'][p]['st'].copy()
        swarm['p'][p]['ve'][0] = np.zeros((d,1))
    swarm['sb'] = swarm['p'][np.random.choice(npart, 1)[0]]['st'].copy()
    
    #Hyperparameters
    wmax = 0.9  #Weight coefficient
    wmin = 0.4
    c1 = 2.05   #Cognitive (personal effect) coefficient
    c2 = 2.05   #Social (swarm effect) coefficient

    #Main loop, simultaneous updates
    for k in range(iters):
        w = (wmax-wmin)*(iters-k)/iters+wmax
        for p in range(npart):  #Swarm best
            if swarm['sb'][1]<swarm['p'][p]['st'][1]: swarm['sb'] = swarm['p'][p]['st'].copy()

        for p in range(npart):  #Update motion of each particle, as well as its personal best
            temp = np.random.uniform(0,1,2)
            swarm['p'][p]['ve'][0] = w*swarm['p'][p]['ve'][0] + temp[0]*c1*(swarm['p'][p]['pb'][0]-swarm['p'][p]['st'][0]) + temp[1]*c2*(swarm['sb'][0]-swarm['p'][p]['st'][0])
            swarm['p'][p]['st'][0] = swarm['p'][p]['st'][0] + 1/iters*swarm['p'][p]['ve'][0]
            for i in range(d):  #Check in case a particle ventures out of the limited domain
                if swarm['p'][p]['st'][0][i,0] > 1:
                    swarm['p'][p]['st'][0][i,0] = 1
                    swarm['p'][p]['ve'][0][i,0] = swarm['p'][p]['ve'][0][i,0]*0
                elif swarm['p'][p]['st'][0][i,0] < 0:
                    swarm['p'][p]['st'][0][i,0] = 0
                    swarm['p'][p]['ve'][0][i,0] = swarm['p'][p]['ve'][0][i,0]*0
            swarm['p'][p]['st'][1] = func(norm.recover(swarm['p'][p]['st'][0]))
            if swarm['p'][p]['st'][1]>swarm['p'][p]['pb'][1]: swarm['p'][p]['pb'] = swarm['p'][p]['st'].copy() #Particle best

    return norm.recover(swarm['sb'][0])

#Genetic Algorithm (maxlim=[1,10,-2...], minlim=[2,11,0...], @func to maximize, generations=300, populationsize=100, initial_offspring=2)
def GA(lmax, lmin, func, generations, popsize, offspring):
    #Initial population through LHS sampling
    genes = len(lmax)
    norm = norm0to1_minmax(lmax, lmin)
    if genes==1:
        X = np.random.uniform(0, 1, (genes, popsize))
    else:
        X = staticSampling('LHS', genes, popsize)
    Y = func(norm.recover(X))

    #Main loop
    for g in range(1,generations):
        parentindex = np.argpartition(-Y[0], 2)[:2]                                 #Parents of the next generation
        tokillindex = np.argpartition(Y[0], offspring)[:offspring]                  #Replacement
        for child in range(offspring):                                              #Crossover
            cutoff = np.random.choice(genes, 1)[0]
            X[:,tokillindex[child]] = X[:,parentindex[0]]
            if child % 2 == 0:
                for gene in range(cutoff): X[gene,tokillindex[child]] = max(1,X[gene,parentindex[1]]*np.random.uniform(0.99,1.01))
            else:
                for gene in reversed(range(cutoff)): X[gene,tokillindex[child]] = max(1,X[gene,parentindex[1]]*np.random.uniform(0.99,1.01))
        
        for individual in range(popsize):                                           #Mutation
            if individual in tokillindex or individual in parentindex: continue
            for gene in range(genes):
                X[gene,individual] = np.random.uniform(0,1)
        
        Y = func(norm.recover(X))
        if offspring<popsize*0.95 and g>popsize*0.5: offspring += 1

    best = np.argpartition(-Y[0], 1)[:1]
    return norm.recover(np.array([X[:,best[0]]]).T)

#############################################################
"""
Normalization (norm0to1_minmax, norm0to1)
"""
#############################################################
#Normalize knowing the true limits along every dimension of the set variables (max=[maxd1, maxd2, ...], min=[mind1, mind2, ...])
#@Normalize or @recover a matrix of points (X[d,m])
class norm0to1_minmax: #Knowing the sampling plan before, one must know the limits of each variable
    def __init__(self, maxx, minn):       #Vectors containing the maximums and minimums along each dimension
        self.d = len(maxx)
        self.max = maxx
        self.min = minn

    def normalize(self, X):                  #X is a matrix with dimension-rows and points-columns
        npoints = X.shape[1]
        if X.shape[0] != self.d: print('Unmatching dimensions: '+str(self.d)+' vs. '+str(X.shape[0]))
        Y = X*1.0
        Y = Y.astype('float64')
        for i in range(self.d):
            for j in range(npoints):
                Y[i,j] = (Y[i,j]-self.min[i])/(self.max[i]-self.min[i])
        return Y

    def recover(self, X):
        npoints = X.shape[1]
        if X.shape[0] != self.d: print('Unmatching dimensions: '+str(self.d)+' vs. '+str(X.shape[0]))
        Y = X*1.0
        for i in range(self.d):
            for j in range(npoints):
                Y[i,j] = Y[i,j]*(self.max[i]-self.min[i])+self.min[i]
        return Y

#Normalize NOT knowing the true limits of the variables (X[d,m])
#@Normalize or @recover a matrix of points (X[d,m])
class norm0to1: #Knowing the initial dataset
    def __init__(self, X, array_format='inverted'):
        '''
        Default array "format" is inverted or (row=categories, column=samples).
        If your array is like a normal pandas dataframe (row=samples, column=categories), specify array_format='normal'
        '''
        self.array_format = array_format
        if self.array_format!='inverted': X = X.T
        if X.shape[0] > X.shape[1]: print('Number of samples smaller than number of categories. If you got an error be sure to specify the right "array_format" class var')

        self.d = X.shape[0]
        self.max, self.min = [None]*self.d, [None]*self.d
        for i in range(self.d):
            if type(X[i,0])==str: continue
            self.max[i] = np.amax(X[i,:])
            self.min[i] = np.amin(X[i,:])

    def normalize(self, X):                  #X is a matrix with dimension-rows and points-columns
        if self.array_format!='inverted': X = X.T
        samplesize = X.shape[1]
        if X.shape[0] != self.d: print('Unmatching dimensions: '+str(self.d)+' vs. '+str(X.shape[0]))
        Y = np.copy(X)
        for i in range(self.d):
            if self.max[i] == None: continue
            for j in range(samplesize):
                Y[i,j] = (Y[i,j]-self.min[i])/(self.max[i]-self.min[i])
        if self.array_format!='inverted': return Y.T
        return Y

    def recover(self, X):
        if self.array_format!='inverted': X = X.T
        samplesize = X.shape[1]
        if X.shape[0] != self.d: print('Unmatching dimensions: '+str(self.d)+' vs. '+str(X.shape[0]))
        Y = np.copy(X)
        for i in range(self.d):
            if self.max[i] == None: continue
            for j in range(samplesize):
                Y[i,j] = Y[i,j]*(self.max[i]-self.min[i])+self.min[i]
        if self.array_format!='inverted': return Y.T
        return Y

#############################################################
"""
Sampling Techniques (staticSampling)
"""
#############################################################
#Static Sampling (Type of LH, space dimension d, number of points p)
def staticSampling(ntype, d, npoints):
    p = npoints
    def fullrandom(p):                       #Uniformly spread randomized points
        matrix = np.zeros((d,p))    #Matrix with dimensions-rows and points-columns
        for j in range(p):
            matrix[:,j] = np.random.uniform(low=0, high=1, size=d)
        return matrix

    def fullfactorial(p):           #The given p is the number of divisions along the first dimension, the true number of points is (p+d-1)!/(d-1)!
        pd = p*1
        p = pd*1
        matrix = np.array([range(pd)])/(pd-1)
        for i in range(1,d):
            pd = pd+1
            p = p*pd
            matrixtemp = matrix*1
            matrix = np.concatenate((matrix, np.zeros((1,int(p/pd)))), axis=0)
            for j in range(1,pd):
                temp = np.concatenate((matrixtemp, np.ones((1,int(p/pd)))*j/(pd-1)), axis=0)
                matrix = np.concatenate((matrix, temp), axis=1)
        print(str(p)+' points')
        return matrix, p


    def LS(p):                        #Latin sample divided in p partitions, and a point in the middle to illustrate them
        psize = 1/p
        matrix = np.zeros((d,p))
        for i in range(d): matrix[i,:] = np.random.choice(p, p, replace=False)
        return (np.ones_like(matrix)*0.5+matrix)*psize

    def LSoptim(p):                   #LS optimized through pairwise random permutations
        matrix = LS(p)
        iters = int(p*2+800)

        for k in range(iters):
            points = np.random.choice(p,2,replace=False)
            dim = np.random.choice(d,1,replace=False)[0]
            temp = matrix[dim,points[0]]

            phiold = phip_reduced(matrix, p, points[0], points[1])
            matrix[dim,points[0]] = matrix[dim,points[1]]
            matrix[dim,points[1]] = temp
            phinew = phip_reduced(matrix, p, points[0], points[1])
            if phinew>phiold:
                matrix[dim,points[1]] = matrix[dim,points[0]]
                matrix[dim,points[0]] = temp
        return matrix

    def LHS(p):                               #LS with random points inside the partitions
        psize = 1/p
        matrix = LS(p)
        return np.random.uniform(low=matrix-0.1*psize*np.ones_like(matrix), high=matrix+0.1*psize*np.ones_like(matrix), size=(d,p))

    def LHSoptim(p):                          #LSoptim with random points inside the partitions
        psize = 1/p
        matrix = LSoptim(p)
        return np.random.uniform(low=matrix-0.1*psize*np.ones_like(matrix), high=matrix+0.1*psize*np.ones_like(matrix), size=(d,p))

    def SLHS(p):                               #Sliced Latin Hypercube Sample or Design, divided in T slices of LHS M partitions, resulting in t x m = p (points), ##WORK IN PROGRESS
        T = 3 #Levels of the categorical variable, nº of slices
        M = 4 #Points per slice
        p = M*T
        psize = 1/p
        X = {}
        matrix = np.zeros((d,p))
        for t in range(T):
            X[t] = np.zeros((d,M),dtype=int)
            for i in range(d): X[t][i,:] = np.random.choice(M, M, replace=False)
        for i in range(d):
            for m in range(M):
                temp = np.random.choice(np.arange(m*T,m*T+T),T,replace=False)
                for t in range(T):
                    index = np.where(X[t][i,:]==m)[0][0]
                    matrix[i,index+M*t] = temp[t]
        return np.random.uniform(low=psize*matrix, high=psize*matrix+psize*np.ones_like(matrix), size=(d,p))

    def TPLHS(p):                              #Translational Propagation Latin Hypercube Sample or Design
        nb = 2**d               #Number of blocks
        pb = int(np.ceil(p/nb)) #Number of points per block
        mp = int(pb*nb)         #Number of points before trimming the design to match p points

        if pb>1: seedl = LSoptim(int(pb))
        elif pb==1: seedl = 0.5*np.ones((d,1))
        X = seedl*(mp/2)-np.ones_like(seedl)

        for i in range(d):      #Construction of the LH
            temp = X[:,:] + np.ones_like(X)
            temp[i,:] = temp[i,:] + (mp/2-1)*np.ones_like(temp[i,:])
            X = np.concatenate((X, temp), axis=1)            

        for k in range(mp-p):   #Reducing and scaling to p points
            index = np.where(X[0,:]==np.amax(X[0,:]))[0][0]
            for i in range(d):
                for j in range(len(X[i,:])):
                    if j==index: continue
                    elif X[i,j]>X[i,index]: X[i,j] = X[i,j]-1
            X = np.delete(X, index, axis=1)
        return X*(1/(p-1))

    def iter_maximin(p):                       #Maximin design by iteration
        psize = 1/p
        matrix = fullrandom(p)
        iters = 150
        a = 1/iters*np.log(0.0001/0.1)

        for k in range(iters):
            step = 0.1*np.exp(a*k)

            for i in range(p):
                dold = d
                point = matrix[:,i]
                for j in range(p):
                    if i==j: continue
                    dnew = np.sum((point-matrix[:,j])**2)
                    if dnew<dold:
                        dold = dnew
                        index = j
                if distancetowall(point)<psize:
                    matrix[:,i] = (1-step)*point+0.5*step*np.ones(d)
                    continue
                matrix[:,i] = (1+step)*point-step*matrix[:,index]
        return matrix
   
    def phip_reduced(matrix, p, index1, index2):
        if p>160:                               #For very large values of p, we can't compute phi but it's equal to 1/mindist in the limit
            k_old = np.sqrt(np.sum((matrix[:,index1]-matrix[:,index2])**2))
            for j in range(p):
                if j==index1 or j==index2: continue
                k = np.sum((matrix[:,index1]-matrix[:,j])**2)**0.5
                if k<k_old: k_old = k
                k = np.sum((matrix[:,index2]-matrix[:,j])**2)**0.5
                if k<k_old: k_old = k
            return 1/k_old

        phi = np.sum((matrix[:,index1]-matrix[:,index2])**2)**(-0.5*p)
        for j in range(p):
            if j==index1 or j==index2: continue
            phi += np.sum((matrix[:,index1]-matrix[:,j])**2)**(-0.5*p)
            phi += np.sum((matrix[:,index2]-matrix[:,j])**2)**(-0.5*p)
        phi = phi**(1/p)
        return phi

    def distancetowall(vector):
        dist0 = np.amin(vector)
        dist1 = np.amax(vector)
        return np.minimum(dist0, 1-dist1)

    if ntype=='fullrandom': matrix = fullrandom(p)
    elif ntype=='fullfactorial': matrix, p = fullfactorial(p)
    elif ntype=='LS': matrix = LS(p)
    elif ntype=='LSoptim': matrix = LSoptim(p)
    elif ntype=='LHS': matrix = LHS(p)
    elif ntype=='LHSoptim': matrix = LHSoptim(p)
    elif ntype=='SLHS': matrix = SLHS(p)
    elif ntype=='TPLHS': matrix = TPLHS(p)
    elif ntype=='iter_maximin': matrix = iter_maximin(p)
    else: print('Error, LHS method not found')

    return matrix

#############################################################
"""
Miscellaneous
"""
#############################################################
class negative:
    def __init__(self, f):
        self.func = f
    def neg(self, x): return self.func(x)*-1