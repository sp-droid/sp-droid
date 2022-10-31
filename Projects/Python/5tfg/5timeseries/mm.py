import numpy as np
import matplotlib.pyplot as plt
import myML

import keras
from keras.models import Sequential
from keras.layers import Dense

def weight(x_t):
    Sw, Wfw, A, Lambda, q, lambda2, tc, Nz, Wdg, Wp = x_t
    Lambda = Lambda/360*2*np.pi
    w = 0.036*Sw**0.758*Wfw**0.0035*(A/np.cos(Lambda)**2)**0.6*q**0.006*lambda2**0.04
    w = w*(100*tc/np.cos(Lambda))**(-0.3)*(Nz*Wdg)**0.49+Sw*Wp
    return w

m = 100
limax = [200, 300, 10, 10, 45, 1, 0.18, 6, 2500, 0.08]
limin =  [150, 220, 6, -10, 16, 0.5, 0.08, 2.5, 1700, 0.025]

x_train = myML.staticSampling('LHS', d=10, npoints=m)
xnorm = myML.norm0to1_minmax(limax, limin)

y_train = np.array([weight(xnorm.recover(x_train))])
deletelist = []
for i in range(m):
    if np.isnan(y_train[0,i]) or y_train[0,i]> 500: deletelist.append(i)

y_train = np.delete(y_train, deletelist, axis=1)
print(y_train.shape)
ynorm = myML.norm0to1(y_train)
y_train = ynorm.normalize(y_train)
x_train = np.delete(x_train, deletelist, axis=1)

# neuralnetwork = myML.ANN(nodes=[1,200,1],activation=['tanh', 'tanh'],lossname='MSE')
# normt = myML.norm0to1(t)
# normy = myML.norm0to1(y)
# costs = neuralnetwork.train(t, y, epochs=500, optimizer='SGD', lr=0.01, lambd=0)
# y_ANN = neuralnetwork.test(t)

model = Sequential()
model.add(Dense(10, input_dim=10, activation='relu'))
model.add(Dense(1, activation='relu'))

model.compile(loss='mse', optimizer='Adam', metrics=['accuracy'])
history = model.fit(x_train.T, y_train.T, epochs=3000, batch_size=100)
y_keras = model.predict(x_train.T).T

plt.plot(history.history['loss'],label='ANN_keras')
plt.title('Model loss') 
plt.ylabel('Loss') 
plt.xlabel('Epoch')
plt.show()

# fig = plt.figure(figsize=(20,6))
# ax = plt.axes()
# ax.scatter(t[0,:],y[0,:])
# ax.plot(t[0,:],y_ANN[0,:],label='ANN')
# ax.plot(t[0,:],y_keras[0,:],label='ANN_keras')
# ax.legend()
# plt.show()