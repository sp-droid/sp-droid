# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
import numpy as np
import matplotlib.pyplot as plt


# %%
def latin_hypercube_2d_uniform(n):  #n == number of samples
    lower_limits = np.arange(0, n)/n
    upper_limits = np.arange(1, n+1)/n

    points = np.random.uniform(low=lower_limits, high=upper_limits, size=[2, n]).T
    np.random.shuffle(points[:,1])

    return points


# %%
n = 6
p = latin_hypercube_2d_uniform(n)

plt.figure(figsize=[5, 5])
plt.xlim(0, 1)
plt.ylim(0, 1)
plt.scatter(p[:,0], p[:,1], c = 'r')

for i in np.arange(0, 1+1/n, 1/n):
    plt.axvline(i)
    plt.axhline(i)
plt.show()


# %%
p


