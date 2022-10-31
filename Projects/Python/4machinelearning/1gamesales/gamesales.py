# %%
import pandas as pd

df = pd.read_csv('vgsales.csv')
print(df.shape)
df.describe()


# %%
df.values
