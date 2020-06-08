
# Libraries
import importlib
import pandas as pd
import plotly.graph_objects as go
from datetime import datetime
from plotly.subplots import make_subplots
from IPython.display import Image
import timeit

sim = pd.read_csv("data/Res_simul_00.txt", header=None, delimiter=r"\s+")
#sim = pd.read_csv("data/Res_simul_13.txt", header=None, delimiter=r"\s+")

sim = sim[[0,6]]
sim = sim.rename(columns={0: 't'})
sim = sim.rename(columns={6: 'deaths_sim'})
ori = pd.read_csv("data/export000.csv")
fin = pd.merge(sim,ori,on='t',how='left').reset_index()
fin.loc[len(ori)-1:len(fin),'date'] = pd.date_range(start=ori.date.iloc[-1],periods=(len(fin)-len(ori)+1))
fin['date'] = pd.to_datetime(fin['date'])

fin.to_csv(r'data/exportfinal_002.csv', index = False)
#fin.to_csv(r'data/exportfinal_132.csv', index = False)
