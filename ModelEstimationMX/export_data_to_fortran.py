
# Libraries
import importlib
import pandas as pd
import plotly.graph_objects as go
from datetime import datetime
from plotly.subplots import make_subplots
from IPython.display import Image
import timeit

# Get Covid-19 data Mx
exec(open('Get_data_mx.py').read())
exec(open('Organize_data_mx.py').read())

# Exports datas
I0  = 149 
mxc = x0c[x0c['RESULTADOl0']>=I0].reset_index()
mxd = x0d[x0d.index>mxc.date[0]].reset_index()
base000   = mxd[['date']]
add000    = x000[['date','0_y','13_y','20_y']]
export000 = pd.merge(base000,add000,on='date',how='left').reset_index()
export000 = export000.rename(columns={'index': 't'})
export001 = export000[['t','0_y','13_y','20_y']]
export001 = export001.fillna(-10000)
export000.to_csv(r'data/export000.csv', index = False)
export001.to_csv(r'data/export001.csv', index = False)

