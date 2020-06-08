
# Libraries
import importlib
import pandas as pd
import plotly.graph_objects as go
from datetime import datetime
from plotly.subplots import make_subplots
from IPython.display import Image
import timeit

# Get Covid-19 data Mx
exec(open('Get_data_en.py').read())

## export
export001 = export000[['t','0_y','13_y','20_y']]
export001 = export001.fillna(-10000)
export000.to_csv(r'data/export000.csv', index = False)
export001.to_csv(r'data/export001.csv', index = False)
