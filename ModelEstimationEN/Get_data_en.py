## Libraries

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from datetime import timedelta
from datetime import datetime
from plotly.subplots import make_subplots
import sys
from IPython.display import Image
import plotly.io as pio

#Data from:: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/

## File for total at last
directory = '/Users/tgst/Desktop/Covid-19-Mx/Data_en/'

x = pd.read_excel(directory+'/'+'COVID-19-total-announced-deaths-8-June-2020.xlsx', sheet_name='Tab1 Deaths by region', index_col=None, header=None)
x = x.fillna(-100000)

y0 = x.loc[15,:].values.tolist()
y1 = x.loc[16,:].values.tolist()
date   = []
deaths = []

for j in range(4,len(y0)+1):
    if y1[j] > -10000:
        date.append(y0[j])
        deaths.append(y1[j])
        #print(j,y0[j],y1[j])
    else:
        break
        
df = pd.DataFrame(date) 
df['Tdeaths'] = deaths
df = df.rename(columns={0: 'date'})


## Files for new deaths
fsel = ['8-June',
    '7-June','6-June','5-June','4-June','3-June','2-June','1-June',
    '31-May','30-May','29-May','28-May','27-May','26-May','25-May',
    '24-May','23-May','22-May','21-May','20-May','19-May','18-May','17-May','16-May','15-May',
    '14-May','13-May','12-May','11-May','10-May','9-May','8-May','7-May','6-May','5-May',
    '4-May','3-May','2-May','1-May',
    '30-April','29-April','28-April','27-April','26-April','25-April',
    '24-April','23-April','22-April','21-April','20-April','19-April','18-April','17-April','16-April','15-April',
    '14-April','13-April','12-April','11-April','10-April','9-April','8-April','7-April','6-April','5-April',
    '4-April','3-April','2-April'
]

isel = ['608','607','607','605','604',
    '603','602','601','531','530','529','528','527','526','525','524','523','522','521','520',
    '519','518','517','516','515','514','513','512','511','510','509','508','507','506','505',
    '504','503','502','501','430','429','428','427','426','425','424','423','422','421',
    '420','419','418','417','416','415','414','413','412','411','410','409','408','407','406',
    '405','404','403','402'
]

for i in range(0,len(fsel)-1):
    #print(fsel[i])

    if datetime.strptime(fsel[i], "%d-%B") < datetime.strptime('21-May', "%d-%B"):
        x = pd.read_excel(directory+'/'+'COVID-19-daily-announced-deaths-'+fsel[i]+'-2020.xlsx', sheet_name='COVID19 daily deaths by region', index_col=None, header=None)
    else:
        x = pd.read_excel(directory+'/'+'COVID-19-daily-announced-deaths-'+fsel[i]+'-2020.xlsx', sheet_name='Tab1 Deaths by region', index_col=None, header=None)

    nnn = x[x[1]=='England'].index.values

    x = x.fillna(-100000)
    y0 = x.loc[nnn[0]-1,:].values.tolist()
    y1 = x.loc[nnn[0],:].values.tolist()
    date   = []
    deaths = []

    for j in range(3,len(y0)+1):
        if y1[j] > -10000:
            date.append(y0[j])
            deaths.append(y1[j])
            #print(j,y0[j],y1[j])
        else:
            break

    df0 = pd.DataFrame(date) 
    df0['ndeaths'+str(i)] = deaths
    df0 = df0.rename(columns={0: 'date'})

    df = pd.merge(df,df0,on='date',how='left').reset_index()
    df = df.drop(['index'], axis=1)

## Get a file with reports

df0 = df.fillna(0)
xx  = df0[['date']].copy()
xx['deathsl0'] = df0['Tdeaths']

tend = xx.index[-1]

for i in range(0,len(fsel)-1):
    xx['deathsl'+str(i+1)] = xx['deathsl'+str(i)] - df0['ndeaths'+str(i)]

x0 = xx[['date']].copy()
for i in range(0,len(fsel)):
    x0['cumdeathsl'+str(i)] = xx['deathsl'+str(i)].cumsum()
    if i > 0:
        x0['cumdeathsl'+str(i)].loc[tend-i+1:tend] = np.nan

x0['date'] = x0.date.dt.strftime('%Y-%m-%d')        
x0 = x0.set_index('date')

## Do some data cleaning

nn= len(fsel)-1

i = 0
ddate = x0.index[-1-nn+i] #idates[-1-i]
aux = x0.loc[ddate,:] 
aux = aux.to_frame()
aux = aux.reset_index() #.iloc[1:2,'date_report']
aux = aux.sort_index(ascending=False)
aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
aux = aux[ddate] .reset_index()
aux = aux.drop("index", axis=1)
#aux.columns = ['index_'+ddate,ddate]
x00 = aux

# #for i in range(1,len(idates)):

for i in range(1,nn+1):
#for i in range(nn,nn+1):
    #ddate = idates[-1-i]
    ddate = x0.index[-1-nn+i]
    #print(ddate,idates[-1-i]) 
    aux = x0.loc[ddate,:] 
    aux = aux.to_frame()
    aux = aux.reset_index() #.iloc[1:2,'date_report']
    aux = aux.sort_index(ascending=False)
    aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
    aux = aux[ddate] .reset_index()
    aux.columns = ['index_'+ddate,ddate]
    #x00['index_'+ddate] = aux['index_'+ddate]
    x00[ddate] = aux[ddate]    

x000 = x00.transpose()

x000 = x000.reset_index()
x000 = x000.rename(columns={'index': 'date'})

## Prepare to export

en=x0[x0.cumdeathsl0>=3].reset_index()
base000= en[['date']]
add000 = x000[['date',0,13,20]]
add000 = add000.rename(columns={0: '0_y',13: '13_y',20: '20_y'})
export000 = pd.merge(base000,add000,on='date',how='left').reset_index()
export000 = export000.reset_index()
export000 = export000.rename(columns={'index': 't'})
export000 = export000.drop(['level_0'], axis=1)

