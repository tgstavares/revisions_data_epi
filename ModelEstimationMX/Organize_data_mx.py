# Libraries
import pandas as pd
from datetime import datetime
import numpy as np

# First set of data
i = 0
x0c = mx[
    (mx['FECHA_INGRESO'+'l'+str(i)] != '9999-99-99') & 
    (mx['RESULTADOl'+str(i)] != 2) &
    (mx['RESULTADOl'+str(i)] != 3)] \
    [['FECHA_INGRESO'+'l'+str(i),'RESULTADOl0']] .groupby('FECHA_INGRESO'+'l'+str(i)) .count() .cumsum()

x0c['RESULTADOl'+str(i)] = mx[
    (mx['FECHA_INGRESO'+'l'+str(i)] != '9999-99-99') & 
    (mx['RESULTADOl'+str(i)] != 2) &
    (mx['RESULTADOl'+str(i)] != 3)] \
    [['FECHA_INGRESO'+'l'+str(i),'RESULTADOl0']] .groupby('FECHA_INGRESO'+'l'+str(i)) .count() .cumsum()

x0c = x0c.reset_index()
x0c = x0c.rename({'FECHA_INGRESO'+'l'+str(i): 'date'}, axis='columns')

if x0c['date'].iloc[-1] < idates[i]:
    jjj = (datetime.strptime(idates[i], "%Y-%m-%d")-datetime.strptime(x0c['date'].iloc[-1], "%Y-%m-%d")).days
    for iii in range(1,jjj+1):
        x0c = x0c.append({'RESULTADOl'+str(i): x0c['RESULTADOl'+str(i)].iloc[-1]},ignore_index=True)
        x0c.loc[x0c.index[-1],'date']=idates[i+jjj-iii]

x0d = mx[
    (mx['FECHA_DEFl'+str(i)] != '9999-99-99') & 
    (mx['RESULTADOl'+str(i)] != 2) &
    (mx['RESULTADOl'+str(i)] != 3)] \
    [['FECHA_DEFl'+str(i),'RESULTADOl0']] .groupby("FECHA_DEFl"+str(i)) .count() .cumsum()

x0d['RESULTADOl'+str(i)] = mx[
    (mx['FECHA_DEFl'+str(i)] != '9999-99-99') & 
    (mx['RESULTADOl'+str(i)] != 2) &
    (mx['RESULTADOl'+str(i)] != 3)] \
    [['FECHA_DEFl'+str(i),'RESULTADOl0']] .groupby("FECHA_DEFl"+str(i)) .count() .cumsum()

x0d = x0d.reset_index()
x0d = x0d.rename({'FECHA_DEFl'+str(i): 'date'}, axis='columns')

if x0d['date'].iloc[-1] < idates[i]:
    jjj = (datetime.strptime(idates[i], "%Y-%m-%d")-datetime.strptime(x0d['date'].iloc[-1], "%Y-%m-%d")).days
    for iii in range(1,jjj+1):
        x0d = x0d.append({'RESULTADOl'+str(i): x0d['RESULTADOl'+str(i)].iloc[-1]},ignore_index=True)
        x0d.loc[x0d.index[-1],'date']=idates[i+jjj-iii]        
        
for i in range(1,len(idates)):    
    z0c = mx[
        (mx['FECHA_INGRESO'+'l'+str(i)] != '9999-99-99') & 
        (mx['RESULTADOl'+str(i)] != 2) &
        (mx['RESULTADOl'+str(i)] != 3)] \
        [['FECHA_INGRESO'+'l'+str(i),'RESULTADOl'+str(i)]] .groupby('FECHA_INGRESO'+'l'+str(i)) .count() .cumsum()

    z0c['RESULTADOl'+str(i)] = mx[
        (mx['FECHA_INGRESO'+'l'+str(i)] != '9999-99-99') & 
        (mx['RESULTADOl'+str(i)] != 2) &
        (mx['RESULTADOl'+str(i)] != 3)] \
        [['FECHA_INGRESO'+'l'+str(i),'RESULTADOl'+str(i)]] .groupby('FECHA_INGRESO'+'l'+str(i)) .count() .cumsum()

    z0c = z0c.reset_index()
    z0c = z0c.rename({'FECHA_INGRESO'+'l'+str(i): 'date'}, axis='columns')
    
    if z0c['date'].iloc[-1] < idates[i]:        
        jjj = (datetime.strptime(idates[i], "%Y-%m-%d")-datetime.strptime(z0c['date'].iloc[-1], "%Y-%m-%d")).days
        for iii in range(1,jjj+1):
            z0c = z0c.append({'RESULTADOl'+str(i): z0c['RESULTADOl'+str(i)].iloc[-1]},ignore_index=True)
            z0c.loc[z0c.index[-1],'date']=idates[i+jjj-iii]
        
    x0c = pd.merge(x0c,z0c,on='date',how='left')
    del z0c
    
    z0d = mx[
        (mx['FECHA_DEFl'+str(i)] != '9999-99-99') & 
        (mx['RESULTADOl'+str(i)] != 2) &
        (mx['RESULTADOl'+str(i)] != 3)] \
        [['FECHA_DEFl'+str(i),'RESULTADOl'+str(i)]] .groupby("FECHA_DEFl"+str(i)) .count() .cumsum()

    z0d['RESULTADOl'+str(i)] = mx[
        (mx['FECHA_DEFl'+str(i)] != '9999-99-99') & 
        (mx['RESULTADOl'+str(i)] != 2) &
        (mx['RESULTADOl'+str(i)] != 3)] \
        [['FECHA_DEFl'+str(i),'RESULTADOl'+str(i)]] .groupby("FECHA_DEFl"+str(i)) .count() .cumsum()

    z0d = z0d.reset_index()
    z0d = z0d.rename({'FECHA_DEFl'+str(i): 'date'}, axis='columns')
    
    if z0d['date'].iloc[-1] < idates[i]:        
        jjj = (datetime.strptime(idates[i], "%Y-%m-%d")-datetime.strptime(z0d['date'].iloc[-1], "%Y-%m-%d")).days
        for iii in range(1,jjj+1):
            z0d = z0d.append({'RESULTADOl'+str(i): z0d['RESULTADOl'+str(i)].iloc[-1]},ignore_index=True)
            z0d.loc[z0d.index[-1],'date']=idates[i+jjj-iii]
        
    x0d = pd.merge(x0d,z0d,on='date',how='left')
    del z0d

x0c = x0c.set_index('date')
x0d = x0d.set_index('date')

# Second set of data
i = 0
ddate = idates[-1-i]
aux = x0d.loc[ddate,:] 
aux = aux.to_frame()
aux = aux.reset_index() #.iloc[1:2,'date_report']
aux = aux.sort_index(ascending=False)
aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
aux = aux[ddate] .reset_index()
aux = aux.drop("index", axis=1)
#aux.columns = ['index_'+ddate,ddate]
x00d = aux

for i in range(1,len(idates)):
    ddate = idates[-1-i]
    aux = x0d.loc[ddate,:] 
    aux = aux.to_frame()
    aux = aux.reset_index() #.iloc[1:2,'date_report']
    aux = aux.sort_index(ascending=False)
    aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
    aux = aux[ddate] .reset_index()
    aux.columns = ['index_'+ddate,ddate]
    #x00['index_'+ddate] = aux['index_'+ddate]
    x00d[ddate] = aux[ddate]    

x000d = x00d.transpose()

nn= (datetime.strptime(idates[0], "%Y-%m-%d")-datetime.strptime(idates[-1], "%Y-%m-%d")).days
i = 0
ddate = x0c.index[-1-nn+i] #idates[-1-i]
aux = x0c.loc[ddate,:] 
aux = aux.to_frame()
aux = aux.reset_index() #.iloc[1:2,'date_report']
aux = aux.sort_index(ascending=False)
aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
aux = aux[ddate] .reset_index()
aux = aux.drop("index", axis=1)
#aux.columns = ['index_'+ddate,ddate]
x00c = aux

for i in range(1,nn+1):
    ddate = x0c.index[-1-nn+i]
    aux = x0c.loc[ddate,:] 
    aux = aux.to_frame()
    aux = aux.reset_index() #.iloc[1:2,'date_report']
    aux = aux.sort_index(ascending=False)
    aux = aux.drop(aux[(np.isnan(aux[ddate]))].index) .reset_index()
    aux = aux[ddate] .reset_index()
    aux.columns = ['index_'+ddate,ddate]
    #x00['index_'+ddate] = aux['index_'+ddate]
    x00c[ddate] = aux[ddate]    

x000c = x00c.transpose()

x000c = x000c.reset_index()
x000c = x000c.rename({'index': 'date'}, axis='columns')
x000d = x000d.reset_index()
x000d = x000d.rename({'index': 'date'}, axis='columns')

x000 = pd.merge(x000c,x000d,on='date',how='right')
