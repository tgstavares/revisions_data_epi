import pandas as pd

## Datasets from: https://www.gob.mx/salud/documentos/datos-abiertos-152127

isel   = ['607',
          '606','605','604','603',
          '602','601','531','530',
          '529','528','527','526',
          '525','524','523','522',
          '521','520','519','518',
          '517','516','515','514',
          '513','512','511','510','509',
          '508','507','506','505','504',
          '503','502','501','430','429',
          '428','427','426','425','424',
          '423','422','421','420','419',
          '418','417','416','415','414',
          '413','412']
idates = ['2020-06-07',
          '2020-06-06','2020-06-05','2020-06-04','2020-06-03',
          '2020-06-02','2020-06-01','2020-05-31','2020-05-30',
          '2020-05-29','2020-05-28','2020-05-27','2020-05-26',
          '2020-05-25','2020-05-24','2020-05-23','2020-05-22',
          '2020-05-21','2020-05-20','2020-05-19','2020-05-18',
          '2020-05-17','2020-05-16','2020-05-15','2020-05-14',
          '2020-05-13','2020-05-12','2020-05-11','2020-05-10','2020-05-09',
          '2020-05-08','2020-05-07','2020-05-06','2020-05-05','2020-05-04',
          '2020-05-03','2020-05-02','2020-05-01','2020-04-30','2020-04-29',
          '2020-04-28','2020-04-27','2020-04-26','2020-04-25','2020-04-24',
          '2020-04-23','2020-04-22','2020-04-21','2020-04-20','2020-04-19',
          '2020-04-18','2020-04-17','2020-04-16','2020-04-15','2020-04-14',
          '2020-04-13','2020-04-12']

j = 0
i = isel[0]
directory = '../Data_mx/'
filename = directory+'/'+'200'+str(i)+'COVID19MEXICO.csv'
mx = pd.read_csv(filename,encoding='latin1',dtype={'PAIS_NACIONALIDAD': 'str','PAIS_ORIGEN': 'str',})
mxall= mx
mx = mx[['ID_REGISTRO','RESULTADO','FECHA_INGRESO','FECHA_DEF']]
mx.columns = ['ID_REGISTRO','RESULTADO'+'l'+str(j),'FECHA_INGRESO'+'l'+str(j),'FECHA_DEF'+'l'+str(j)]

j = 1
for i in isel[1:]:
    filename = directory+'/'+'200'+str(i)+'COVID19MEXICO.csv'
    #print(filename,i)
    aux = pd.read_csv(filename,encoding='latin1',dtype={'PAIS_NACIONALIDAD': 'str','PAIS_ORIGEN': 'str',})
    aux = aux[['ID_REGISTRO','RESULTADO','FECHA_INGRESO','FECHA_DEF']]
    aux.columns = ['ID_REGISTRO','RESULTADO'+'l'+str(j),'FECHA_INGRESO'+'l'+str(j),'FECHA_DEF'+'l'+str(j)]
    mx = pd.merge(mx,aux,on='ID_REGISTRO',how='left')
    j = j + 1
    
