# -*- coding: utf-8 -*-
"""
Created on May - Jun 2017
Limpieza de Datos de la Encuesta
@author: Kamal Romero
"""

#Importamos las librerías necesarias
import pandas as pd
import numpy as np
import time

t1=time.time()

#Cargamos los datos
encu_1 = pd.read_csv('Data/encuesta.txt', sep=';', skiprows=1, header=None)

#PRE-PROCESADO
#Esta operación no forma parte del proceso automático de limpieza
#Para detalles, ver el notebook
encu_1.iloc[3508:,2] = 'A'
encu_1.iloc[3640:,1] = 5
encu_1.iloc[2721:2777,2] = 'D'
encu_10 = encu_1.drop(encu_1.index[3395:3508])
encu_11 = encu_10.drop(encu_1.index[3191:3265])
dif_fil = encu_1.shape[0] - encu_11.shape[0]
print('Se han eliminado %d filas' %dif_fil)

##############################################################################
#FUNCIONES
##############################################################################
#Se definen las funciones que se van a emplear en el proceso de limpieza

def llenar(x):
    '''
    INPUT: fila de un data frame
      
    OUTPUT: fila con los cambios descritos abajo
    
    La siguiente función realiza lo siguiente:
        
    - Verfifica si la fila tiene NaN's

    - En caso afirmativo localiza los indices donde se encuentran los NaN's

    - Si tiene un solo NaN (if len (ii) < 2) hace un loop que recorre la fila
      a partir de dicho indice hasta la penultima posición, y sustituye cada
      celda por el valor de la siguiente
  
    - En caso que tenga más de un NaN (else) hace lo mismo, pero sustituyendo
      la celda por el valor de la celda + (número de nan's)
    '''
    if any(l == True for l in x[:-2].isnull()):
        ii = x[:-2].isnull()
        ii = np.where(ii == 1)[0].tolist()
        if len (ii) < 2:
            for indice in range(ii[0],(len(x)-1)):
                x.iloc[indice] = x.iloc[indice+1]
        else:
            for indice in range(ii[0],(len(x)-len(ii))):
                x.iloc[indice] = x.iloc[indice+len(ii)]
    return x

def reemplazo_mediana(x):
    '''
    INPUT: fila de un data frame
    OUTPUT: fila con las modificaciones descritas abajo
    Calcula la mediana de cada fila y porteriormente sustituye los nan's por
    dicho valor
    '''
    mediana = x.median()
    return x.fillna(mediana)
    
    
def sustitucion(datos, nulos, columna, direccion):
    '''
    INPUTS:
    - datos: Data frame en el cual se deben realizar las sustituciones

    - nulos: Una lista que contiene los índices de las filas en las que se
    encuentran los datos nulos

    - columna: En que columna se quieren realizar las sustituciones

    - direccion: Indica si se debe usar la columna de la izquierda o derecha
    en el caso de que el resgistro anterior y posterior no coincidan.
    El valor debe ser **1** en el caso de la colummna derecha y **-1** en el
    caso de la izquierda
    
    OUTPUT: columna del data frame modificada
    
    La función sustituye valores en blanco NO CONSECUTIVOS de una columna de un
    data frame con valores localizados en el entorno de la misma columna.
    Especificamente, el valor anterior al registro en blanco o el posterior.
    Se emplean dos criterios:
    
    - El primer criterio se basa en el valor del registro anterior y
    posterior, si estos son iguales se emplea dicho valor para rellenar
    el valor en blanco.S
    
    - Si el registro posterior y anterior no coinciden se emplea como
    referencia una de las columnas contiguas, la cual vendrá dada por el 
    argumento "direccion". Si el valor de la columna escogida coincide con el
    registro anterior, se sustituye por el valor anterior al nulo. En caso
    contrario se emplea el valor anterior posterior.
    
    - Si dicho registro es NAN, no se realiza ninguna sustitución
    '''
    datos2 = datos.copy()
    nombre ='columna_'+str(columna)+'.txt'
    file = open(nombre,'w')
    for i in nulos[columna]:
        if  i == datos2.shape[0]-1:
            file.write('Sustituyendo fila %d por el valor anterior (ultimo dato) \n' %i)
            datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
        elif datos2.iloc[i-1,columna] == datos2.iloc[i+1,columna]:
            file.write('Sustituyendo fila %d por el valor anterior condicion 1 \n' %i)
            datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
        else:
            if datos2.iloc[i-1,(columna + direccion)] == datos2.iloc[i,(columna + direccion)]:
                file.write('Sustituyendo fila %d por el valor anterior, condicion 2 \n' %i)
                datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
            else:
                if pd.isnull(datos2.iloc[i,(columna + direccion)]) == True:
                    file.write('Sustituyendo fila %d por Nan \n' %i)
                    datos2.iloc[i,columna] = pd.np.nan
                else:
                    file.write('Sustituyendo fila %d por el valor posterior, condicion 3 \n' %i)
                    datos2.iloc[i,columna] = datos.iloc[i+1,columna]
    file.close()
    return datos2
    
def sustitucion2(datos, nulos, nulos2, columna, direccion):
    '''
    INPUTS:
    - datos: data frame en el cual se deben realizar las sustituciones
    
    - nulos: Una lista que contiene los índices de las filas en las que se
    encuentran los datos nulos a partir del cual comienza la secuencia de
    nulos negativos
    
    - nulos2: Una lista que contiene los índices de las filas en las que se
    encuentran los datos nulos en el cual acaba la secuencia de nulos negativos
    
    - columna: En que columna se quieren realizar las sustituciones
    
    - direccion: Indica si se debe usar la columna de la izquierda o derecha
    en el caso de que el resgistro anterior y posterior no coincidan. El valor
    debe ser 1 en el caso de la colummna derecha y -1 en el caso de la
    izquierda
    
    La función sustituye valores en blanco CONSECUTIVOS de una columna de un
    data frame con valores localizados en el entorno de la misma columna.
    Especificamente, el valor anterior al registro en blanco o el posterior.
    
    Criterios:
    - Si el registro anterior y posterior es el mismo se sustituye por el
    registro anterior
    - Si no se cumple lo anterior, se hace un bucle que recorra ese tramo de la
    columna y realice lo siguiente:
        * Si el código de la columna contigua en esa celda es igual al del
        último registro no NAN, se sustituye por el registro anterior
        * Si el código de la columna contigua en esa celda es igual al del
        próximo registro no NAN, se sustituye por el registro posterior
        * Si el código de la columna contigua en esa celda es NAN, se mantiene
        el NAN
    '''
    datos2 = datos.copy()
    for j,i in enumerate(nulos[columna]):
        if datos2.iloc[i-1,columna] == datos2.iloc[nulos2[columna][j]+1,columna]:
            datos2.iloc[i:(nulos2[columna][j]+1),columna] = datos2.iloc[i-1,columna] 
        else:
            for l in range(i,(nulos2[columna][j]+1)):
                if datos2.iloc[l,(columna + direccion)] == datos2.iloc[i-1,(columna + direccion)]:
                    datos2.iloc[l,columna] = datos2.iloc[i-1,columna]
                elif pd.isnull(datos2.iloc[l,(columna + direccion)]) == True:
                    datos2.iloc[l,columna] = pd.np.nan
                elif datos2.iloc[l,(columna + direccion)] == datos2.iloc[nulos2[columna][j]+1,(columna + direccion)]:
                    datos2.iloc[l,columna] = datos2.iloc[nulos2[columna][j]+1,columna]
    return datos2

def localiza(datos, columna):
    '''
    INPUTS:
    - datos: data frame en el cual se desean localizar los valores que cambian
    
    - columna: En que columna se desea realizar la búsqueda
    
    La función localiza los índices en los cuales hay un cambio entre registros
    y se queda con los valores no consecutivos
    '''
    indice_post = [indice for indice in np.arange(1,datos.shape[0])  if \
                   datos.iloc[indice,columna] != datos.iloc[indice-1,columna] ]
    indice_post_uniq = [ x for i,x in enumerate(indice_post[0:len(indice_post)-1]) if \
                         indice_post[i] == indice_post[i+1]-1 ] 
    return indice_post_uniq
    
    
def sustitucion3(datos, nulos, columna, direccion):
    '''
    INPUTS:
    - datos: data frame en el cual se deben realizar las sustituciones
    
    - nulos: Una lista que contiene los índices de las filas en las que se
    encuentran los datos producto de errores humanos que se desean sustituir
    
    - columna: En que columna se quieren realizar las sustituciones
    
    - direccion: Indica si se debe usar la columna de la izquierda o derecha
    en el caso de que el resgistro anterior y posterior no coincidan. El valor
    debe ser 1 en el caso de la colummna derecha y -1 en el caso de la
    izquierda
    
    La función sustituye valores producto de errores humanos de una columna de
    un data frame con valores localizados en el entorno de la misma columna.
    Especificamente, el valor anterior al registro erroneo o el posterior.
    
    Criterios:
    - Si el registro anterior y posterior es el mismo se sustituye por el
    registro anterior
    
    - Si el registro posterior y anterior no coinciden se emplea como
    referencia una de las columnas contiguas, la cual vendrá dada por el 
    argumento "direccion". Si el valor de la columna escogida coincide con el
    registro anterior, se sustituye por el valor anterior al errone. En caso
    copntrario se emplea el valor anterior posterior.
    
    - Existen casos en que la columna escogida en el argumento dirección no es 
    suficiente, por lo que se emplea la columna opuesta empleando el mismo
    criterio que en el paso anterior
    
    - Si el valor de la columna dirección es NAN, se reemplaza por dicho valor
    '''
    datos2 = datos.copy()
    otracol = -1*columna
    for i in nulos:
        if datos2.iloc[i-1,columna] == datos2.iloc[i+1,columna]:
            datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
        else:
            if datos2.iloc[i-1,(columna + direccion)] == datos2.iloc[i,  (columna + direccion)]:
                datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
            elif datos2.iloc[i+1,(columna + direccion)] == datos2.iloc[i,(columna + direccion)]:
                datos2.iloc[i,columna] = datos2.iloc[i+1,columna]
            elif  pd.isnull(datos2.iloc[i,(columna + direccion)]) == False and \
            datos2.iloc[i-1,(columna + otracol)] == datos2.iloc[i,  (columna + otracol)]:
                datos2.iloc[i,columna] = datos2.iloc[i-1,columna]
            elif  pd.isnull(datos2.iloc[i,(columna + direccion)]) == False and \
            datos2.iloc[i+1,(columna + otracol)] == datos2.iloc[i,  (columna + otracol)]:
                datos2.iloc[i,columna] = datos2.iloc[i+1,columna]
            else:
                datos2.iloc[i,columna] = pd.np.nan
    return datos2
    
##############################################################################
    

encu_2 = encu_11.iloc[:,5:].replace(r'\s+', np.nan, regex=True).dropna(axis=1, how='all')
i_nulos_p = encu_2.iloc[:,-2].isnull()
i_nonulos_p = np.where(i_nulos_p == 0)[0].tolist()
i_nulos_u = encu_2.iloc[:,-1].isnull()

i_nonulos_u = np.where(i_nulos_u == 0)[0].tolist()
i_nonulos_temp = set(i_nonulos_p) | set(i_nonulos_u)
i_nonulos = sorted(list(i_nonulos_temp))


prueba = encu_2.iloc[i_nonulos,:]
prueba.apply(llenar, axis =1)
encu_2.iloc[i_nonulos_p,:] = prueba
encu_2.drop(encu_2.columns[-2:],inplace=True,axis=1)

encu_3 = encu_2.replace(['??'], np.nan)

encu_3.apply(reemplazo_mediana, axis = 1)

#Bloque de Códigos (División, grupo, curso, asignatura y profesor)

#Se define el data frame
encu_codigos = encu_11.iloc[:,:5]

#Se realiza la limpieza básica, donde se sustituyen espacios, signos de
#interrogación y ceros localizados al principio de los códigos
#Ver el notebook para más detalles
encu_codigos2 = encu_codigos.replace(r'\s$', np.nan, regex=True)
encu_codigos21 = encu_codigos2.replace(r'\?', np.nan, regex=True)
encu_codigos211 = encu_codigos21.replace(r'^0+', ' ', regex=True)
encu_codigos222 = encu_codigos211.replace(r'^\s+', ' ', regex=True)
encu_codigos22 = encu_codigos222.replace(' ', np.nan)


#Localización de columnas con valores NAN's

#En el siguiente bucle se localizan lo valores nulos consecutivos y no
#consecutivos y se almacenan en una lista. Esta lista se emplea posteriormente
#para indicar a las funciones "sustitucion" y "sustitucion2" los índices del 
#data frame sobre el que deben realizarse las sustituciones
nulos_columnas = []
nulos_consec_columnas = []
for columna in range(len(encu_codigos22.columns)):
    cod_nul_asig = encu_codigos22.iloc[:, columna].isnull()
    ind_nul_asig = np.where(cod_nul_asig == 1)[0].tolist()
    lista_consec_asig = []
    for i in range((len(ind_nul_asig)-1)):
        if ind_nul_asig[i] == ind_nul_asig[i+1]-1:
            lista_consec_asig.append(ind_nul_asig[i])
            lista_consec_asig.append(ind_nul_asig[i+1])
            unique_asig = np.unique(lista_consec_asig)
            unique_asig = unique_asig.tolist()
    lista_noconsec_asig = [x for x in ind_nul_asig if x not in unique_asig]
    nulos_columnas.append(lista_noconsec_asig)
    nulos_consec_columnas.append(unique_asig)

#Numeros de nulos en cada caso
for columna in range(len(encu_codigos22.columns)):
    num_nulos = len(nulos_columnas[columna])
    print('En la columna %d hay %d registros nulos no consecutvos' % (columna, num_nulos))
    
for columna in range(len(encu_codigos22.columns)):
    num_nulos = len(nulos_consec_columnas[columna])
    print('En la columna %d hay %d registros nulos consecutvos' % (columna, num_nulos))
    
null_columnas = []
for columna in range(len(encu_codigos22.columns)):
    cod_nul_asig = encu_codigos22.iloc[:, columna].isnull()
    ind_nul_asig = np.where(cod_nul_asig == 1)[0].tolist()
    null_columnas.append(ind_nul_asig)
for columna in range(len(encu_codigos22.columns)):
    num_nulos = len(null_columnas[columna])
    print('En la columna %d hay %d registros nulos' % (columna, num_nulos))
    
#Inicio y fin de de las secuencias de nulos consecutivos

#En la función "sustitucion2" necesitamos el inicio y fin de la secuncia de
#nulos consecutivos.
#La lista de los índices de resgistros de NAN's consecutivos no nos sirve para
#iterar sobre el mismo si queremos realizar las sustituciones, así que tenemos
#que crear primero un lista sobre la cual iterar que corresponde al índice del
#primero de los registros consecutivos que se repite.
#Por ejemplo si los registros son [12,13,14,25,26,38,39,40] nos gustaría tener
#una lista del tipo [12,25,38]
#Realizamos esto en el siguiente list comprehension
    
consec_col = []
for columna in range(len(nulos_consec_columnas)):
    consec_temp = [ j for i, j in enumerate(nulos_consec_columnas[columna]) \
                   if j!= nulos_consec_columnas[columna][i-1]+1 \
                    or j == nulos_consec_columnas[columna][0]]
    consec_col.append(consec_temp)

#Asimismo necesitamos el índice donde acaba la secuencia de dichos registros.
#En el ejemplo anterior sería pasar de [12,13,14,25,26,38,39,40] a [14,26,40]
#De manera que la función rellene los registros desde 12 a 14 por ejemplo.
#Obtenemos dicha lista siguiendo un procedimiento similar al paso anterior.
#Como es posible observar, se realiza un paso adicional donde se incluye el
#valor terminal, ya que si se intenta hacer desde el list comprehension el
#índice sale fuera del rango
    
consec2_col = []
for columna in range(len(nulos_consec_columnas)):
    consec2_temp = [ j for i, j \
                    in enumerate(nulos_consec_columnas[columna][0:(len(nulos_consec_columnas[columna])-1)])\
                     if j!= nulos_consec_columnas[columna][i+1]-1 ]
    consec2_temp.append(nulos_consec_columnas[columna][(len(nulos_consec_columnas[columna])-1)])
    consec2_col.append(consec2_temp)

   
#Aplicamos las funciones "sustitucion", "sustitucion2", "localiza" y 
#"sustitucion3"  a las columnas correspondientes

#El orden es aplicar primero los funciones sustitucion y sustitucion 2, para de
#ese modo reemplazar los NAN's. Posteriormente se usa la función localiza para
#identificar los índices en los cuales cambian los resgistros y se señalan como
#errores humanos. Finalmente se aplica la función sustitucion 3 para reemplazar
#los valores identificados en el paso anterior

#En el caso de las columnas de Asignaturas y Profesores se altera el orden 
#debido al ser las primeras que se emplea al iniciar el proceso

#Columnas Asignaturas y Profesores
encu_codigos23 = sustitucion(encu_codigos22, nulos_columnas, 3, 1)
encu_codigos24 = sustitucion2(datos=encu_codigos23, columna=3, nulos=consec_col, nulos2=consec2_col, direccion=1)
indice_post3 = localiza(datos= encu_codigos24, columna= 3)
encu_codigos25 = sustitucion(datos=encu_codigos24, columna=4, nulos=nulos_columnas, direccion=-1)
encu_codigos26 = sustitucion2(datos=encu_codigos25, columna=4, nulos=consec_col, nulos2=consec2_col, direccion=-1)
indice_post4 = localiza(datos= encu_codigos26, columna= 4)
encu_codigos262 = sustitucion3(datos=encu_codigos26,columna=3,direccion=1,nulos=indice_post3)
encu_codigos263 = sustitucion3(datos=encu_codigos262, columna=4, direccion=-1,nulos=indice_post4)

#Columna Grupo
encu_codigos27 = sustitucion(datos=encu_codigos263, columna=2, nulos=nulos_columnas, direccion=1)
encu_codigos271 = sustitucion2(datos=encu_codigos27, columna=2, nulos=consec_col, nulos2=consec2_col, direccion=1)
indice_post2 = localiza(datos=encu_codigos271,columna=2)
encu_codigos272 = sustitucion3(datos=encu_codigos271,columna=2,direccion=1, nulos=indice_post2)

#Columna Curso
encu_codigos28 = sustitucion(datos=encu_codigos272, columna=1, nulos=nulos_columnas, direccion=-1)
encu_codigos29 = sustitucion2(datos=encu_codigos28, columna=1, nulos=consec_col, nulos2=consec2_col, direccion=-1)
indice_post1 = localiza(datos=encu_codigos29, columna=1)
encu_codigos291 = sustitucion3(datos=encu_codigos29, columna=1,nulos=indice_post1,direccion=-1)

#Columna División
encu_codigos30 = sustitucion(datos=encu_codigos291, columna=0, nulos=nulos_columnas, direccion=1)
encu_codigos31 = sustitucion2(datos=encu_codigos30, columna=0, nulos=consec_col, nulos2=consec2_col, direccion=1)
indice_post0 = localiza(datos=encu_codigos31,columna=0)
encu_codigos32 = sustitucion3(datos=encu_codigos31,columna=0,nulos=indice_post0,direccion=1)

t2 = time.time()

print("Tiempo de ejecución ", t2-t1)