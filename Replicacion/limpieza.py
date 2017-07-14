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
    
def localiza2(datos, columna):
    '''
    INPUTS:
    - datos: data frame en el cual se desean localizar los valores que cambian
    
    - columna: En que columna se desea realizar la búsqueda
    
    El output  son listas de tuplas, con el primer elemento de la tupla
    indicando el índice donde comienza la secuencia y el segundo elemento el
    índice en el cual acaba
    
    La siguiente función es igual a ``localiza`` salvo que en lugar de
    quedarnos con los valores únicos, nos quedamos con valores que se
    repiten hasta 3 veces.

    '''
    indice_bound1 = []
    indice_bound2 = []
    indice_post = [indice for indice in np.arange(1,datos.shape[0])  if \
                   datos.iloc[indice,columna] != datos.iloc[indice-1,columna] ]
    indice_post2 = [ (x,i) for i,x in enumerate(indice_post[0:len(indice_post)-3]) if \
                         indice_post[i] == indice_post[i+1]-2 or \
                         indice_post[i] == indice_post[i+1]-3 ]
    for i,x in enumerate(indice_post2[0:len(indice_post2)]):
        indice1 = indice_post2[i][0]
        indice_temp = indice_post2[i][1] + 1
        indice2 = indice_post[indice_temp]
        indice_bound1.append(indice1)
        indice_bound2.append(indice2)
    return indice_bound1, indice_bound2
    
def sustitucion4(datos, nulos, columna, direccion):
    '''
    INPUTS:
    - datos: data frame en el cual se deben realizar las sustituciones
    
    - nulos: Una lista que contiene los índices de las filas en las que se
    encuentran los datos nulos a partir del cual comienza la secuencia de
    nulos negativos
    
    - columna: En que columna se quieren realizar las sustituciones
    
    - direccion: Indica si se debe usar la columna de la izquierda o derecha
    en el caso de que el resgistro anterior y posterior no coincidan. El valor
    debe ser 1 en el caso de la colummna derecha y -1 en el caso de la
    izquierda
    
    Esta función esta hecha NO para sustituir registros en blanco, sino
    registros producto de errores humanos.
    
    Criterios:
    - Para los rangos definido por localiza2, si el registro de la columna de
    referencia es igual al de la misma columna antes del inicio del rango, se
    sustituye el registro por el valor anterior.
    - En caso que el registro de la columna de referencia es igual al de la
    misma columna al final del rango, se sustituye por por el valor posterior
    '''
    datos2 = datos.copy()
    for i,x in enumerate(nulos[0]):
        for l in range(x, nulos[1][i]):
            if datos2.iloc[l, columna + direccion] == datos2.iloc[x-1, columna + direccion]:
                datos2.iloc[l, columna] = datos2.iloc[x-1,columna]
            elif datos2.iloc[l, columna + direccion] == datos2.iloc[nulos[1][i], columna + direccion]:
                datos2.iloc[l, columna] = datos2.iloc[nulos[1][i], columna]
    return datos2
    
##############################################################################
    
#El proceso de limpieza de datos se divide en dos fases: 
#- La primera, en la cual se limpian los datos correspondientes a los items de
#evaluación. Primero se corrige un desplazamiento de columnas realizado por la
#lectora óptica, posteriormente se eliminan caracteres generados por la lectora
#y finalmente se sustituyen las valoraciones NAN por la mediana del resto.

#-El segundo paso es el más complejo, y requiere la limpieza de un bloque de 
#códigos administrativos (división, curso, asignatura y profesor). 
#El reto de esta sección es que no se pueden emplear tácticas comunes de
#eliminación y sustitución por imputación de valores.
#Dado lo complejo de este bloque, se refiere al usuario revisar el notebook
#para los detalles de la estrategia de limpieza

#################################################################
#Bloque de Items de Evaluación
#################################################################

#Se Cogen los items de respuesta (columnas de la 5 hasta el final) y con una
#expresión regular se sustituyen los espacios en blanco por NaN y luego se 
#eliminan las columnas donde todos los valores son NaN's
encu_2 = encu_11.iloc[:,5:].replace(r'\s+', np.nan, regex=True).dropna(axis=1, how='all')

#A continuación se deben realizar unas manipulaciones en las cuales se deben
#desplazar registros de las últimas columnas. Para detalles ver el notebook

#Se localizan los NaN's de la penúltima columna. Lo anterior nos da un vector
#booleano de unos y ceros, los unos (TRUE) nos dan los elementos nulos.
#En la próxima celda localizamos los valores no nulos buscando (con where) 
#aquellas celdas iguales a cero
i_nulos_p = encu_2.iloc[:,-2].isnull()
i_nonulos_p = np.where(i_nulos_p == 0)[0].tolist()
i_nulos_u = encu_2.iloc[:,-1].isnull()

#Repetimos lo anterior con la última columna
i_nonulos_u = np.where(i_nulos_u == 0)[0].tolist()
i_nonulos_temp = set(i_nonulos_p) | set(i_nonulos_u)
i_nonulos = sorted(list(i_nonulos_temp))

#Se construye una lista que contenga los no nulos de la última y penúltima 
#fila, usando la unión
prueba = encu_2.iloc[i_nonulos,:]

#Se aplica la función llenar
prueba.apply(llenar, axis =1)

#Sustituimos en el data frame original los valores modificados en el proceso
#anterior
encu_2.iloc[i_nonulos_p,:] = prueba

#Eliminamos las dos últimas columnas
encu_2.drop(encu_2.columns[-2:],inplace=True,axis=1)

#Ahora procedemos a la siguiente operación, imputar los items vacios por el
#valor mediano del resto de las valoraciones (fila)

#Primero sustituimos los '??' por NAN
encu_3 = encu_2.replace(['??'], np.nan)

#Se aplica la función reemplazo mediana a las filas del data frame
encu_4 = encu_3.apply(reemplazo_mediana, axis = 1)

#################################################################
#Bloque de Códigos (División, grupo, curso, asignatura y profesor)
#################################################################

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

#Se realiza la limpieza final de errores humanos empleando las funciones 
#loaliza2 y sustitucion4
indice_f3 = localiza2(datos= encu_codigos32, columna=3)
encu_codigos33 = sustitucion4(datos=encu_codigos32, columna=3, nulos=indice_f3, direccion=1)
indice_f2 = localiza2(datos= encu_codigos32, columna=2)
encu_codigos34 = sustitucion4(datos=encu_codigos33, columna=2, nulos=indice_f2, direccion=1)

#♦Se verifica que ambos bloques tengan el mismo número de filas
encu_codigos34.shape
encu_4.shape

#Se convierte a numérico el bloque de valoraciones
encu_5 = encu_4.apply(pd.to_numeric, axis = 1)

#Se agrupan ambos bloques
encu_6 = encu_codigos34.join(encu_5)

#Se eliminan las filas con NAN's
encu_def = encu_6.dropna()

#Verificamos cuantas filas se han perdido en el proceso
dif_fil = encu_6.shape[0] - encu_def.shape[0]
print('Se han eliminado %d filas' %dif_fil)

#Se asignan nombres a las columnas
nombre_columnas = ['División', 'Curso', 'Grupo', 'Asignatura', 'Profesor', 'Item 1', 'Item 2', 'Item 3', 'Item 4', \
                  'Item 5', 'Item 6', 'Item 7', 'Item 8', 'Item 9', 'Item 10']
encu_def.columns = nombre_columnas

encu_def.to_csv('encuesta.csv', index=False)

t2 = time.time()

print("Tiempo de ejecución ", t2-t1)