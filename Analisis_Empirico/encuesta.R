library(readr)
library(psych)
library(corrplot)
library(REdaS)
library(nFactors)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)
library(readxl)
library(rmarkdown)
library(charlatan)

##########################################################################################
#-----------PROCESADO DE ENCUESTAS DE VALORACIÓN DE UNA EMPRESA EDUCATIVA-----------------
#-----------------------------------SEGUNDA PARTE-----------------------------------------
#-----------------ANÁLISIS EMPÍRICO Y ELABORACIÓN AUTOMÁTICA DE REPORTES------------------
#-----------------Autor: Kamal A. Romero S.--Contacto: karomero@ucm.es--------------------
##########################################################################################

#-----------------------------------------------------------------------------------------
#                             Pre-procesado
#-----------------------------------------------------------------------------------------

#Fijamos el directorio
setwd("C:/Users/Usuario/Documents/KSchool/TFM")

#Cargamos los datos
encuesta <- read_csv("~/KSchool/TFM/encuesta.csv")

#Renombramos las columnas
colnames(encuesta) <- c('Division', 'Curso', 'Grupo', 'Asignatura', 'Profesor', 'Item_1', 'Item_2', 
                        'Item_3', 'Item_4', 'Item_5', 'Item_6', 'Item_7', 'Item_8', 'Item_9', 'Item_10')

#Colocamos a cada división su nombre correspondiente

for(i in 1:nrow(encuesta)){
  if (encuesta$Division[i] == 1){
    encuesta$Division[i] = 'Derecho'
  }
  else if (encuesta$Division[i] == 2){
    encuesta$Division[i] = 'ADE'
  }
  else if (encuesta$Division[i] == 3){
    encuesta$Division[i] = 'Der+ADE'
  }
  else{
    encuesta$Division[i] = 'Psicología'
  }
}

#Examinamos los datos
dim(encuesta)
str(encuesta)
head(encuesta)
tail(encuesta)
summary(encuesta)
describe(encuesta[,6:ncol(encuesta)])

#Armamos una sub-muestra solo con las valoraciones de las encuestas
valoraciones.01 <- encuesta[,6:ncol(encuesta)]

#-----------------------------------------------------------------------------------------
#                                Análisis descriptivo
#-----------------------------------------------------------------------------------------

#Analizamos las correlaciones
correlacion <- cor(valoraciones.01)
correlacion
corrplot(correlacion, type = "upper", order = "original", 
         tl.col = "black")
corrplot(correlacion, type = "upper", order = "hclust", 
         tl.col = "black")

#chart.Correlation(valoraciones.01, histogram=TRUE, pch=19)
#Miramos las correlaciones excluyendo el item 10, que representa la valoración global del
#profesor la cual usaremos como variable dependiente en el ejercicio empírico
valoraciones.02 <- valoraciones.01[,1:ncol(valoraciones.01)-1]
correlacion2 <- cor(valoraciones.02)
corrplot(correlacion2,  order = "hclust", 
         tl.col = "black")

#Boxplot
valoraciones.01.flat <- melt(valoraciones.01)
p <- ggplot(valoraciones.01.flat)
p <- p + aes(x=variable, y = value) + geom_boxplot(fill='steelblue', alpha = 0.7)
p <- p + scale_y_continuous(name = "Valoración del Item", breaks = seq(0, 10, 2)) +
  scale_x_discrete(name= "Items")
p <- p + ggtitle("Diagramas de Cajas de las Valoraciones por Item") + 
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
p

#En los diagramas de caja observamos que la mayoría de los items poseen una media en torno a 8
#Los items 2, 8 y 9 son los que presentan más dispersión, los items 6 y 8 mayor mediana (9) y
#los items 2 y 5 la menor (7)

#Histrogramas
h <- ggplot(valoraciones.01.flat)
h <- h + aes(value)
h <- h + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
h <- h + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
h <- h + labs(x="Valoración", y="Frecuencia")
h

#En los histrogramas destaca el sesgo hacia la derecha que presentan todas las distribuciones de
#frecuencia. Lo cual es de esperar, ya que la evaluaciones están concentradas en las valoraciones
#altas y suele haber poca frecuencia de valoraciones muy bajas.
#En todos los casos la calificación más frecuente es la máxima (10), excepto en el item 5 (8) y
# el 10 que representa la valoración global del profesor (9). El item que mayor valoración presenta
#con diferencia es el 6 (puntualidad)

#Análisis por Divisiones
encuesta.01 <- cbind(encuesta$Division,valoraciones.01)
colnames(encuesta.01)[1] <- 'Division'

encuesta.01.flat <- melt(encuesta.01, id.vars = 'Division')

p1 <- ggplot(encuesta.01.flat)
p1 <- p1 + aes(x=variable, y = value) + geom_boxplot(fill='steelblue', alpha = 0.7)
p1 <- p1 + scale_y_continuous(name = "Valoración del Item", breaks = seq(0, 10, 2)) +
  scale_x_discrete(name= "Items")
p1 <- p1 + facet_wrap(~Division)
p1 <- p1 + ggtitle("Diagramas de Cajas de las Valoraciones por Item y División") + 
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
p1



encuesta.ade <- subset(encuesta.01, Division == 'ADE')
encuesta.ade.flat <- melt(encuesta.ade) 

hade <- ggplot(encuesta.ade.flat)
hade <- hade + aes(value)
hade <- hade + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
hade <- hade + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item. ADE") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
hade <- hade + labs(x="Valoración", y="Frecuencia")
hade

encuesta.psi <- subset(encuesta.01, Division == 'Psicología')
encuesta.psi.flat <- melt(encuesta.psi) 

hpsi <- ggplot(encuesta.psi.flat)
hpsi <- hpsi + aes(value)
hpsi <- hpsi + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
hpsi <- hpsi + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item. Psicología") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
hpsi <- hpsi + labs(x="Valoración", y="Frecuencia")
hpsi

encuesta.der <- subset(encuesta.01, Division == 'Derecho')
encuesta.der.flat <- melt(encuesta.der) 

hder <- ggplot(encuesta.der.flat)
hder <- hder + aes(value)
hder <- hder + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
hder <- hder + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item. Derecho") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
hder <- hder + labs(x="Valoración", y="Frecuencia")
hder

encuesta.derad <- subset(encuesta.01, Division == 'Der+ADE')
encuesta.derad.flat <- melt(encuesta.derad) 

hderad <- ggplot(encuesta.derad.flat)
hderad <- hderad + aes(value)
hderad <- hderad + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
hderad <- hderad + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item. Derecho + ADE") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
hderad <- hderad + labs(x="Valoración", y="Frecuencia")
hderad

#Se calcula la media, mediana y desviación típica por división
encuesta.02 <- group_by(encuesta.01, Division) %>% 
                summarise(Media = mean(Item_10), 
                          Mediana = median(Item_10),
                          Desviación = sd(Item_10),
                          Obsevaciones = n())
encuesta.02

#-----------------------------------------------------------------------------------------
#                                Análisis Empírico
#               Análisis factorial (reducción de dimensión) y Regresión logística
#-----------------------------------------------------------------------------------------


##Contrastes previos a la extracción de factores

#El análisis factorial tiene sentido en la medida que las variables en estudio se encuentren
#altamente correladadas, los tests previos realizan contrastes que permiten contrastar la 
#hipótesis de alta o baja correlación entre las variables

#Test de Barlett
#Asumiendo el caso extremo de ninguna relación entre las variables, la matriz de correlaciones
#sería una matriz identidad. Es decir, los elementos fuera de la diagonal que representan las
#correlaciones entre las variables serían iguales a cero, y la diagonal como es habitual igual
#a uno. En este caso el determinante de la matriz es igual a uno

#Partiendo de esto el test de Barlett toma como hipótesis nula que el determinante de la matriz de
#correlaciones es igual a uno y como alternativa que el mismo es distinto de cero. Bajo la hipótesis
#nula el estadístico de contraste sigue una distribución chi-cuadrado.

BT <- bartlett.test(valoraciones.02)
BT$statistic
BT$p.value

#El valor del estadístico es alto y el p-valor cercano a cero, por lo que se rechaza la 
#hipótesis nula de ausencia de correlación entre las variables con un nivel de significación
#del 1%

#El siguiente contraste es el test KMO (Kaiser, Meyer y Olkin) o de adecuación muestral, el cual
#parte de la idea de que  #si un cojunto de variables se encuentran altamente relacionadas entre
#si, sus coeficientes de correlación serán altos mientras que sus coeficientes de correlación 
#parcial (relación entre pares de variables sin tomar en cuenta el efecto del resto de las
#variables) serán bajos. De ser esto así, las variables comunes (factores) entre las variables
#serán de importancia

#Algebraicamente se contrasta si la siguiente expresión 
# $$
#Como se puede observar, a medida que es menor el valor del coeficiente de correlación parcial
#más se acerca a la unidad la expresión. Valores cercanos a uno del KMO indican un alto grado de 
#asociación de las variables debido a factores comunes.
#En la literatura varios autores señalan distintas _escalas_ de la adecuación de los datos a un
#estudio factorial según el valor del KMO. Enumeramos las propuesta por Kaiser y citada en la 
#documentación del paquete REdaS: maravilloso si KMO ≥ 0.9, mieritorio si 0.9 > KMO ≥ 0.8 <;
#mediano para 0.8 > KMO ≥ 0.7; mediocre para 0.7 > KMO ≥ 0.6; miserable si 0.6 > KMO ≥ 0.5 e
#inaceptable para KMO < 0.5.

KMO.T <- KMOS(valoraciones.02, use= "pairwise.complete.obs")
KMO.T$KMO

#El valor del test KMO es de 0.9337983, por lo que nuestra muestra puede ser calificada de adecuada
#para un análisis factorial

##Analisis Factorial

set.seed(123)

fa.valoraciones1 <- factanal(valoraciones.02, factors = 2, rotation = "none", scores = "regression")
fa.valoraciones1$loadings
fa.valoraciones1$uniquenesses
fa.valoraciones1$STATISTIC
fa.valoraciones1$PVAL
#fa.valoraciones1$scores
#graficos
cargas1 <- fa.valoraciones1$loadings[,1:2]
plot(cargas1,type="n")
text(cargas1,labels=names(valoraciones.02),cex=.7)


fa.valoraciones2 <- factanal(valoraciones.02, factors = 2, rotation = "varimax")
fa.valoraciones2$loadings
fa.valoraciones2$uniquenesses
fa.valoraciones2$STATISTIC
fa.valoraciones2$PVAL
#graficos
cargas2 <- fa.valoraciones2$loadings[,1:2]
plot(cargas2,type="n")
text(cargas2,labels=names(valoraciones.02),cex=.7)

fa.valoraciones3 <- factanal(valoraciones.02, factors = 2, rotation = "promax")
fa.valoraciones3$loadings
fa.valoraciones3$uniquenesses
fa.valoraciones3$STATISTIC
fa.valoraciones3$PVAL
#graficos
cargas3 <- fa.valoraciones3$loadings[,1:2]
plot(cargas3,type="n")
text(cargas3,labels=names(valoraciones.02),cex=.7)

#Numero de factores a incluir
ev <- eigen(cor(valoraciones.02)) # get eigenvalues
ap <- parallel(subject=nrow(valoraciones.02),var=ncol(valoraciones.02), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##Modelo Logit

datos_profesores <- read_excel("~/KSchool/TFM/datos_profesores.xlsx")

datos_profesores$`publicaciones cientificas` <- NULL
colnames(datos_profesores)[6:ncol(datos_profesores)] <- c('Gestion','Acreditacion')

#-----------------------------------------------------------------------------------------
#                        Elaboración Automática de Reportes
#-----------------------------------------------------------------------------------------


#Cargamos el archivo con los items de preguntas de la encuesta
items <- read_excel("~/KSchool/TFM/items.xlsx", col_names = FALSE)

#Elaboramos los reportes en base a la información de las asignaturas,
#profesores y las valoraciones de los items. Así que prescindimos de los
#datos originales las columnas  de División, Grupo y Asignatura
reporte.01 <- encuesta[,-c(1:3)]

#Vector de asignaturas
asignaturas <- unique(encuesta$Asignatura)


#Elaboramos un bucle el cual va creando subconjuntos de los datos originales según
#asignatura y profesor (en el caso que la misma asignatura la imparta más de un 
#profesor), y para cada par asignatura-profesor crea una tabla de frecuencias de las
#respuestas de cada item, dos columnas con la media y la desviación típica, así como
#la pregunta que corresponde a cada item

tablas <- list()
nombres <- list()

for(i in asignaturas){
  reptemp <- subset(reporte.01, Asignatura == i)  #Se filtra por asignatura
  profesores <- unique(reptemp$Profesor)          #Se determina cuantos profersores tiene la asignatura
  for(j in profesores){                           #Se elabora un reporte por (asignatura,profesor)
    nombre <- paste(i,j, sep = '-')               #Se crea una etiqueta (i,j) de la forma 'i-j' para localizar la tabla en la lista
    nombres[[nombre]] <- nombre                   #Se crea una lista de etiquetas, que se empleará posteriormente
    reptemp.01 <- subset(reptemp, Profesor == j)  #Se filtra por profesor
    tab1 <- sapply(reptemp.01, function(x) round(mean(x), digits = 2))    #Se calcula la media de cada item y se almacena en `tab1`
    tab2 <- sapply(reptemp.01, function(x) round(sd(x), digits=2))        #Se calcula la desviación típica de cada item y se almacena en `tab2`
    reptemp.02 <- sapply(reptemp.01[,3:ncol(reptemp.01)],
                         function(x) unname(table(factor(x, levels = 1:10)))    #Se calcula la tabla de frecuencias por item. Se convierte a factor para que
                          )                                                     #aparezcan los items con frecuencia cero y se eliminan las etiquetas con `unname`  
    tab3 <- rbind(reptemp.02, rbind(tab1[3:length(tab1)], tab2[3:length(tab2)]))  #Juntamos la tabla de frecuencias con los vectores de media y desviación
    tabla.freq <- as.data.frame(t(tab3))                                          #Se transpone la tabla
    colnames(tabla.freq) <- c(seq(1:10), 'Media', 'Desviación Típica')            #Se nombran las columnas
    rownames(tabla.freq) <- items$X0                                    #Se añaden las preguntas de cada item
    tablas[[nombre]] <- tabla.freq[,c(11,12,seq(1:10))]                 #Se reordenan las columnas y almacena la tabla en la lista
  }
}


#Para guardar el anonimato de las encuestas, se generean nombres de asignaturas y profesores
#ficticios, empleando el paquete `charlatan`. Se crea un data frame en el cual se almacenan
#los códigos de profesores y asignaturas con sus nombres ficticios correspondientes

#Genera nombres para profesores
profes.cod <- unique(encuesta$Profesor)
nombres.fic <-  ch_generate('name',n=length(unique(encuesta$Profesor)))
profes.01 <- data.frame(profes.cod, nombres.fic)
rownames(profes.01) <- profes.01$profes
profes.01[,1] <- NULL

#Genera nombres para asignaturas
asignaturas.cod <- unique(encuesta$Asignatura)
asignaturas.fic <- ch_generate('job',n=length(unique(encuesta$Asignatura)))
cursos.01 <- data.frame(asignaturas.cod, asignaturas.fic)
rownames(cursos.01) <- cursos.01$asignaturas.cod
cursos.01[,1] <- NULL


#Generación de reportes con Markdown

#Para cada elemento de la tabla `tablas` el cual representa un cuadro por (asignatura,profesor)
#con las preguntas de cada item, su frecuencia, la media y la desviación típica; se elabora un
#reporte en pdf donde aparece la asignatura, el profesor y el cuadro anterior que resume el 
#resultado de la encuesta.

for (i in 1:length(tablas)){
  cuadro <- tablas[[i]]                   #Se accede a la tabla. Esta es la tabla que aparece en el informe
  id <- nombres[[i]]                      #Se accede a la etiqueta generada en el bucle anterior
  id2 <- strsplit(id[[1]], split = '-')   #Se separa el código de asignatura y profesor
  id.01 <- id2[[1]][1]                    #Código de asignatura
  id.02 <- id2[[1]][2]                    #Código de profesor
  prof <- profes.01[row.names(profes.01)==id.02,]     #Se asocia el código de profesor a su nombre ficticio
  curs <- cursos.01[row.names(cursos.01)==id.01,]     #Se asocia el código de asignatura a su nombre ficticio
  n <- c(curs,prof)
  s <- c('Asignatura', 'Profesor')
  df <- data.frame(s,n)                       #Se crea un data frame con los nombres de asignatura y profesor
  colnames(df) <- c()                         #Se eliminan las etiquetas de las columnas
  cabecera <- t(df)                           #Se traspone el data frame. Este es la cabecera del informe
  render("Prueba_reporte.rmd",output_file = paste0('report.', id, '.pdf'))    
}


