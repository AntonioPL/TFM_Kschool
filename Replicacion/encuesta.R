
##########################################################################################
#-----------PROCESADO DE ENCUESTAS DE VALORACIÓN DE UNA EMPRESA EDUCATIVA-----------------
#-----------------------------------SEGUNDA PARTE-----------------------------------------
#-----------------ANÁLISIS EMPÍRICO Y ELABORACIÓN AUTOMÁTICA DE REPORTES------------------
#-----------------Autor: Kamal A. Romero S.--Contacto: karomero@ucm.es--------------------
##########################################################################################

PAQUETES <- c("readr","corrplot","REdaS","nFactors","reshape2","tidyverse","readxl","dplyr","ggplot2",
              "gmodels","lmtest","mfx","ROCR","caTools","rmarkdown","charlatan")
inst <- match(PAQUETES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PAQUETES[need])
#Cargar paquetes
lapply(PAQUETES, require, character.only=T)


#-----------------------------------------------------------------------------------------
#                             Pre-procesado
#-----------------------------------------------------------------------------------------

#Fijamos el directorio
setwd("")

#Cargamos los datos
encuesta <- read_csv("encuesta.csv")

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


#-----------------------------------------------------------------------------------------
#                                Análisis descriptivo
#-----------------------------------------------------------------------------------------

#Examinamos los datos
dim(encuesta)
str(encuesta)
head(encuesta)
tail(encuesta)
summary(encuesta)
describe(encuesta[,6:ncol(encuesta)])

#Armamos una sub-muestra solo con las valoraciones de las encuestas
valoraciones.01 <- encuesta[,6:ncol(encuesta)]


#Analizamos las correlaciones
correlacion <- cor(valoraciones.01)
correlacion
corrplot(correlacion, type = "upper", order = "hclust", 
         tl.col = "black")

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


#Histrogramas
h <- ggplot(valoraciones.01.flat)
h <- h + aes(value)
h <- h + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
h <- h + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
h <- h + labs(x="Valoración", y="Frecuencia")
h


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

#-----------------------------------------------------------------------------------------
#                                 Análisis factorial
#-----------------------------------------------------------------------------------------

##Contrastes previos a la extracción de factores

#Test de Barlett

BT <- bartlett.test(valoraciones.02)
BT$statistic
BT$p.value

#Test KMO (Kaiser, Meyer y Olkin)

KMO.T <- KMOS(valoraciones.02, use= "pairwise.complete.obs")
KMO.T$KMO

##Analisis Factorial

set.seed(123)

fa.valoraciones1 <- factanal(valoraciones.02, factors = 2, rotation = "none", scores = "regression")
fa.valoraciones1$loadings
fa.valoraciones1$uniquenesses
fa.valoraciones1$STATISTIC
fa.valoraciones1$PVAL
factores1 <- fa.valoraciones1$scores
#graficos
cargas1 <- fa.valoraciones1$loadings[,1:2]
plot(cargas1,type="n")
text(cargas1,labels=names(valoraciones.02),cex=.7)

#Rotación Ortogonal
fa.valoraciones2 <- factanal(valoraciones.02, factors = 2, rotation = "varimax", scores = "regression")
fa.valoraciones2$loadings
fa.valoraciones2$uniquenesses
fa.valoraciones2$STATISTIC
fa.valoraciones2$PVAL
factores2 <- fa.valoraciones2$scores
#graficos
cargas2 <- fa.valoraciones2$loadings[,1:2]
plot(cargas2,type="n")
text(cargas2,labels=names(valoraciones.02),cex=.7)

#Rotación Oblicua
fa.valoraciones3 <- factanal(valoraciones.02, factors = 2, rotation = "promax", scores = "regression")
fa.valoraciones3$loadings
fa.valoraciones3$uniquenesses
fa.valoraciones3$STATISTIC
fa.valoraciones3$PVAL
factores3 <- fa.valoraciones3$scores
#graficos
cargas3 <- fa.valoraciones3$loadings[,1:2]
plot(cargas3,type="n")
text(cargas3,labels=names(valoraciones.02),cex=.7)

#Numero de factores a incluir
ev <- eigen(cor(valoraciones.02)) # get eigenvalues
ap <- parallel(subject=nrow(valoraciones.02),var=ncol(valoraciones.02), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#-----------------------------------------------------------------------------------------
#                                 Regresión Logística
#-----------------------------------------------------------------------------------------

#En esta sección se construye regresión logística con el objeto de analizar el efecto de una serie de variables
#sobre la valoración de una asignatura.

#La variable dependiente (respuesta, target) es la valoración de una asignatura. Las variables independientes o
#regresores son:

#Los factores obtenidos en el apartado anterior, y los cuales resumen las características latentes evaluadas
#la encuesta, _docencia_ y _relación alumno-profesor_

#Características individuales de los profesores:
#   + Edad
#   + Sexo
#   + Tipo de asignatura (cuantitativa o no)
#   + Categoría (licenciado, doctor, titular o catedrático)
#   + Tener o no un cargo administrativo dentro de la universidad
#   + Estar en posesión o no de una acreditación de la ANECA

#Los factores y la edad son variables continuas, el tipo de asignatura, cargo administrativo y la acreditación
#son binarias 0-1 y la categoría es una variable categórica con 4 niveles del 1 al 4.

#Las variables se encuentran codificadas del siguiente modo:

#Sexo: 1 si es hombre y 0 si es mujer
#Cuantitativa: 1, si el profesor imparte alguna asignatura de tipo cuantitativo 
#Categoria: 1 licenciado, 2 doctor, 3 titular y 4 catedrático
#Tareas de gestión: 1 si se dedica a ello y 0 si no lo hace 
#Acreditación: toma el valor 1 si el individuo tiene algún tipo de acreditación y 0 en caso contrario
#(los funcionarios están incluidos como acreditados)


#Se cargan los datos
datos_profesores <- read_excel("datos_profesores_v2.xlsx")

dim(datos_profesores)
head(datos_profesores)
tail(datos_profesores)
summary(datos_profesores)

#Eliminos las filas con NA's. En nuestro caso específico es solo la última fila
datos_profesores <- na.omit(datos_profesores)

#Convertimos en data frame
datos_profesores.01 <- as.data.frame(datos_profesores)

#Cambiamos los nombres de las dos últimas columnas
colnames(datos_profesores.01)[6:ncol(datos_profesores.01)] <- c('Gestion','Acreditacion')

#Hacemos que los códigos sean los índices del data frame
rownames(datos_profesores.01) <- datos_profesores.01$codigo
datos_profesores.01[,1] <- NULL

#Añadimos los factores estimados en el apartado anterior 
encuesta.fac <- cbind(encuesta, factores3)

#Eliminamos los items de respuesta, ya que (parte de) dicha información está resumida
#en los factores
encuesta.fac01 <- encuesta.fac[,-c(6:14)]

#También eliminamos la información administrativa distinta del profesor
encuesta.fac01 <- encuesta.fac01[,-c(1:4)]

#En el siguiente bucle añadimos las columnas de características de los docentes al data frame de las
#encuestas

encuesta.logit <- as.data.frame(encuesta.fac01)

encuesta.logit.01 <- list()

for(i in 1:nrow(encuesta.logit)){
  encuesta.logit.01[[i]] <- cbind(encuesta.logit[i,], 
                                  datos_profesores.01[as.character(encuesta.logit[i,'Profesor']),])
}

#"Juntamos" las elementos de las filas como filas y convertimos en un data frame
encuesta.logit.temp <- do.call('rbind',encuesta.logit.01)
encuesta.logit.temp1 <- as.data.frame(encuesta.logit.temp)

#Eliminamos registros de profesores que no tienen información (ver markdown)
encuesta.logit.02 <- na.omit(encuesta.logit.temp1)

dim(encuesta.logit.temp1)
dim(encuesta.logit.02)

#Finalmente, eliminamos la columna de profesores, ya que la hemos empleado solo para añadir las 
#características de los mismos
encuesta.logit.03 <- encuesta.logit.02[,-c(1)]

#Procedemos a convertir la variable dependiente (`Item_10`) en binaria. Se sigue un trabajo anterior hecho
#en el centro y se divide la valoración global de la encuesta en 'Alta' si la valoración es mayor a 6 y 
#'Baja' en caso contrario

encuesta.logit.03$Target <- 1

for(i in 1:nrow(encuesta.logit.03)){
  if (encuesta.logit.03$Item_10[i] > 6){
    encuesta.logit.03$Target[i] = 1
  }
  else{
    encuesta.logit.03$Target[i] = 0
  }
}

#Observamos las proporciones

#Totales
CrossTable(encuesta.logit.03$Target)
#Según categoría
CrossTable(encuesta.logit.03$Target, encuesta.logit.03$categoria,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Según sexo
CrossTable(encuesta.logit.03$Target, encuesta.logit.03$sexo,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Según gestión
CrossTable(encuesta.logit.03$Target, encuesta.logit.03$Gestion,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Según naturaleza de asignatura
CrossTable(encuesta.logit.03$Target, encuesta.logit.03$cuantitativa,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Según acreditación
CrossTable(encuesta.logit.03$Target, encuesta.logit.03$Acreditacion,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

correlacion.logit <- cor(encuesta.logit.03[,2:9])
correlacion.logit
corrplot(correlacion.logit, type = "upper", order = "original", 
         tl.col = "black", tl.cex = 0.9)



#Convertimos la variables categóricas a factor
encuesta.logit.03$Target <- as.factor(encuesta.logit.03$Target)
encuesta.logit.03$sexo <- as.factor(encuesta.logit.03$sexo)
encuesta.logit.03$cuantitativa <- as.factor(encuesta.logit.03$cuantitativa)
encuesta.logit.03$Gestion <- as.factor(encuesta.logit.03$Gestion)
encuesta.logit.03$categoria <- as.factor(encuesta.logit.03$categoria)
encuesta.logit.03$Acreditacion <- as.factor(encuesta.logit.03$Acreditacion)

#Eliminamos la variable `Item_10`, ya que una vez teniendo la variable binaria `Target`, no
#nos hace falta
encuesta.logit.03$Item_10 <- NULL


#Finalmente tenemos los datos que vamos a emplear en el logit, la primera columna es la variable 
#independiente (target, respuesta) y el resto las variables independientes (regresores, características)

#Inspeccionamos los datos

str(encuesta.logit.03)
dim(encuesta.logit.03)
summary(encuesta.logit.03)
describe(encuesta.logit.03)


##############################Estimación (entrenamiento)#######################################

valoracion.logit <- glm(Target ~ ., family = binomial(link = 'logit'), data = encuesta.logit.03)

summary(valoracion.logit)

##Efectos marginales (ver markdown para la explicación de como se calculan)

fav <- mean(dlogis(predict(valoracion.logit)))
efectos.marginales <- fav*valoracion.logit$coefficients
efectos.marginales
efectos.marginales[2]
efectos.marginales[3]
efectos.marginales[4]

#Esta función estima directamente los efectos marginales

logitmfx(Target ~ ., data = encuesta.logit.03, atmean = FALSE)


##Diagnosis del modelo

#Pseudo-R^2 

pseudo.R <- 1 - valoracion.logit$deviance/valoracion.logit$null.deviance


#Contraste de significatividad global

lrtest(valoracion.logit)

#Matriz de confusión

table(encuesta.logit.03$Target)
table(predicciones = predict(valoracion.logit, newdata = encuesta.logit.03, type= 'response')>0.5)
matriz.confusion <-  table(prediccion = predict(valoracion.logit, newdata = encuesta.logit.03, type='response')>0.5,
                           observado =encuesta.logit.03$Target)
matriz.confusion

#Calculamos la precisión, especificidad y sensibilidad 

A1 = matriz.confusion[1,1] / sum(matriz.confusion[,1])
A2 = matriz.confusion[2,2] / sum(matriz.confusion[,2])
A3 <- sum(diag(matriz.confusion)) / sum(matriz.confusion)

A1
A2
A3


#El tomar 0,5 como límite para determinar la probabilidad de una valoración alta es una decisión estándar pero
#arbitraria. Existen maneras de determinar si esta tasa es la adecuada empleando varios criterios, a continuación
#mostramos como varía la precisión del modelo con distintos limites.

prediccion <- prediction(fitted(valoracion.logit),encuesta.logit.03$Target)
precision <- performance(prediccion,'acc')
ac.val = max(unlist(precision@y.values))
th = unlist(precision@x.values)[unlist(precision@y.values) == ac.val]
plot(precision)
abline(v=th, col='grey', lty=2)

#El límite que maximiza la precisión es `th`. Repetimos la elaboración de la matriz de confusión con ese límite

table(encuesta.logit.03$Target)
table(predicciones = predict(valoracion.logit, newdata = encuesta.logit.03, type= 'response')>th)
matriz.confusion <-  table(prediccion = predict(valoracion.logit, newdata = encuesta.logit.03, type='response')>th,
                           observado =encuesta.logit.03$Target)
matriz.confusion

#Calculamos la precisión, especificidad y sensibilidad 

A1 = matriz.confusion[1,1] / sum(matriz.confusion[,1])
A2 = matriz.confusion[2,2] / sum(matriz.confusion[,2])
A3 <- sum(diag(matriz.confusion)) / sum(matriz.confusion)

A1
A2
A3


#Curva ROC

plot(performance(prediccion,'tpr','fpr'),colorize=T)
abline(0,1,lty=2, col='grey')
lines(x=c(0, 1), y=c(0, 1), col="grey", lty=2)

#AUC
auc = performance(prediccion, "auc")
auc = unlist(auc@y.values)
auc


#Debido a los peligros que representa un modelo sobreajustado, repetimos el ejercicio anterior con una
#sub muestra de entrenamiento (65%) y otra de prueba (35%).

set.seed(123) 
sample = sample.split(encuesta.logit.03$Target, SplitRatio = .65)
encuesta.train = subset(encuesta.logit.03, sample == TRUE)
encuesta.test  = subset(encuesta.logit.03, sample == FALSE)

valoracion.logit.01 <- glm(Target ~ ., family = binomial(link = 'logit'), data = encuesta.train)

summary(valoracion.logit)

logitmfx(Target ~ ., data = encuesta.train, atmean = FALSE)

pseudo.R.01 <-  1 - valoracion.logit.01$deviance/valoracion.logit.01$null.deviance

lrtest(valoracion.logit.01)

#Matriz de confusión

table(encuesta.test$Target)
table(predicciones = predict(valoracion.logit.01, newdata = encuesta.test, type= 'response')>0.5)
matriz.confusion.01 <-  table(prediccion = predict(valoracion.logit.01, newdata = encuesta.test, type='response')>0.5,
                              observado =encuesta.test$Target)
matriz.confusion.01

#Calculamos la precisión, especificidad y sensibilidad 

A1 = matriz.confusion.01[1,1] / sum(matriz.confusion.01[,1])
A2 = matriz.confusion.01[2,2] / sum(matriz.confusion.01[,2])
A3 <- sum(diag(matriz.confusion.01)) / sum(matriz.confusion.01)

A1
A2
A3

#Determinamos de nuevo el límite que maximiza la precisión

prediccion.01 <- prediction(predict(valoracion.logit.01, newdata = encuesta.test, type= 'response'),
                            encuesta.test$Target)
precision.01 <- performance(prediccion.01,'acc')
ac.val.01 = max(unlist(precision.01@y.values))
th.01 = unlist(precision.01@x.values)[unlist(precision.01@y.values) == ac.val.01]
plot(precision.01)
abline(v=th.01, col='grey', lty=2)

#En este caso tenemos 3 valores que maximizan la precisión. Empleamos el mayor de los valores `th.01[1]`

table(encuesta.test$Target)
table(predicciones = predict(valoracion.logit.01, newdata = encuesta.test, type= 'response')>unname(th.01[1]))
matriz.confusion.01 <-  table(
  prediccion = predict(valoracion.logit.01, newdata = encuesta.test, type='response')>unname(th.01[1]),
  observado =encuesta.test$Target)
matriz.confusion.01

#Calculamos la precisión, especificidad y sensibilidad 

A1 = matriz.confusion.01[1,1] / sum(matriz.confusion.01[,1])
A2 = matriz.confusion.01[2,2] / sum(matriz.confusion.01[,2])
A3 <- sum(diag(matriz.confusion.01)) / sum(matriz.confusion.01)

A1
A2
A3

#Curva ROC del modelo en submuestras

plot(performance(prediccion.01,'tpr','fpr'),colorize=T)
abline(0,1,lty=2, col='grey')
lines(x=c(0, 1), y=c(0, 1), col="grey", lty=2)

#AUC
auc.01 = performance(prediccion.01, "auc")
auc.01 = unlist(auc.01@y.values)
auc.01


#-----------------------------------------------------------------------------------------
#                        Elaboración Automática de Reportes
#-----------------------------------------------------------------------------------------

ptm <- proc.time()

#Cargamos el archivo con los items de preguntas de la encuesta
items <- read_excel("items.xlsx", col_names = FALSE)
save(items, file = '~/Reporte/items.RData')

#Elaboramos los reportes en base a la información de las asignaturas,
#profesores y las valoraciones de los items. Así que prescindimos de los
#datos originales las columnas  de División, Grupo y Asignatura
reporte.01 <- encuesta[,-c(1:3)]

#Guardamos la tabla para usarla posteriormente en un shiny
write.csv(reporte.01, file = "~/Reporte/reporte.csv")
#write.table(reporte.01, file = "reporte.csv")
save(reporte.01, file = '~/Reporte/reporte.RData')

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

save(profes.01, file = '~/Reporte/profes.RData')
save(cursos.01, file = '~/Reporte/cursos.RData')

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


tiempo <- proc.time() - ptm

sprintf('Se han elaborado los reportes en %f minutos\n', tiempo[3]/60)
