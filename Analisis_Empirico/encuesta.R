library(readr)
library(PerformanceAnalytics)
library(corrplot)
library(REdaS)
library(nFactors)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)

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


#Armamos una sub-muestra solo con las valoraciones de las encuestas
valoraciones.01 <- encuesta[,6:ncol(encuesta)]

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

##Análisis descriptivo

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
                           Desviación = sd(Item_10) )
encuesta.02

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



fa.valoraciones1 <- factanal(valoraciones.02, factors = 3, rotation = "promax", scores = "regression")
fa.valoraciones1$loadings
fa.valoraciones1$uniquenesses
fa.valoraciones1$STATISTIC
fa.valoraciones1$PVAL
#fa.valoraciones1$scores
#graficos
cargas1 <- fa.valoraciones1$loadings[,1:2]
plot(cargas1,type="n")
text(cargas1,labels=names(valoraciones.02),cex=.7)
