library("rmarkdown")
library("charlatan")
library("readr")
library(readxl)

items <- read_excel("items.xlsx", col_names = FALSE)
reporte <- read_csv("reporte.csv")
reporte <- reporte[,-c(1)]

#Para guardar el anonimato de las encuestas, se generean nombres de asignaturas y profesores
#ficticios, empleando el paquete `charlatan`. Se crea un data frame en el cual se almacenan
#los códigos de profesores y asignaturas con sus nombres ficticios correspondientes

#Genera nombres para profesores
profes.cod <- unique(reporte$Profesor)
nombres.fic <-  ch_generate('name',n=length(unique(reporte$Profesor)))
profes.01 <- data.frame(profes.cod, nombres.fic)
rownames(profes.01) <- profes.01$profes
profes.01[,1] <- NULL

#Genera nombres para asignaturas
asignaturas.cod <- unique(reporte$Asignatura)
asignaturas.fic <- ch_generate('job',n=length(unique(reporte$Asignatura)))
cursos.01 <- data.frame(asignaturas.cod, asignaturas.fic)
rownames(cursos.01) <- cursos.01$asignaturas.cod
cursos.01[,1] <- NULL

input <- cursos.01[3,1]

tablas <- list()
nombres <- list()

temporal <- subset(cursos.01, job == input)
input.01 <- as.integer(row.names(temporal))
reptemp <- subset(reporte, Asignatura == input.01)  #Se filtra por asignatura
profesores <- unique(reptemp$Profesor)            #Se determina cuantos profersores tiene la asignatura
  for(j in profesores){                           #Se elabora un reporte por (asignatura,profesor)
    nombre <- paste(i,j, sep = '-')               #Se crea una etiqueta (i,j) de la forma 'i-j' para localizar la tabla en la lista
    nombres[[nombre]] <- nombre                   #Se crea una lista de etiquetas, que se emplear? posteriormente
    reptemp.01 <- subset(reptemp, Profesor == j)  #Se filtra por profesor
    tab1 <- sapply(reptemp.01, function(x) round(mean(x), digits = 2))    #Se calcula la media de cada item y se almacena en `tab1`
    tab2 <- sapply(reptemp.01, function(x) round(sd(x), digits=2))        #Se calcula la desviación típica de cada item y se almacena en `tab2`
    reptemp.02 <- sapply(reptemp.01[,3:ncol(reptemp.01)],
                         function(x) unname(table(factor(x, levels = 1:10)))    #Se calcula la tabla de frecuencias por item. Se convierte a factor para que
    )                                                     #aparezcan los items con frecuencia cero y se eliminan las etiquetas con `unname`  
    tab3 <- rbind(reptemp.02, rbind(tab1[3:length(tab1)], tab2[3:length(tab2)]))  #Juntamos la tabla de frecuencias con los vectores de media y desviaci?n
    tabla.freq <- as.data.frame(t(tab3))                                          #Se transpone la tabla
    colnames(tabla.freq) <- c(seq(1:10), 'Media', 'Desviación Típica')            #Se nombran las columnas
    rownames(tabla.freq) <- items$X0                                    #Se añaden las preguntas de cada item
    tablas[[nombre]] <- tabla.freq[,c(11,12,seq(1:10))]                 #Se reordenan las columnas y almacena la tabla en la lista
  }


for (i in 1:length(tablas)){
  cuadro <- tablas[[i]]                   #Se accede a la tabla. Esta es la tabla que aparece en el informe
  id <- nombres[[i]]                      #Se accede a la etiqueta generada en el bucle anterior
  id2 <- strsplit(id[[1]], split = '-')   #Se separa el c?digo de asignatura y profesor
  id.01 <- id2[[1]][1]                    #C?digo de asignatura
  id.02 <- id2[[1]][2]                    #C?digo de profesor
  prof <- profes.01[row.names(profes.01)==id.02,]     #Se asocia el c?digo de profesor a su nombre ficticio
  curs <- cursos.01[row.names(cursos.01)==id.01,]     #Se asocia el c?digo de asignatura a su nombre ficticio
  n <- c(curs,prof)
  s <- c('Asignatura', 'Profesor')
  df <- data.frame(s,n)                       #Se crea un data frame con los nombres de asignatura y profesor
  colnames(df) <- c()                         #Se eliminan las etiquetas de las columnas
  cabecera <- t(df)                           #Se traspone el data frame. Este es la cabecera del informe
  render("prueba_reporte.rmd",output_file = paste0('report.', id, '.pdf'))    
}
