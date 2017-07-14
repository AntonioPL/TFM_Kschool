library("shiny")
library("rmarkdown")
library("charlatan")
library("readr")
library("readxl")
library("ggplot2")
library("reshape2")


tablas.funcion <- function(x){
  tablas <- list()
  temporal <- subset(cursos.01, job == x)
  input.01 <- as.integer(row.names(temporal))
  reptemp <- subset(reporte.01, Asignatura == input.01)  
  profesores <- unique(reptemp$Profesor)           
  profesores2 <- profesores
  for(j in profesores2){                           
    nombre <- paste(input.01,j, sep = '-')               
    reptemp.01 <- subset(reptemp, Profesor == j)  
    tab1 <- sapply(reptemp.01, function(x) round(mean(x), digits = 2))    
    tab2 <- sapply(reptemp.01, function(x) round(sd(x), digits=2))        
    reptemp.02 <- sapply(reptemp.01[,3:ncol(reptemp.01)],
                         function(x) unname(table(factor(x, levels = 1:10))))                                                     
    tab3 <- rbind(reptemp.02, rbind(tab1[3:length(tab1)], tab2[3:length(tab2)]))  
    tabla.freq <- as.data.frame(t(tab3))                                          
    colnames(tabla.freq) <- c(seq(1:10), 'Media', 'Desviación Típica')            
    rownames(tabla.freq) <- items$X0                                    
    tablas[[nombre]] <- tabla.freq[,c(11,12,seq(1:10))]  
    return(tablas)
  }
}


nombres.funcion <- function(y){
  nombres.temp <- list()
  temporal.p <- subset(cursos.01, job == y)
  input.01.p <- as.integer(row.names(temporal.p))
  reptemp.p <- subset(reporte.01, Asignatura == input.01.p)  
  profesores.p <- unique(reptemp.p$Profesor)           
  profesores3 <- profesores.p
  for(j in profesores3){                           
    nombre.p <- paste(input.01.p,j, sep = '-')               
    nombres.temp[[nombre.p]] <- nombre.p
    return(nombres.temp)
  }
}


#En la siguiente función se hace un subset según la asignatura, que servirá 
#para hacer el gráfico de frecuencias por item de la encuesta correspondiente
#a dicha asignatura

datos.grafico <- function(z){
  nombres.temp2 <- list()
  temporal.q <- subset(cursos.01, job == z)
  input.01.q <- as.integer(row.names(temporal.q))
  pp <- subset(reporte.01, Asignatura == input.01.q)
  pp.flat <- melt(pp[3:12])
  
}

shinyServer(
function(input, output) {

  
  tablas <- reactive({
  tablas.funcion(input$x)  
    })
  
  
  nombres <- reactive({
    nombres.funcion(input$x)
  })
  
  datos.grafico.01 <-   reactive({
    datos.grafico(input$x)
  })
  
  output$tablaFrec <- renderPlot({
    ppp <- datos.grafico.01()
    h <- ggplot(ppp)
    h <- h + aes(value)
    h <- h + geom_histogram(fill='steelblue', col = 'black', breaks=seq(0, 10, by = 1)) 
    h <- h + facet_wrap(~variable) + ggtitle("Histogramas de las Valoraciones por Item") +
      theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el título
    h <- h + labs(x="Valoración", y="Frecuencia")
    h
    })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('reporte', sep = '.', switch(
        input$format, PDF = 'pdf'
      ))
    },
  
    
    
    
    content = function(file) {
      src <- normalizePath('Prueba_reporte.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Prueba_reporte.Rmd', overwrite = TRUE)
      
      nombres2 <- nombres()
      tablas3 <- tablas()
      nombres3 <- as.character(nombres2)
      for (i in 1:length(tablas3)){
        cuadro <- tablas3[[i]]                   #Se accede a la tabla. Esta es la tabla que aparece en el informe
        id <- nombres3[[i]]                      #Se accede a la etiqueta generada en el bucle anterior
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
      }
      
      
      
      library(rmarkdown)
      out <- render('Prueba_reporte.Rmd', switch(
        input$format,
        PDF = pdf_document()
      ))
      file.rename(out, file)
    }
  )
  
}

)