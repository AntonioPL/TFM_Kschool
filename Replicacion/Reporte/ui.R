library(shiny)

fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Seleccione una asignatura:',
                  choices = cursos.01[,1] ),
      radioButtons('format', 'Formato del reporte', c('PDF'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    mainPanel(
      plotOutput('tablaFrec',  width = "100%", height = "800px")
    )
  )
)