# Load libraries
library(shiny)

# UI
ui <- fluidPage(
  titlePanel("TMT ratio check report"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data", "Upload FragPipe's psm.tsv file:", accept = ".tsv"),
      fileInput("anno", "Upload Fragpipe's annotation file:", accept = ".txt"),
      radioButtons("tmt", "What kind of TMT label was used?", 
                   choices = c("TMT" = "tmt",
                               "TMTpro" = "tmtpro")),
      numericInput("volume", "Desired volume (uL):", value = 30),
      actionButton("generar", "Generate report")
    ),
    mainPanel(
      uiOutput("descarga_ui")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive para guardar temporalmente la ruta del archivo generado
  archivo_tmp <- reactiveVal(NULL)
  
  observeEvent(input$generar, {
    # Crear archivo temporal
    tmpfile <- tempfile(fileext = ".html")
    
    # Renderizar el .qmd/.Rmd con los parametros
    # quarto::quarto_render(
    #   input = "ratio_check_report.qmd",
    #   output_file = tmpfile,
    #   execute_params = list(
    #     data = input$data$datapath,
    #     anno = input$anno$datapath,
    #     tmt = input$tmt,
    #     volume = input$volume
    #   )
    # )
    # 
    rmarkdown::render(
      input = "ratio_check_report.Rmd",
      output_file = tmpfile,
      params = list(
        data = input$data$datapath,
        anno = input$anno$datapath,
        tmt = input$tmt,
        volume = input$volume
      ),
      envir = new.env(parent = globalenv())
    )
    
    # Guardar la ruta para el handler de descarga
    archivo_tmp(tmpfile)
  })
  
  output$descarga_ui <- renderUI({
    req(archivo_tmp())
    downloadButton("descargar", "Descargar report")
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".html")
    },
    content = function(file) {
      file.copy(archivo_tmp(), file)
    }
  )
}

# Run app
shinyApp(ui, server)
