# Load libraries
library(shiny)

# Increase max upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# UI
ui <- navbarPage(
  title = "ProteoLab Suite",
  
  # ── Pestaña 1: Ratio Check ─────────────────────────────────────────────
  tabPanel("Ratio Check",
           sidebarLayout(
             sidebarPanel(
               fileInput("data", "Upload FragPipe's psm.tsv file:", accept = ".tsv"),
               fileInput("anno", "Upload Fragpipe's annotation file:", accept = ".txt"),
               radioButtons("tmt", "What kind of TMT label was used?", 
                            choices = c("TMT" = "tmt",
                                        "TMTpro" = "tmtpro")),
               checkboxInput("tmtextra", "LysN extra labeling", value = FALSE),
               numericInput("volume", "Desired volume (uL):", value = 30),
               actionButton("generar", "Generate report")
             ),
             mainPanel(
               uiOutput("descarga_ui")
             )
           )
  ),
  
  # ── Pestaña 2: Overlap ─────────────────────────────────────────────────
  tabPanel("Overlap",
           sidebarLayout(
             sidebarPanel(
               fileInput("data_ov", "Upload file for overlap analysis:"),
               # → Aquí irán los inputs específicos de overlap_report.Rmd
               actionButton("generar_ov", "Generate overlap report")
             ),
             mainPanel(
               uiOutput("descarga_ov_ui")
             )
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
    
    rmarkdown::render(
      input = "ratio_check_report.Rmd",
      output_file = tmpfile,
      params = list(
        data = input$data$datapath,
        anno = input$anno$datapath,
        tmt = input$tmt,
        tmtextra = input$tmtextra,
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
  
  # ── Overlap ────────────────────────────────────────────────────────────
  
  # Reactive para guardar temporalmente la ruta del archivo generado
  archivo_tmp_ov <- reactiveVal(NULL)
  
  observeEvent(input$generar_ov, {
    req(input$data_ov)
    # Crear archivo temporal
    tmpfile_ov <- tempfile(fileext = ".html")
    
    # Renderizar el overlap_report.Rmd con los parametros
    rmarkdown::render(
      input = "fraction_overlap_report.Rmd",
      output_file = tmpfile_ov,
      params = list(
        data = input$data_ov$datapath
        # → Añadir aquí los params que defina en overlap_report.Rmd
      ),
      envir = new.env(parent = globalenv())
    )
    
    # Guardar la ruta para el handler de descarga
    archivo_tmp_ov(tmpfile_ov)
  })
  
  output$descarga_ov_ui <- renderUI({
    req(archivo_tmp_ov())
    downloadButton("descargar_ov", "Download overlap report")
  })
  
  output$descargar_ov <- downloadHandler(
    filename = function() {
      paste0("overlap_", Sys.Date(), ".html")
    },
    content = function(file) {
      file.copy(archivo_tmp_ov(), file)
    }
  )
}

# Run app
shinyApp(ui, server)