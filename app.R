library(shiny)
library(shinyjs)

options(shiny.maxRequestSize = 50 * 1024^2)

# UI ----------------------------------------------------------------------------------------------------------------
ui <- navbarPage(
  title = "ProteoLab Suite",
  header = tags$head(
    # Include external CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  useShinyjs(),
  
  # Tab 1: Ratio Check ----------------------------------------------------------------------------------------------
  tabPanel("Ratio Check",
           sidebarLayout(
             sidebarPanel(
               div(class = "pl-card",
                   div(class = "section-label", "Input files"),
                   fileInput("data", "FragPipe PSM file", accept = ".tsv"),
                   uiOutput("anno_ui")
               ),
               
               div(class = "pl-card",
                   div(class = "section-label", "TMT label type"),
                   div(class = "tmt-group",
                       div(class = "tmt-pill",
                           tags$input(type = "radio", name = "tmt", id = "tmt_tmt", value = "tmt"),
                           tags$label(`for` = "tmt_tmt",
                                      "TMT", tags$span(class = "tmt-sub", "6/10-plex"))
                       ),
                       div(class = "tmt-pill",
                           tags$input(type = "radio", name = "tmt", id = "tmt_tmtpro", value = "tmtpro", checked = NA),
                           tags$label(`for` = "tmt_tmtpro",
                                      "TMTpro", tags$span(class = "tmt-sub", "16-plex"))
                       ),
                       div(class = "tmt-pill",
                           tags$input(type = "radio", name = "tmt", id = "tmt_zero", value = "tmtzero"),
                           tags$label(`for` = "tmt_zero",
                                      "TMTpro-zero", tags$span(class = "tmt-sub", ""))
                       )
                   ),
                   tags$script(HTML("
            $(document).ready(function() {
              $('input[name=\"tmt\"]').on('change', function() {
                Shiny.setInputValue('tmt', this.value);
                Shiny.setInputValue('tmt_changed', this.value, {priority: 'event'});
              });
              $('#tmt_tmtpro').prop('checked', true);
              Shiny.setInputValue('tmt', 'tmtpro');
            });
          ")),
                   uiOutput("zero_note_ui"),
                   uiOutput("charts_chips_ui"),
                   uiOutput("lysn_ui")
               ),
               
               div(class = "pl-card",
                   div(class = "section-label", "Output settings"),
                   numericInput("volume", "Desired volume (µL)", value = 30, min = 1, max = 500)
               ),
               
               actionButton("generar", "Generate report", class = "btn-generate")
             ),
             
             mainPanel(
               uiOutput("descarga_ui")
             )
           )
  ),
  
  # Tab 2: Overlap --------------------------------------------------------------------------------------------------
  tabPanel("Overlap",
           sidebarLayout(
             sidebarPanel(
               div(class = "pl-card",
                   div(class = "section-label", "Input files"),
                   fileInput("data_ov", "Upload file for overlap analysis:")
               ),
               actionButton("generar_ov", "Generate overlap report", class = "btn-generate")
             ),
             mainPanel(
               uiOutput("descarga_ov_ui")
             )
           )
  )
)

# Server ------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  is_zero <- reactive({ isTRUE(input$tmt == "tmtzero") })
  
  observeEvent(input$tmt_changed, {
    reset("data")
    reset("volume")
    archivo_tmp(NULL)
  })
  
  output$anno_ui <- renderUI({
    if (!is_zero()) fileInput("anno", "Annotation file", accept = ".txt")
  })
  
  output$zero_note_ui <- renderUI({
    if (is_zero()) {
      div(class = "zero-note", "No multiplexing. Quality analysis equivalent to standard TMT check")
    }
  })
  
  output$charts_chips_ui <- renderUI({
    if (is_zero()) {
      tagList(
        tags$hr(class = "pl-divider"),
        div(class = "section-label", style = "margin-top:4px;", "Plots to be generated"),
        div(class = "charts-chips",
            div(class = "chart-chip", tags$span(class = "chart-chip-dot"), "Missed cleavages"),
            div(class = "chart-chip", tags$span(class = "chart-chip-dot"), "Base peak chromatogram")
        )
      )
    }
  })
  
  output$lysn_ui <- renderUI({
    if (!is_zero()) {
      tagList(
        tags$hr(class = "pl-divider"),
        div(class = "extra-opts", checkboxInput("tmtextra", "LysN extra labeling", value = FALSE))
      )
    }
  })
  
  # Ratio Check Logic
  archivo_tmp <- reactiveVal(NULL)
  
  observeEvent(input$generar, {
    req(input$data)
    anno_path <- if (!is_zero()) { req(input$anno); input$anno$datapath } else { NULL }
    tmpfile <- tempfile(fileext = ".html")
    
    withProgress(message = "Generating report...", value = 0.5, {
      rmarkdown::render(
        input = "ratio_check_report.Rmd",
        output_file = tmpfile,
        params = list(
          data = input$data$datapath, anno = anno_path,
          tmt = input$tmt, tmtextra = isTRUE(input$tmtextra), volume = input$volume
        ),
        envir = new.env(parent = globalenv())
      )
    })
    archivo_tmp(tmpfile)
  })
  
  output$descarga_ui <- renderUI({
    req(archivo_tmp())
    downloadButton("descargar", "Download report", class = "btn-download")
  })
  
  output$descargar <- downloadHandler(
    filename = function() paste0("ratio_check_", Sys.Date(), ".html"),
    content = function(file) file.copy(archivo_tmp(), file)
  )
  
  # Overlap Logic
  archivo_tmp_ov <- reactiveVal(NULL)
  
  observeEvent(input$generar_ov, {
    req(input$data_ov)
    tmpfile_ov <- tempfile(fileext = ".html")
    
    withProgress(message = "Generating overlap report...", value = 0.5, {
      rmarkdown::render(
        input = "fraction_overlap_report.Rmd",
        output_file = tmpfile_ov,
        params = list(data = input$data_ov$datapath),
        envir = new.env(parent = globalenv())
      )
    })
    archivo_tmp_ov(tmpfile_ov)
  })
  
  output$descarga_ov_ui <- renderUI({
    req(archivo_tmp_ov())
    downloadButton("descargar_ov", "Download overlap report", class = "btn-download")
  })
  
  output$descargar_ov <- downloadHandler(
    filename = function() paste0("overlap_", Sys.Date(), ".html"),
    content = function(file) file.copy(archivo_tmp_ov(), file)
  )
}

shinyApp(ui, server)