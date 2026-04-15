library(shiny)
library(shinyjs)

options(shiny.maxRequestSize = 50 * 1024^2)

# CSS inline -----------------------------------------------------------------------------------------------------------------------------
custom_css <- "
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}
.pl-card {
  background: #fff;
  border: 0.5px solid rgba(0,0,0,0.12);
  border-radius: 12px;
  padding: 1.1rem 1.3rem;
  margin-bottom: 0.9rem;
}
.section-label {
  font-size: 11px;library(shiny)
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
  font-weight: 500;
  letter-spacing: 0.06em;
  color: #888;
  text-transform: uppercase;
  margin-bottom: 10px;
}
.tmt-group {
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  gap: 8px;
  margin-bottom: 4px;
}
.tmt-pill input[type='radio'] {
  display: none;
}
.tmt-pill label {
  display: block;
  padding: 8px 6px;
  border: 0.5px solid rgba(0,0,0,0.14);
  border-radius: 8px;
  background: #f5f5f5;
  color: #666;
  font-size: 13px;
  text-align: center;
  cursor: pointer;
  transition: all 0.15s;
  line-height: 1.3;
}
.tmt-pill label .tmt-sub {
  display: block;
  font-size: 11px;
  opacity: 0.7;
  margin-top: 2px;
}
.tmt-pill input[type='radio']:checked + label {
  background: #E6F1FB;
  border-color: #378ADD;
  color: #0C447C;
  font-weight: 500;
}
.zero-note {
  font-size: 12px;
  color: #555;
  padding: 8px 10px;
  background: #f5f9ff;
  border-left: 2px solid #378ADD;
  border-radius: 4px;
  margin-top: 8px;
}
.charts-chips {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
  margin-top: 8px;
}
.chart-chip {
  font-size: 12px;
  padding: 5px 10px;
  border-radius: 6px;
  background: #f5f5f5;
  border: 0.5px solid rgba(0,0,0,0.10);
  color: #555;
  display: flex;
  align-items: center;
  gap: 6px;
}
.chart-chip-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: #1D9E75;
  flex-shrink: 0;
  display: inline-block;
}
.two-col {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 12px;
}
.form-group label {
  font-size: 13px;
  color: #555;
}
.form-control {
  font-size: 13px !important;
}
.extra-opts {
  border: 0.5px solid rgba(0,0,0,0.12);
  border-radius: 8px;
  padding: 8px 12px;
  background: #f9f9f9;
  margin-top: 10px;
}
.extra-opts .checkbox {
  margin: 0;
  font-size: 13px;
}
.btn-generate {
  width: 100%;
  padding: 9px;
  font-size: 14px;
  font-weight: 500;
  border: 0.5px solid rgba(0,0,0,0.18);
  border-radius: 8px;
  background: #fff;
  color: #111;
  cursor: pointer;
  margin-top: 4px;
}
.btn-generate:hover {
  background: #f5f5f5;
}
.btn-download {
  display: inline-block;
  margin-top: 1rem;
  padding: 8px 18px;
  font-size: 13px;
  border: 0.5px solid rgba(0,0,0,0.18);
  border-radius: 8px;
  background: #fff;
  color: #111;
  text-decoration: none;
  cursor: pointer;
}
.col-sm-4 {
  max-width: 360px;
}
hr.pl-divider {
  border: none;
  border-top: 0.5px solid rgba(0,0,0,0.10);
  margin: 10px 0 12px;
}
"

# UI -------------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(
  title = "ProteoLab Suite",
  header = tags$head(tags$style(HTML(custom_css))),
  
  # shinyjs must be initialised inside the UI
  useShinyjs(),
  
  # Tab 1: Ratio Check -------------------------------------------------------------------------------------------------------------------
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
                // notify server that pill changed so it can reset inputs
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
  
  # Tab 2: Overlap -----------------------------------------------------------------------------------------------------------------------
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

# Server ---------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  is_zero <- reactive({ isTRUE(input$tmt == "tmtzero") })
  
  # Reset all inputs and hide download button when pill changes
  observeEvent(input$tmt_changed, {
    # Reset file inputs
    reset("data")
    reset("volume")
    # anno and tmtextra are renderUI — reset by re-rendering (handled below)
    # Also clear any previously generated report
    archivo_tmp(NULL)
  })
  
  output$anno_ui <- renderUI({
    if (!is_zero()) {
      fileInput("anno", "Annotation file", accept = ".txt")
    }
  })
  
  output$zero_note_ui <- renderUI({
    if (is_zero()) {
      div(class = "zero-note",
          "No multiplexing. The report will include a quality analysis equivalent to the standard TMT check"
      )
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
        div(class = "extra-opts",
            checkboxInput("tmtextra", "LysN extra labeling", value = FALSE)
        )
      )
    }
  })
  
  # -- Ratio Check render ----------------------------------------------------------------------------------------------------------------
  
  archivo_tmp <- reactiveVal(NULL)
  
  observeEvent(input$generar, {
    req(input$data)
    
    anno_path <- if (!is_zero()) {
      req(input$anno)
      input$anno$datapath
    } else {
      NULL
    }
    
    tmtextra_val <- isTRUE(input$tmtextra)
    
    tmpfile <- tempfile(fileext = ".html")
    
    withProgress(message = "Generating report...", value = 0.5, {
      rmarkdown::render(
        input       = "ratio_check_report.Rmd",
        output_file = tmpfile,
        params      = list(
          data     = input$data$datapath,
          anno     = anno_path,
          tmt      = input$tmt,
          tmtextra = tmtextra_val,
          volume   = input$volume
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
    content  = function(file) file.copy(archivo_tmp(), file)
  )
  
  # -- Overlap render --------------------------------------------------------------------------------------------------------------------
  
  archivo_tmp_ov <- reactiveVal(NULL)
  
  observeEvent(input$generar_ov, {
    req(input$data_ov)
    tmpfile_ov <- tempfile(fileext = ".html")
    
    withProgress(message = "Generating overlap report...", value = 0.5, {
      rmarkdown::render(
        input       = "fraction_overlap_report.Rmd",
        output_file = tmpfile_ov,
        params      = list(data = input$data_ov$datapath),
        envir       = new.env(parent = globalenv())
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
    content  = function(file) file.copy(archivo_tmp_ov(), file)
  )
}

shinyApp(ui, server)