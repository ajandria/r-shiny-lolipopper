library(shiny)
library(readxl)
library(dplyr)
library(shinyjqui)
library(shinyWidgets)
library(plotly)
library(ggpubr)
library(reticulate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(2,
  theme = bslib::bs_theme(version = 4, bootswatch = "lux"),
  # Application title
  titlePanel("LOLIPOPPER"),
  # Input
  fileInput(inputId = 'gsea_input', 'Please select GSEA results', multiple = FALSE, accept = c(".xlsx")),
  numericInput(inputId = "n_gsea_records", "How many terms to plot (top):", 10),
  uiOutput("var_selection"),
  numericInput("plot_width", "Plot width:", 10, min = 1, max = 50),
  numericInput("plot_height", "Plot height:", 8, min = 1, max = 50),
  downloadButton("downloadPDF", "Download Plot as PDF")),
  column(4,
  # Main panel
  mainPanel(
    jqui_resizable(
      plotlyOutput("lolipop", height = "600px", width = "200%")
    ))
  )
),
fluidRow(
  tableOutput("table")
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Allocate GSEA results
  gsea_res <- reactive({
    req(input$gsea_input)
    read_excel(input$gsea_input$datapath)
  })
  
  output$var_selection <- renderUI({
    req(gsea_res()) # Make sure data is available
    varSelectInput(inputId = "sorting_var", "Sort by:", gsea_res())
  })
  
  n_top_records <- reactive({
    req(input$n_gsea_records)
  })
  
  selected_sorting_var <- reactive({
    req(input$sorting_var)
  })
  
  gsea_main <- reactive({
    req(gsea_res(), n_top_records(), selected_sorting_var())
    
    # Transform to numeric if not
    gsea_main <- gsea_res() %>%
      mutate(
        NES = as.numeric(NES),
        p.adjust = as.numeric(p.adjust)
      ) %>%
      dplyr::arrange(!!sym(selected_sorting_var())) %>% 
      slice(1:n_top_records())
    
    if (is.null(gsea_main$ONTOLOGY)) {
      gsea_main <- gsea_main %>% 
        mutate(ONTOLOGY = 'NONE')
    }
    
    gsea_main
  })
  
  # Store the ggplot in a reactive expression
  ggplot_res <- reactive({
    ggplot_object <- ggdotchart(gsea_main(), x = "Description", y = "NES",
                                color = "ONTOLOGY",                                # Color by groups
                                palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                                sorting = "descending",                       # Sort value in descending order
                                rotate = T,
                                add = "segments",                             # Add segments from y = 0 to dots
                                add.params = list(color = "lightgray", size = 2), # Change segment color and size
                                group = "ONTOLOGY",                                # Order by groups
                                dot.size = 8,                                 # Large dot size
                                label = paste('',label=stringr::str_remove(stringr::str_sub(gsea_main()$leading_edge, 6, 8), ',')),  # Add tags to enrichment plot from the leading edge analysis
                                font.label = list(color = "white", size = 9,
                                                  vjust = 0.5),               # Adjust label parameters
                                ggtheme = theme_pubr()                        # ggplot2 theme
    )+
      geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
    
    return(ggplot_object)
  })
  
  # Convert ggplot to plotly for rendering
  output$lolipop <- renderPlotly({
    plotly::ggplotly(ggplot_res())
  })
  
  output$table <- renderTable(
    gsea_main()
  )
  
  # Server logic for PDF download
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste('GSEA_plot', Sys.Date(), '.pdf', sep = '')
    },
    content = function(file) {
      ggplot2::ggsave(filename = file, plot = ggplot_res(), device = 'pdf', width = input$plot_width, height = input$plot_height, limitsize = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
