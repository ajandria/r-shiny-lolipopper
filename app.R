library(shiny)
library(readxl)
library(dplyr)
library(shinyjqui)
library(shinyWidgets)
library(plotly)
library(ggpubr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("ggpubr lolipop chart plotter"),
  # Input
  fileInput(inputId = 'gsea_input', 'GSEA Results', multiple = FALSE, accept = c(".xlsx")),
  # Main panel
  mainPanel(
    jqui_resizable(
      plotlyOutput("lolipop", height = "600px", width = "100%")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Allocate GSEA results
  gsea_res <- reactive({
    req(input$gsea_input)
    gsea_file <- read_excel(input$gsea_input$datapath)
  })
  # Send output
  output$lolipop <- renderPlotly({
    # Validation
    req(gsea_res())
    # Transform to numeric if not
    gsea_main <- gsea_res() %>% 
      mutate(
        NES = as.numeric(NES),
        p.adjust = as.numeric(p.adjust)
      ) %>% 
      filter(p.adjust < 0.05)
    
    ggdotchart(gsea_main, x = "Description", y = "NES",
               color = "ONTOLOGY",                                # Color by groups
               palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
               sorting = "descending",                       # Sort value in descending order
               rotate = T,
               add = "segments",                             # Add segments from y = 0 to dots
               add.params = list(color = "lightgray", size = 2), # Change segment color and size
               group = "ONTOLOGY",                                # Order by groups
               dot.size = 8,                                 # Large dot size
               label = paste('',label=stringr::str_remove(stringr::str_sub(gsea_main$leading_edge, 6, 8), ',')),  # Add tags to enrichment plot from the leading edge analysis
               font.label = list(color = "white", size = 9, 
                                 vjust = 0.5),               # Adjust label parameters
               ggtheme = theme_pubr()                        # ggplot2 theme
    )+
      geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
