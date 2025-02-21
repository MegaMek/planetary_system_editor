#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(megamekR)
library(dplyr)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Planetary Data Editor"),
  fileInput("upload", "Upload a YAML file", accept = c("yml")),
  rHandsontableOutput('system'),
  tableOutput('system_events'),
  tableOutput('planets'),
  tableOutput('prime_planet_events')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  planetary_data <- reactive({
    req(input$upload)
    read_planetary_data(input$upload$datapath)
  })
  
  output$system <- renderRHandsontable({
    rhandsontable(planetary_data()$system) %>%
      hot_col(col = c("id", "sucsId", "xcood", "ycood"), readOnly = TRUE) %>%
      hot_col(col = c("source_spectralType", "source_primarySlot"), type = "text") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  output$system_events <- renderTable({
    planetary_data()$system_events |>
      mutate(date = as.character(date))
  })
  
  output$planets <- renderTable({
    planetary_data()$planets |>
      select(!desc)
  })
  
  output$prime_planet_events <- renderTable({
    planetary_data()$planetary_events[[planetary_data()$system$primarySlot]] |>
      mutate(date = as.character(date))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
