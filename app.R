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

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Planetary Data Editor"),
  fileInput("upload", "Upload a YAML file", accept = c("yml")),
  tableOutput('table')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  planetary_data <- reactive({
    req(input$upload)
    read_planetary_data(input$upload$datapath)
  })
  
  output$table <- renderTable({
    planetary_data()$system
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
