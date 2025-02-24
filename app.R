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
  downloadButton("download"),
  rHandsontableOutput('system'),
  rHandsontableOutput('system_events'),
  rHandsontableOutput('planets'),
  rHandsontableOutput('prime_planet_events')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  planetary_data <- reactive({
    req(input$upload)
    read_planetary_data(input$upload$datapath)
  })
  
  output$system <- renderRHandsontable({
    planetary_data()$system %>%
      rhandsontable() %>%
      hot_col(col = c("id", "sucsId", "xcood", "ycood"), readOnly = TRUE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  output$system_events <- renderRHandsontable({
    planetary_data()$system_events %>%
      mutate(date = as.character(date)) %>%
      rhandsontable() %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  output$planets <- renderRHandsontable({
    planetary_data()$planets %>%
      select(!desc) %>%
      mutate(atmosphere = factor(atmosphere)) %>%
      rhandsontable() %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  output$prime_planet_events <- renderRHandsontable({
    planetary_data()$planetary_events[[planetary_data()$system$primarySlot]] %>%
      mutate(date = as.character(date)) %>%
      rhandsontable() %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "test.yml"
    },
    content = function(file) {
      planetary_system <- planetary_data()
      planetary_system$system <- as_tibble(hot_to_r(input$system))
      write_planetary_data(planetary_system, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
