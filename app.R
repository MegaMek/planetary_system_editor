#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(bslib)
library(shiny)
library(megamekR)
library(dplyr)
library(rhandsontable)
library(editbl)

tab_names <- c()

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Planetary Data Editor",
  sidebar = sidebar(
    fileInput("upload", "Upload a YAML file", accept = c("yml")),
    downloadButton("download")
  ),
  tabsetPanel(id = "tabs",
              tabPanel("Base Information",
                       card(
                         card_header("Overall System Information"),
                         rHandsontableOutput('system')
                       ),
                       card(
                         card_header("Base Planet Information"),
                         eDTOutput('planets'),
                       )
                       ))
  )
  #  uiOutput("planetTabs")
  #   tabPanel("Planetary System Information",
  #            rHandsontableOutput('system'),
  #            h2("Planetary system events"),
  #            rHandsontableOutput('system_events')
  #   ),
  #   tabPanel("Base Planet Information",
  #            rHandsontableOutput('planets')
  #   ),
  #   tabPanel("Primary Planet Events",
  #            rHandsontableOutput('prime_planet_events')
  #   )

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
  
  modifiedPlanets <- reactiveVal()
  
  observe({
    req(planetary_data())
    modifiedPlanets(eDT(id = 'planets', data = planetary_data()$planets))
  })
  
  observeEvent(input$upload, {
    
    # remove old tabs
    for(tab_name in tab_names) {
      removeTab(inputId = "tabs", target = tab_name)
    }
    tab_names <- c()
    n <- length(planetary_data()$planetary_events)
    for(i in 1:n) {
      events <- planetary_data()$planetary_events[[i]]
      landmasses <- planetary_data()$landmasses[[i]]
      satellites <- planetary_data()$satellites[[i]]
      if(!is.null(landmasses)) {
        landmasses <- landmasses %>%
          rhandsontable() %>%
          hot_table(stretchH = "all") %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      }
      if(!is.null(satellites)) {
        satellites <- satellites %>%
          rhandsontable() %>%
          hot_table(stretchH = "all") %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      }
      if(!is.null(events)) {
        events <- events %>%
          mutate(date = as.character(date)) %>%
          rhandsontable() %>%
          hot_table(stretchH = "all") %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      }
      insertTab(inputId = "tabs",
                tabPanel(planetary_data()$planets$name[i], 
                         card(card_header("Landmasses"), landmasses), 
                         card(card_header("Satellites"), satellites), 
                         card(card_header("Planetary Events"), events)))
      tab_names <- c(tab_names, planetary_data()$planets$name[i])
    }
  })
  
  # output$system_events <- renderRHandsontable({
  #   planetary_data()$system_events %>%
  #     mutate(date = as.character(date)) %>%
  #     rhandsontable() %>%
  #     hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  # })
  # 
  
  #output$planets <- renderRHandsontable({
  #  planetary_data()$planets %>%
  #    select(!desc) %>%
  #    mutate(atmosphere = factor(atmosphere)) %>%
  #    rhandsontable() %>%
  #    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  #})
  # 
  # output$prime_planet_events <- renderRHandsontable({
  #   planetary_data()$planetary_events[[planetary_data()$system$primarySlot]] %>%
  #     mutate(date = as.character(date)) %>%
  #     rhandsontable() %>%
  #     hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  # })
  
  output$download <- downloadHandler(
    filename = function() {
      "test.yml"
    },
    content = function(file) {
      planetary_system <- planetary_data()
      planetary_system$system <- as_tibble(hot_to_r(input$system))
      planetary_system$planets <- modifiedPlanets()$result()
      write_planetary_data(planetary_system, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
