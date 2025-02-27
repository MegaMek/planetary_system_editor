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
                         eDTOutput('system')
                       ),
                       card(
                         card_header("Planetary System Events"),
                         eDTOutput('systemEvents')
                       ),
                       card(
                         card_header("Base Planet Information"),
                         eDTOutput('planets'),
                       )
                       ))
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  planetary_data <- reactive({
    req(input$upload)
    read_planetary_data(input$upload$datapath)
  })
  
  modifiedPlanetarySystem <- reactiveVal()
  modifiedSystemEvents <- reactiveVal()
  modifiedPlanets <- reactiveVal()
  modifiedLandmasses <- reactiveVal()
  modifiedSatellites <- reactiveVal()
  modifiedPlanetaryEvents <- reactiveVal()
  
  
  observe({
    req(planetary_data())
    modifiedPlanetarySystem(eDT(id = 'system', 
                                data = planetary_data()$system, 
                                canDeleteRow = FALSE,
                                options = list(dom = 'Bt', 
                                               keys = TRUE,
                                               ordering = FALSE,
                                               autoFill = list(update =FALSE, focus = "focus"),
                                               buttons = list("undo", "redo", "save")),
                                editable = list(target = "cell",
                                                disable = list(columns = 1:4))))
    modifiedSystemEvents(eDT(id = 'systemEvents', 
                             data = planetary_data()$system_events,
                             options = list(dom = 'Bt', 
                                            keys = TRUE,
                                            ordering = FALSE,
                                            autoFill = list(update =FALSE, focus = "focus"),
                                            buttons = list("add","undo", "redo", "save"))))
    modifiedPlanets(eDT(id = 'planets', 
                        data = planetary_data()$planets,
                        options = list(dom = 'Bt', 
                                       keys = TRUE,
                                       ordering = FALSE,
                                       autoFill = list(update =FALSE, focus = "focus"),
                                       buttons = list("add","undo", "redo", "save"))))
    
    landmasses <- list()
    for(i in 1:length(planetary_data()$landmasses)) {
      if(!is.null(planetary_data()$landmasses[[i]])) {
        lm <- eDT(id = paste("landmass", i, sep=""),
                  data = planetary_data()$landmasses[[i]],
                  options = list(dom = 'Bt', 
                                 keys = TRUE,
                                 ordering = FALSE,
                                 autoFill = list(update =FALSE, focus = "focus"),
                                 buttons = list("add","undo", "redo", "save")))
        landmasses[[paste("landmass", i, sep="")]] <- lm
      } else {
        landmasses[paste("landmass", i, sep="")] <- list(NULL)
      }
    }
    modifiedLandmasses(landmasses)
    
    satellites <- list()
    for(i in 1:length(planetary_data()$satellites)) {
      if(!is.null(planetary_data()$satellites[[i]])) {
        sat <- eDT(id = paste("satellite", i, sep=""),
                  data = planetary_data()$satellites[[i]],
                  options = list(dom = 'Bt', 
                                 keys = TRUE,
                                 ordering = FALSE,
                                 autoFill = list(update =FALSE, focus = "focus"),
                                 buttons = list("add","undo", "redo", "save")))
        satellites[[paste("satellite", i, sep="")]] <- sat
      } else {
        satellites[paste("satellite", i, sep="")] <- list(NULL)
      }
    }
    modifiedSatellites(satellites)
    
    planetary_events <- list()
    for(i in 1:length(planetary_data()$planetary_events)) {
      if(!is.null(planetary_data()$planetary_events[[i]])) {
        pevents <- eDT(id = paste("planetary_events", i, sep=""),
                   data = planetary_data()$planetary_events[[i]],
                   options = list(dom = 'Bt', 
                                  keys = TRUE,
                                  ordering = FALSE,
                                  autoFill = list(update =FALSE, focus = "focus"),
                                  buttons = list("add","undo", "redo", "save")))
        planetary_events[[paste("planetary_events", i, sep="")]] <- pevents
      } else {
        planetary_events[paste("planetary_events", i, sep="")] <- list(NULL)
      }
    }
    modifiedPlanetaryEvents(planetary_events)
    
  })
  
  observeEvent(input$upload, {
    
    # remove old tabs
    for(tab_name in tab_names) {
      removeTab(inputId = "tabs", target = tab_name)
    }
    tab_names <- c()
    n <- nrow(planetary_data()$planets)
    for(i in 1:n) {
      insertTab(inputId = "tabs",
                tabPanel(planetary_data()$planets$name[i], 
                         card(card_header("Landmasses"),  eDTOutput(paste("landmass", i, sep=""))), 
                         card(card_header("Satellites"), eDTOutput(paste("satellite", i, sep=""))), 
                         card(card_header("Planetary Events"), eDTOutput(paste("planetary_events", i, sep="")))))
      tab_names <- c(tab_names, planetary_data()$planets$name[i])
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "test.yml"
    },
    content = function(file) {
      planetary_system <- planetary_data()
      planetary_system$system <- modifiedPlanetarySystem()$result()
      planetary_system$system_events <- modifiedSystemEvents()$result()
      planetary_system$planets <- modifiedPlanets()$result()
      for(i in 1:length(planetary_system$landmasses)) {
        if(is.null(modifiedLandmasses()[[i]])) {
          planetary_system$landmasses[i] <- list(NULL)
        } else {
          planetary_system$landmasses[[i]] <- modifiedLandmasses()[[i]]$result()
        }
      }
      for(i in 1:length(planetary_system$satellites)) {
        if(is.null(modifiedSatellites()[[i]])) {
          planetary_system$satellites[i] <- list(NULL)
        } else {
          planetary_system$satellites[[i]] <- modifiedSatellites()[[i]]$result()
        }
      }
      for(i in 1:length(planetary_system$planetary_events)) {
        if(is.null(modifiedPlanetaryEvents()[[i]])) {
          planetary_system$planetary_events[i] <- list(NULL)
        } else {
          planetary_system$planetary_events[[i]] <- modifiedPlanetaryEvents()[[i]]$result()
        }
      }
      
      write_planetary_data(planetary_system, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
