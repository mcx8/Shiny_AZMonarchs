# Maxine Cruz
# tmcruz@arizona.edu
# Created: September 23, 2022
# Last edited: October 10, 2022
# Version: RStudio 2021.09.0+351 "Ghost Orchid" Release

# Purpose:
  # Generates an R Shiny app for AZ Monarch data from GBIF and iNaturalist
  # Interactive - shows and plots available data by month on map

##############################################################################
# Load libraries
##############################################################################

library(shiny)
library(semantic.dashboard)
library(leaflet)


##############################################################################
# Load monarch_data.csv
##############################################################################

monarchs <- read.csv("AZ_Monarchs_map/data/monarch_data.csv")


##############################################################################
# Define UI (User Interface)
##############################################################################

ui <- dashboardPage(
  
  # Title of page (shows up on browser tab)
  title = "Arizona Monarchs",
  
  # Add header stuff
  dashboardHeader(
    
    color = "yellow", inverted = TRUE,
    
    title = span(
      tags$img(src="monarch-butterfly-ge3a41ab2f_1920.jpg", width = '100%'), 
      column(12, class="title-box", 
             tags$h1(class="primary-title", style='margin-top:10px;', 
                     "ARIZONA MONARCH OCCURENCES"), 
             tags$h2(class="primary-subtitle", style='margin-top:10px;', 
                     "Data from January 2000 - December 2022")))
    ),
  
  # Add content to main body of page
  dashboardBody(
    
    fluidRow(
    # Add interactive content
    
    # First item
    box(
      h3("Welcome!"),
      helpText("Insert note here"),
    
    # Second item
      checkboxGroupInput("checkGroup", 
                         h3("Select data"), 
                         choices = list("Choice 1" = 1, 
                                        "Choice 2" = 2, 
                                        "Choice 3" = 3),
                         selected = 1),
    
    # Third item
      sliderInput("slider",
                  "Date:",
                  min = as.Date("2000-01-01"),
                  max = as.Date("2022-12-31"),
                  value = as.Date("2010-01-01"),
                  timeFormat="%b %Y"),
      textOutput("SliderText"),
    
    # Fourth item
      h3("End Notes"),
      helpText("Insert note here")
    ),
    
    # Add map
    box(tags$style(type = "text/css", 
                   "#map {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map"))
    )
    
    )
  
)


##############################################################################
# Define server logic
##############################################################################

server <- function(input, output) {
  
  output$map <- renderLeaflet({ leaflet() %>%
      setView(lng = -111.682299, lat = 34.496789, zoom = 6.4) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addCircles(
        data = monarchs,
        color = "#000000",
        fillColor = "#ffffff",
        fillOpacity = 0.5)
  })

}


##############################################################################
# Run the application 
##############################################################################

shinyApp(ui = ui, server = server)

