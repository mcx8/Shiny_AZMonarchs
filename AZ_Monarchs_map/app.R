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

# For app
library(shiny)
library(shinydashboard)
library(leaflet)

# For data
library(spocc)
library(dplyr)
library(ggplot2)
library(rinat)
library(rnaturalearth)
library(lubridate)


##############################################################################
# Collect occurrence data from GBIF and iNaturalist
##############################################################################

# Pull data from GBIF
mdata <- occ(query = "Danaus plexippus",
             from = "gbif",
             gbifopts = list(country = "US",
                             stateProvince = "Arizona",
                             year = "2000, 2022"),
             limit = 1100)

# Searched: gbif
# Occurrences - Found: 1,038, Returned: 500
# Search type: Scientific
# gbif: Danaus plexippus (500)

# Pull data from iNaturalist
inat <- get_inat_obs(query = "Danaus plexippus",
                     quality = "research",
                     place_id = 40,
                     maxresults = 1000)

# Without maxresults = 1000, the default maximum is 100


##############################################################################
# Clean data
##############################################################################

# Isolate GBIF data from other stuff
mdata <- mdata$gbif$data$Danaus_plexippus

# Store N/A values under a different dataframe
mdata_na <- mdata[is.na(mdata$individualCount), ]

# Remove N/A values in individualCount
mdata <- mdata[!is.na(mdata$individualCount), ]

# Keep necessary columns
mdata <- select(mdata,
                c(longitude, latitude, year, month, day))

mdata_na <- select(mdata_na,
                   c(longitude, latitude, year, month, day))

inat <- select(inat,
               c(longitude, latitude, observed_on))

# iNat does not have year, month, and day separated
inat <- inat %>% 
  mutate(observed_on = ymd(observed_on)) %>% 
  mutate_at(vars(observed_on), funs(year, month, day)) %>%
  select(longitude, latitude, year, month, day)

# Add new column to dataframes designating the type
mdata$dataname <- "gbif_filtered"
mdata_na$dataname <- "gbif_with_na"
inat$dataname <- "inat"

# Combine all into one dataframe (= 2067 obs.)
monarch_data <- rbind(mdata, mdata_na, inat)

# De-duplicate data (= 2008 obs.)
monarch_data <- unique(monarch_data, by = c("longitude", "latitude", 
                                            "year", "month", "day"))


##############################################################################
# Define UI (User Interface)
##############################################################################

ui <- dashboardPage(
  
  skin = "yellow",
  
  # Add header stuff
  dashboardHeader(title = "Arizona Monarch Occurrences from 2000-2022",
                  titleWidth = 500),
  
  # Add sidebar content
  dashboardSidebar(disable = TRUE),
  
  # Add content to main body of page
  dashboardBody(
              # Lines boxes up on dashboardBody
              fluidRow(
                # Box for widgets and notes
                box(width = 4, height = ,
                    status = "primary",
                    h3("AZ Monarch Occurrences"),
                    helpText("This map displays monarch occurrences",
                             "in Arizona, USA. Insert instructions."),
                    checkboxGroupInput(inputId = "checkbox",
                                       h3("Select data:"),
                                       choices = list("GBIF (exclude NA individualCount)" = 1,
                                                      "GBIF (NA individualCount)" = 2,
                                                      "iNaturalist" = 3)),
                    sliderInput(inputId = "date",
                                h3("Select month and year:"),
                                min = as.Date("2000-01-01"),
                                max = as.Date("2022-12-31"),
                                value = as.Date("2010-01-01"),
                                timeFormat="%b %Y"),
                    h3("About the data"),
                    helpText("The data used to generate this map",
                             "includes GBIF and iNaturalist data",
                             "that classified as research grade.",
                             "Insert other notes.")),
                # Box for map
                box(width = 8,
                    status = "primary",
                    tags$style(type = "text/css", 
                               "#map {height: calc(100vh - 80px) !important;}"),
                    leafletOutput("map")),
              )
      
    )
    
  )


##############################################################################
# Define server logic
##############################################################################

server <- function(input, output) {
  
  # Make data reactive
  reactive_data <- reactive({
    req(input$checkbox)
    monarch_data[monarch_data$dataname %in% input$checkbox, ]
    })
  
  # Generate map
  output$map <- renderLeaflet({ 
    leaflet() %>%
      # Set the default zoom for map (zooms to show Arizona)
      setView(lng = -111.682299, lat = 34.496789, zoom = 6.8) %>%
      # Add base layer
      addProviderTiles("Esri.WorldStreetMap") # %>%
      # Add data points to map
      # addCircles(
        # data = input$checkbox,
        # color = "#000000",
        # fillColor = "#ffffff",
        # fillOpacity = 0.5)
  })
  
  # leafletProxy() helps customize an already-rendered map
  map_proxy <- leafletProxy("map")
  
  observe({
    md <- monarch_data[monarch_data$dataname %in% input$checkbox, ]
    map_proxy %>%
      addCircles(lng = md$longitude, lat = md$latitude)
  })
  
}


##############################################################################
# Run the application 
##############################################################################

shinyApp(ui = ui, server = server)

