# Maxine Cruz
# tmcruz@arizona.edu
# Created: September 23, 2022
# Last edited: November 20, 2022
# Version: RStudio 2021.09.0+351 "Ghost Orchid" Release

# Purpose:
  # Generates an R Shiny app for AZ Monarch data from GBIF and iNaturalist
  # Interactive - shows and plots available data by month and year on map

##############################################################################
# Load libraries
##############################################################################

# For app
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinythemes)

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

ui <- navbarPage(
  
  # Settings for header and theme
  title = "Arizona Monarch Occurrences from 2000-2022",
  theme = shinytheme("sandstone"),
  
  # Add sidebar content (disabled in this case)
  dashboardSidebar(disable = TRUE),
  
  # Add content to main body of page
  dashboardBody(
    
              # Lines boxes up on dashboardBody()
              fluidRow(
                
                # Box for widgets and notes
                box(width = 4,
                    
                    # Add title
                    h3(HTML("<b>Mapping AZ Monarch Occurrences</b>")),
                    
                    # Some text
                    tags$hr(),
                    helpText(HTML("This map displays Monarch <em>(Danaus plexippus)</em>",
                             "occurrences in Arizona, USA. Please select data",
                             "and date to be displayed on the interactive map.",
                             "The months span over the years 2000-2022.",
                             "When selecting the data, GBIF individualCount is",
                             "the number of observed individuals at that site.",
                             "The code used to create this site can be found at:",
                             "<a>https://github.com/mcx8/Shiny_AZMonarchs</a>")),
                    tags$hr(),
                    
                    # Controls for selecting data
                    checkboxGroupInput(inputId = "checkbox",
                                       h4(HTML("<b>Select data:</b>")),
                                       choices = list("GBIF (exclude NA individualCount)" = 1,
                                                      "GBIF (NA individualCount)" = 2,
                                                      "iNaturalist" = 3),
                                       selected = c(1, 2, 3)),
                    
                    # Controls to filter for month
                    selectInput(inputId = "date",
                                h4(HTML("<b>Select month:</b>")),
                                choices = list("January" = 1,
                                               "February" = 2,
                                               "March" = 3,
                                               "April" = 4,
                                               "May" = 5,
                                               "June" = 6,
                                               "July" = 7,
                                               "August" = 8,
                                               "September" = 9,
                                               "October" = 10,
                                               "November" = 11,
                                               "December" = 12),
                                selected = 1),
                    # setSliderColor("CornflowerBlue", sliderId = 1),
                    # chooseSliderSkin("Flat"),
                    # sliderInput(inputId = "date",
                    #            h4(HTML("<b>Select month:</b>")),
                    #            min = as.Date("2000-01-01"),
                    #            max = as.Date("2022-12-31"),
                    #            value = as.Date("2016-09-01"),
                    #            timeFormat="%b"),
                    
                    # Add text
                    tags$hr(),
                    h4(HTML("<b>About the data</b>")),
                    helpText(HTML("The data used to generate this map includes",
                             "data that classified as research grade from the",
                             "Global Biodiversity Information Facility (GBIF)",
                             "(<a>https://www.gbif.org/</a>) and iNaturalist",
                             "(<a>https://www.inaturalist.org/</a>). Duplicates",
                             "were removed from the data before plotting."))
                    ),
                
                # Box with tabbed options for visuals
                tabBox(
                  title = NULL,
                  width = 8,
                  
                  # Tab for map
                  tabPanel(
                    title = "Map",
                    tags$style(type = "text/css",
                               "#map {height: calc(100vh - 80px) !important;}"),
                    leafletOutput("map")
                    ),
                  
                  # Tab for plot
                  tabPanel(
                    title = "Observation Count",
                    plotOutput("plot"))
                    ),
              )
    )
    
  )


##############################################################################
# Define server logic
##############################################################################

server <- function(input, output) {
  
  # Generate map
  output$map <- renderLeaflet({ 
    
    # Select data of interest
    
    # Part 1: use local variable "datanames" to filter on column of same name 
    # in monarch_data
    datanames <- ""
    if (1 %in% input$checkbox) {
      datanames <- c(datanames, "gbif_filtered")
    }    
    if (2 %in% input$checkbox) {
      datanames <- c(datanames, "gbif_with_na")
    } 
    if (3 %in% input$checkbox) {
      datanames <- c(datanames, "inat")
    }
    plot_data <- monarch_data %>%
      filter(dataname %in% datanames)
    # Part 2: further filter plot_data by month
    plot_data <- plot_data %>%
      filter(month == input$date)
    
    # Settings for base map
    leaflet() %>%
      # Set the default zoom for map (zooms to show Arizona)
      setView(lng = -111.682299, lat = 34.296789, zoom = 6.8) %>%
      # Add base layer
      addProviderTiles("Esri.WorldStreetMap") %>%
      # Add data points to map
      addCircles(
        data = plot_data,
        color = "#000000",
        fillColor = "#ffffff",
        fillOpacity = 0.5)
  })
  
  # Generate plot
  output$plot <- renderPlot({
    # In progress
  })
  
}


##############################################################################
# Run the application 
##############################################################################

shinyApp(ui = ui, server = server)

