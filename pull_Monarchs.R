# Maxine Cruz
# tmcruz@arizona.edu

# Created: September 23, 2022
# Last edited: September 23, 2022
# Version: RStudio 2021.09.0+351 "Ghost Orchid" Release

# Purpose:
  # Load libraries for downloading data
  # Download Danaus plexippus (Monarch) data from GBIF
  # Clean data for use in R Shiny

##############################################################################
# Load libraries
##############################################################################

library("spocc")
library("rgbif")


##############################################################################
# Collect occurrence data from GBIF
##############################################################################

# This takes a very long time, would be good to find a faster way
D_plex <- occ_data(taxonKey = 5133088, 
                   country = "US",
                   limit = 500000)

# Records found [489570] 
# Records returned [500] 
# Therefore, we change the limit to 500000 to account for all observations


##############################################################################
# Clean data
##############################################################################

# Filter to years 2000-2022


# Filter to observations in Arizona


# Remove duplicates


