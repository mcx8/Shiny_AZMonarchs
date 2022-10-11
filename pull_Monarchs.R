# Maxine Cruz
# tmcruz@arizona.edu

# Created: September 23, 2022
# Last edited: October 10, 2022
# Version: RStudio 2021.09.0+351 "Ghost Orchid" Release

# Purpose:
  # Load libraries for downloading data
  # Download Danaus plexippus (Monarch) data from GBIF and iNaturalist
  # Clean data
  # Save data for use in R Shiny
  # Inspect data to determine settings in R Shiny

##############################################################################
# Load libraries
##############################################################################

library("spocc")
library("dplyr")
library("ggplot2")
library("rinat")
library("rnaturalearth")
library("lubridate")


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

# Combine all into one dataframe
monarch_data <- rbind(mdata, mdata_na, inat)


##############################################################################
# Save data
##############################################################################

# Save as .csv
write.csv(monarch_data, "AZ_Monarchs_map/data/monarch_data.csv")


##############################################################################
# Check for correctness
##############################################################################

# Sets up map stuff
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)

# Plot on map
# sample <- ggplot(data = world) +
#  geom_sf() +
#  coord_sf(xlim = c(-115.9, -108.15), ylim = c(30.5, 37.85)) +
#  geom_point(data = mdata,
#             aes(x = longitude, y = latitude),
#             position = position_jitter(width = 0.1, height = 0.1),
#             colour = "red") +
#  geom_point(data = inat,
#             aes(x = longitude, y = latitude),
#             position = position_jitter(width = 0.1, height = 0.1),
#             colour = "blue") +
#  borders("state") +
#  theme_bw()
  

