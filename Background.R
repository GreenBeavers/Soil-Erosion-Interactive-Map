
# Title:      Background Data
# Objective:  Make and extract the data that is going to be displayed in the app
# Created by: Pia Benaud
# Created on: 2024-01-04

# Install tidyverse, sf, terra, and rnaturalearth packages on Tools/Install packages ----------

# Load packages -----------------------------------------------------------

library(tidyverse) # default import for general wranglings and ggplot etc
library(sf) # for dealing with vector data
library(terra) # for dealing with raster data

library(rnaturalearth) # contains vector country and other administrative boundaries

# When library(tidyverse)it will ask you to deal with conflicts. 
# To fix this I did install.package("conflicted") then library (conflicted)
# Followed by conflict_prefer("filter", "dplyr") and conflict_prefer("lag", "dplyr")

# Import a UK boundary ----------------------------------------------------

UK <- ne_countries(country = 'united kingdom', scale='large', returnclass='sf') %>%
  st_transform(crs = 4326) %>% 
  select(name, geometry)

# May ask you to install rnaturalearthhires. If it does do install.packages("devtools") then library(devtools)
# Then install package by doing devtools::install_github("ropensci/rnaturalearthhires") and type 1 to update all packages

#plot(UK)


# Make a grid of points within the UK -------------------------------------

UK_Grid <- st_make_grid(UK, 
                        cellsize = c(25000, 25000), #25km by 25km
                        what = "centers") %>% 
  st_as_sf() %>%  # convert to sf object
  st_filter(., UK) # restrict points to within the UK

# ggplot()+
#   geom_sf(data = UK)+
#   geom_sf(data = UK_Grid)


# Import Landcover Data ---------------------------------------------------

UKCEH_LC_2021 <- rast("C:/Users/bs498/OneDrive - University of Exeter/GIS group project/Project_C/LandCover2021_WG84.tif")
 # Marston, C., Rowland, C. S., O’Neil, A. W., & Morton, R. D. (2022). Land Cover Map 2021 (10m classified pixels, GB) [Data set]. 
 # NERC EDS Environmental Information Data Centre. https://doi.org/10.5285/A22BAA7C-5809-4A02-87E0-3CF87D4E223A

# Change the file pathway to where you have downloaded the land cover tif file 

# Extract landcover values ------------------------------------------------

# Read the CSV file into a data frame
main_database_WGS <- read.csv("main_database_WGS.csv")

# Assuming the first two columns are Easting and Northing
# Convert it to sf object with appropriate coordinate reference system (CRS)
# Replace "+init=epsg:xxxx" with the appropriate EPSG code for your coordinate system
main_database_WGS <- st_as_sf(main_database_WGS, coords = c("Easting", "Northing"), crs = 4326) 


Points_LC <- extract(UKCEH_LC_2021, main_database_WGS, bind = TRUE, na.rm = TRUE) %>% 
  st_as_sf(.) %>%  # change back to sf object...
  rename(Class = LandCover2021_WG84_1, # rename to something more user friendly
         Probability = LandCover2021_WG84_2)

# Assuming main_database_WGS is loaded as a spatial object (sf) containing soil erosion sites

# Extract land cover values for each soil erosion site



#Conflict error appears. I did conflicts_prefer(terra::extract)

# Give proper names to landcover classes ----------------------------------

LC_table <- tibble("Name" = c("Deciduous woodland", 
                              "Coniferous woodland",
                              "Arable",
                              "Improved grassland",
                              "Neutral grassland",
                              "Calcareous grassland",
                              "Acid grassland",
                              "Fen",
                              "Heather",
                              "Heather grassland",
                              "Bog",
                              "Inland rock",
                              "Saltwater",
                              "Freshwater",
                              "Supralittoral rock",
                              "Supralittoral sediment",
                              "Littoral rock",
                              "Littoral sediment",
                              "Saltmarsh",
                              "Urban",
                              "Suburban"),
                   "Number" = 1:21)


Points_LC_Named <- left_join(Points_LC, LC_table, by = c("Class" = "Number")) %>% 
  filter(., !is.na(Class)) # LC map doesn't include NI


# Export for further use --------------------------------------------------

st_write(Points_LC_Named, "C:/Users/bs498/OneDrive - University of Exeter/GIS group project/Project_C/Data/UK_LC_Grid_SoilErosion1.gpkg")

# Make sure to change file path to where you want it to go