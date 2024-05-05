
# Title:      Background Data
# Objective:  Make and extract the data that is going to be displayed in the app
# Created by: Pia Benaud
# Created on: 2024-01-04


# Load packages -----------------------------------------------------------

library(tidyverse) # default import for general wranglings and ggplot etc
library(sf) # for dealing with vector data
library(terra) # for dealing with raster data
library(conflicted)
library(rnaturalearth) # contains vector country and other administrative boundaries


# Import a UK boundary ----------------------------------------------------

UK <- ne_countries(country = 'united kingdom', scale='large', returnclass='sf') %>%
  st_transform(crs = 27700) %>% 
  select(name, geometry)

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

UKCEH_LC_2021 <- rast("C:/Users/bs498/OneDrive - University of Exeter/GIS Group Work/Project_C/Data/gblcm10m2021.tif")
 # Marston, C., Rowland, C. S., O’Neil, A. W., & Morton, R. D. (2022). Land Cover Map 2021 (10m classified pixels, GB) [Data set]. 
 # NERC EDS Environmental Information Data Centre. https://doi.org/10.5285/A22BAA7C-5809-4A02-87E0-3CF87D4E223A

# Import Soil Erosion Sites
Soil_Erosion_Data <- read.csv("C:/Users/bs498/OneDrive - University of Exeter/GIS Group Work/Project_C/Data/Soil_Erosion_Data.csv")

# Extract landcover values ------------------------------------------------

Points_LC <- extract(UKCEH_LC_2021, Soil_Erosion_Data[, 1:2], bind = TRUE, na.rm = TRUE) %>% 
  st_as_sf(.) %>%  # change back to sf object...
  rename(Class = gblcm10m2021_1, # rename to something more user friendly
         Probability = gblcm10m2021_2)


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

st_write(Points_LC_Named, "C:/Users/bs498/OneDrive - University of Exeter/GIS Group Work/Project_C/Data/UK_LC_Grid_SoilErosion.gpkg")

