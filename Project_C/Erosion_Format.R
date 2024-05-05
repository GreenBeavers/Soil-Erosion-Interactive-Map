
# Title:      Import Soil Erosion Observations
# Objective:  Import csv of points, convert to sf point and buffer into polygons
# Created by: Pia Benaud
# Created on: 2024-01-20


# Load packages -----------------------------------------------------------

library(tidyverse) # wranglings
library(sf) # for dealing with vectors


# Import and format the data ----------------------------------------------

 # one option is to turn the point data into polygons - this is useful for extracting some information. Though it can make it challenging to visualise on the map due to scale.

Soil_Erosion <- read_csv("C:/Users/bs498/OneDrive - University of Exeter/GIS Group Work/Project_C/Data/Soil_Erosion_Data.csv") %>% # read in the csv
  st_as_sf(., coords = c("Easting", "Northing"), crs = 27700) %>%  # convert to sf object. note BNG
  st_buffer(., dist = 250) %>%   # put a 250 m buffer around each point
  st_jitter(., 100)  %>% # there is some overlap between points, to make sure they are all visible, we can slightly alter their coordinates i.e. randomly within 100m
  st_transform(., 4326) # convert to wgs84 for the main map



