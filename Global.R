
# Title:      Global script
# Objective:  Loads the necessary data and provides 
# Created by: Pia Benaud
# Created on: 


# Load packages -----------------------------------------------------------

library(sf) # for dealing with vectors


# Load the shapefile. Make sure to change file path again ------------------------------------------------------

LC_Grid <- st_read("C:/Users/bs498/OneDrive - University of Exeter/GIS group project/Project_C/Data/UK_LC_Grid_SoilErosion1.gpkg")


# Transform to WGS84 ------------------------------------------------------

 # leaflet runs in WGS84 so will need to convert anything that is OSGB
LC_Grid_WGS84 <- st_transform(LC_Grid, 4326)

rm(LC_Grid) # remove this as we don't need a BNG version


# Point colour options ----------------------------------------------------

# this is here because it is needed in both the UI and server

point_vars <- c("Default" = "Default",
                  "Land Cover" = "Class")
  

# Make a consistent colour palette ----------------------------------------
# library(viridis)
# sample(viridis(21)) # lets get some colours

colour_pal <- c("Deciduous woodland" = "#21908CFF", 
                "Coniferous woodland" = "#2A788EFF",
                "Arable" = "#440154FF",
                "Improved grassland" = "#43BF71FF",
                "Neutral grassland" = "#35608DFF",
                "Calcareous grassland" = "#5DC863FF",
                "Acid grassland" = "#3B528BFF",
                "Fen" = "#2FB47CFF",
                "Heather" = "#463480FF",
                "Heather grassland" = "#DEE318FF",
                "Bog" = "#7AD151FF",
                "Inland rock" = "#BBDF27FF",
                "Saltwater" = "#482576FF",
                "Freshwater" = "#9AD93CFF",
                "Supralittoral rock" = "#481467FF",
                "Supralittoral sediment" = "#FDE725FF",
                "Littoral rock" = "#414487FF",
                "Littoral sediment" = "#25848EFF",
                "Saltmarsh" = "#22A884FF",
                "Urban" = "#2F6C8EFF",
                "Suburban" = "#1E9C89FF")



