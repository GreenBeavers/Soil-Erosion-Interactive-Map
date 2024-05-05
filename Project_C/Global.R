
# Title:      Global script
# Objective:  Loads the necessary data and provides 
# Created by: Pia Benaud
# Created on: 


# Load packages -----------------------------------------------------------

library(sf) # for dealing with vectors
library(raster) #For the UK DTM


# Load the shapefile ------------------------------------------------------

LC_Grid <- st_read("Data/UK_LC_Grid_SoilErosion.gpkg")


# Transform to WGS84 ------------------------------------------------------

#LC_Grid has an undefined CRS so first we assign it to British National Grid before converting it
st_crs(LC_Grid) <- st_crs(27700)


 # leaflet runs in WGS84 so will need to convert anything that is OSGB
LC_Grid_WGS84 <- st_transform(LC_Grid, 4326)

rm(LC_Grid) # remove this as we don't need a BNG version

#Read Rainfall.csv file
Interpolated_Rainfall_Data <- read.csv("Data/Interpolation.csv")

#Read the UK DTM

DEM <- raster("Data/Reprojected_Rasterized_DEM.tif")
DTM_df <- as.data.frame(rasterToPoints(DEM))

#Extract elevation values from the DTM
elevation_values <- DTM_df$Reprojected_Rasterized_DEM

# Point colour options ----------------------------------------------------

# this is here because it is needed in both the UI and server

point_vars <- c("Default" = "Default",
                  "Land Cover" = "Class",
                "Rainfall" = "Rainfall",
                "Elevation" = "DTM")


# Make a consistent colour palette ----------------------------------------
# library(viridis)
#sample(viridis(21)) # lets get some colours

unique_classes <- unique(LC_Grid_WGS84$Class)

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

class_color_mapping <- setNames(colour_pal, unique_classes)

