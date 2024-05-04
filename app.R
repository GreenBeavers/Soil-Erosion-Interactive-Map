
# Title:      Shiny Map
# Objective:  An example interactive map made using {shiny}
# Created by: Pia Benaud
# Created on: 2024-01-05

# At start, install tidyverse, then conflicted and set preferences, install shiny, rnaturalearthh and leaflet too 
# Do conflicted before tidyverse, conflicts go away

# Load packages -----------------------------------------------------------

library(shiny)
library(leaflet)
library(tidyverse)
library(conflicted)
library(sf) # for dealing with vectors
library(raster)


# Run global script -------------------------------------------------------

source("Global.R")

# The User Interface ------------------------------------------------------

ui <- navbarPage("SoilErosionMap", id = 'nav',
                 tabPanel("The Map", source("UI.R", local = TRUE)),
                 tabPanel("UK Rainfall Data", source("Rainfall.UI.R", local = TRUE))
)

# The Server --------------------------------------------------------------

server <- function(input, output, session){
  source("Server.R", local = TRUE)
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -2.141, lat = 54.648, zoom = 6) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Colour") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(opacity = 0.3), group = "Colour") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray-scale") %>%
      addMarkers(data = rainfall_stations,  # Replace `rainfall_stations_sf` with your rainfall data
                 popup = ~paste("<b>Station:</b>", rainfall_stations$Stations, "<br>",
                                "<b>Longitude:</b>", rainfall_stations$Longitude, "<br>",
                                "<b>Latitude:</b>", rainfall_stations$Latitude, "<br>",
                                "<b>Avg. Annual Rainfall:</b>", rainfall_stations_sf$avg_annual))  # Customize popup info
  })
}


# Run the App! ------------------------------------------------------------

shinyApp(ui = ui, server = server)
