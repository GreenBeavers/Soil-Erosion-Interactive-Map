
# Title:      Shiny Map
# Objective:  An example interactive map made using {shiny}
# Created by: Pia Benaud
# Created on: 2024-01-05


# Load packages -----------------------------------------------------------

library(shiny)
library(leaflet)
library(tidyverse)

library(sf) # for dealing with vectors


# Run global script -------------------------------------------------------

source("Global.R")


# The User Interface ------------------------------------------------------

ui <- navbarPage("SoilErosionMap", id = 'nav',
                 tabPanel("The Map", source("UI.R", local = TRUE))
)


# The Server --------------------------------------------------------------

server <- function(input, output, session){
  source("Server.R", local = TRUE)
}


# Run the App! ------------------------------------------------------------

shinyApp(ui = ui, server = server)
