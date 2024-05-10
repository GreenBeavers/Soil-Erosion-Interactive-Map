#################################################################
##           Practical 6 for GEOM184 - Open Source GIS         ##
##                      28/02/2024                             ##
##                  Creating a ShinyApp                        ##
##                         UI.R                                ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
#################################################################

library(ggiraph)

(
  div(class = "outer",
      leafletOutput("map", height = "calc(100vh - 70px)"), #this is needed for main map "map"; it adapts to vertical height minus 70 pixels
      
      absolutePanel(id = "sidebar", class = "panel panel-default", fixed = TRUE, #we use this to create a side bar with rainfall stations and plots
                    draggable = TRUE, top = 90, right = 30, left = "auto", bottom = "auto", #define the position of the side bar
                    width = 300, height = 300, #defines the size of the side bar
                    h3("Rainfall Stations"), #header
                    selectInput("station", "Rainfall Stations", choices = setNames(rainfall_stations$Stations, rainfall_stations$Stations)), #creates the dropdown menu
                    girafeOutput("rainfall_plot") #calls the ggiraph plot 
      )
  )
  
  
)
#(
#div(class="outer",
    
    #tags$head(
      #includeCSS("styles.css") # Include our custom CSS
    #),
    
    # If not using custom CSS, set height of leafletOutput to a number instead of percent
    #leafletOutput("map", width="100%", height="100%"),
    
    
    #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  #draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  #width = 330, height = "auto",
                  #
                  #h3("Observation Explorer"),
                  
                  #selectInput("point_colour", "Point Colour Coding", point_vars),
                  
                  #selectInput("monitoring_colour", "Monitoring Colour Coding", monitoring_vars),
                  
                  #h3("Land Cover"),
                  
                  #plotOutput("LC_barplot", height = 300),
                  
                  
    #)
#)
#)
