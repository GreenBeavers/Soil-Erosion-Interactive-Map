
# Title:      The map UI
# Objective:  The UI script defines the ... User Interface
# Created by: Pia Benaud
# Created on: 2024-01-05


# The code ----------------------------------------------------------------

(
  div(class="outer",
      
      tags$head(
        includeCSS("styles.css") # Include our custom CSS
      ),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h3("Observation Explorer"),
                    
                    selectInput("point_colour", "Point Colour Coding", point_vars),
                    
                    #selectInput("monitoring_colour", "Monitoring Colour Coding", monitoring_vars),
                    
                    #h3("Land Cover"),
                    
                    plotOutput("LC_barplot", height = 300),
                    
                    
      )
  )
)



