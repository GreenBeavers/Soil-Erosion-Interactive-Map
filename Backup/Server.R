
# Title:      The map server
# Objective:  The server hosts all the components used to make the app
# Created by: Pia Benaud
# Created on: 2024-01-04


# Make the basemap --------------------------------------------------------

output$map <- renderLeaflet({
  leaflet() %>%  
    setView(lng = -2.141, lat = 54.648, zoom = 6) %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Colour") %>% # full list of basemaps 'names(providers)' or https://leaflet-extras.github.io/leaflet-providers/preview/index.html
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 0.3), group = "Colour") %>% 
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray-scale") 
})


# Let's add a popup -------------------------------------------------------

popup_g <- paste0("<b>","Land Cover: ","</b>", LC_Grid_WGS84$Name, "<br>",
                  "<b>","Study Start Date: ","</b>", Soil_Erosion$Stdy_Start, "<br>",
                  "<b>","Study End Date: ","</b>", Soil_Erosion$Stdy_Fin, "<br>",
                  "<b>","Study Method: ","</b>", Soil_Erosion$Stdy_Method, "<br>",
                  "<b>","Erosion rate: ","</b>", Soil_Erosion$Erosion_Rate, "<br>"
                  
)



# Let's filter the data based on the zoom ---------------------------------

# here we are creating a "reactive expression" which subsets the data based on those visible in the UI - zoom in or out and this changes

obsInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(LC_Grid_WGS84[FALSE,])
  
  bounds <- input$map_bounds
  ylims <- range(bounds$north, bounds$south)
  xlims <- range(bounds$east, bounds$west)
  
  bbox_coords <- tibble(x = xlims, y = ylims) %>% # make bounds into a tibble
    st_as_sf(coords = c("x", "y")) %>% # convert to sf points
    st_set_crs(4326) # set crs to WGS84
  
  bbox <- st_bbox(bbox_coords) %>% # make it into a sfc aka polygon
    st_as_sfc(.)
  
  st_filter(LC_Grid_WGS84, bbox) # filter to points within the bbox
 
  
})


# Now let's plot the visible observations ---------------------------------

output$LC_barplot <- renderPlot({
  if (nrow(obsInBounds()) == 0) # If zero observations are in view, don't plot
    return(NULL)
  
  ggplot(obsInBounds(), aes(Name)) +  
    geom_bar(colour= "gray20", aes(fill = Name)) + 
    scale_fill_manual(values = colour_pal) +
    labs(title = "Visible Land Cover Classes", x = "Land Cover", y = "Frequency") + 
    theme_classic() + 
    theme(axis.text.x=element_text(angle=90,hjust=1)) + # rotate the text
    theme(axis.title = element_text(size = 10.5, colour = "gray20"),
          plot.title = element_text(colour = "gray20", size = 12, face = "bold")) +
    theme(legend.position = "none")
  
})


# Now let's define the colour palettes and bring it all together ----------

observe({
  
  colour_by <- input$point_colour # for the input (on ui) 'point_colour'
 
  if (colour_by == "Class") {
  colour_group <- LC_Grid_WGS84$Name
    pal1 <- colorFactor(palette = class_color_mapping, domain = colour_group)
  } else if (colour_by == "Rainfall") {
    colour_group <- Interpolated_Rainfall_Data$TMR
    pal1 <- colorNumeric(palette = "Blues", domain = colour_group)
    popup_g <- paste("<b> Total Monthly Rainfall: </b>", Interpolated_Rainfall_Data$TMR)
  } else if (colour_by == "DTM") {
    colour_group <- elevation_values
    pal1 <- colorNumeric(palette = "terrain.colors", domain = colour_group)
    popup_g <- paste("<b> Elevation (m): </b>", DTM_df$Reprojected_Rasterized_DEM)
  } else {
    colour_group <- LC_Grid_WGS84$Name
    pal1 <- colorFactor(palette = "#d95f02", domain = colour_group)
  }
  
  
  
  leafletProxy("map") %>%  # Then adding it all to the map
    clearShapes() %>%
    addCircleMarkers(data = LC_Grid_WGS84, group = "Locations", radius = 8, fillOpacity = 0.8, weight = 0.8, color = pal1(colour_group), popup = popup_g) %>%
    addLayersControl(
      options = layersControlOptions(collapsed = FALSE),
      overlayGroups = c("Locations"),
      baseGroups = c("Colour", "Gray-scale"),
      position = "topleft") 

})




