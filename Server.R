
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

library(ggplot2)
library(maps)
library(sf)
library(sp)
library(raster)
library(terra)
library(gridExtra)
library(dplyr)
library(sfheaders)
library(stars)
library(gstat)
library(automap)
library(rnaturalearth)
library(ggspatial)
library(automap)
library(tmap)
library(tidyverse)
library(remotes)


#---- Add the DTM
#DTM10 <- raster(Rasterized_DEM.tif)
# Load raster data
DEM <- raster("Reprojected_Rasterized_DEM.tif")
DTM_df <- as.data.frame(rasterToPoints(DEM))

# Convert raster to points
DEM_df <- rasterToPoints(DEM)

# Convert points to SpatialPointsDataFrame if needed
DEM_spdf <- SpatialPointsDataFrame(DEM_df[,1:2], data = data.frame(z = DEM_df[,3]))

# Plot DEM
plot(DEM, main = "UK DTM")

##Add rainfall stations ----
## read csv ----
rainfall_stations <- read.csv("RainfallStations.csv") 
##convert to sf object
rainfall_stations_sf <- st_as_sf(rainfall_stations, coords = c('Longitude', 'Latitude'), crs = 4326)
##Change CRS 

# 1.3 Embed rainfall data in your environment ----

### 1.3.1 Load rainfall data using a loop ----

# Create an empty list to store data frames for each station
rainfall_data <- list()
# Iterate over each row of the dataframe
for (i in 1:nrow(rainfall_stations_sf)) {
  # Get the station name from each row
  station_name <- rainfall_stations_sf$Station[i]
  
  # Construct the filename based on the station name and the sub-folder
  filename <- paste0("Rainfall/", station_name, ".csv")
  
  # Check if the file exists
  if (file.exists(filename)) {
    # If the file exists, load it into R
    data <- read.csv(filename)
    
    # Store the data frame in the rainfall_data list
    rainfall_data[[station_name]] <- data
    
    
    # For demonstration, let's print the first few rows of the loaded data
    cat("Data loaded for station:", station_name, "\n")
    
  } else {
    cat("File not found for station:", station_name, "\n")
  }
}
rm(data)

### 1.3.2 Calculate average annual rainfall for each station ----

# Calculate average annual rainfall for each station
for (station_name in names(rainfall_data)) {
  # Get daily rainfall values for the station
  daily_rainfall <- rainfall_data[[station_name]]$value
  
  # Calculate average annual rainfall by taking mean of daily values and multiplying by 365
  avg_annual_rainfall <- mean(daily_rainfall, na.rm=TRUE) * 365
  
  # Update rainfall_stations_sf with new column for average annual rainfall
  idx <- rainfall_stations_sf$Station == station_name
  rainfall_stations_sf$avg_annual[idx] <- avg_annual_rainfall
}
rm(daily_rainfall)

### 1.3.3 Plot rainfall in a world map ----
# Load UK map
uk_sf <- ne_states(country = "united kingdom", returnclass = "sf")
uk_sf <- st_transform(uk_sf, st_crs(rainfall_stations_sf))

#Set limits
x_lim <- c(ext(rainfall_stations_sf)[1]*0.95,ext(rainfall_stations_sf)[2]*1.05)
y_lim <- c(ext(rainfall_stations_sf)[3]*0.95,ext(rainfall_stations_sf)[4]*1.05)

# Plot a map with the rain stations and their annual mean

ggplot() +
  geom_sf(data = uk_sf) +
  geom_point(data = rainfall_stations_sf, aes(x = st_coordinates(rainfall_stations_sf)[, "X"],
                                              y = st_coordinates(rainfall_stations_sf)[, "Y"],
                                              fill = avg_annual,), size = 4, shape = 22,) +
  geom_text(data = rainfall_stations_sf, aes(x = st_coordinates(rainfall_stations_sf)[, "X"],
                                             y = st_coordinates(rainfall_stations_sf)[, "Y"],
                                             label = Stations),
            size = 3, vjust = -0.5) +
  scale_color_gradient(name = "Average Annual Rainfall",
                       na.value = "grey",
                       low = "skyblue",
                       high = "navy",
                       limits = c(floor(min(rainfall_stations_sf$avg_annual, na.rm = TRUE) / 100) * 100,
                                  ceiling(max(rainfall_stations_sf$avg_annual, na.rm = TRUE) / 100) * 100),
                       breaks = seq(floor(min(rainfall_stations_sf$avg_annual, na.rm = TRUE) / 100) * 100,
                                    ceiling(max(rainfall_stations_sf$avg_annual, na.rm = TRUE) / 100) * 100,
                                    by = 500),
                       labels = scales::comma) +
  theme_minimal() +
  coord_sf(xlim = c(-10, 3), ylim = y_lim) +
  labs(x = "Longitude", y = "Latitude") +
  ggspatial::annotation_scale(location = "tr", bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true",
                                    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(
                                      fill = c("black", "white"),
                                      line_col = "grey20"
                                    )) +
  theme(legend.position = "right")


#Interpolation code
packages = c('sf', 'sp', 'raster', 'gstat', 
             'automap','tmap', 'tidyverse')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
library(st)
library(sf)
library(raster)


# Read the UK shapefile
sg <- st_read(dsn = "GBR_adm0.shx",
              layer = "GBR_adm0", 
              crs = 4326)

# Read the rainfall data from CSV file
rainfall <- read.csv("Interpolation.csv")

# Convert rainfall data to sf object
rainfall_sf <- st_as_sf(rainfall, 
                        coords = c("Longitude", "Latitude"),
                        crs = 4326) %>%
  st_transform(crs = st_crs(sg))  # Transform to match the CRS of sg

sg_sp <- as_Spatial(sg)
rainfall_sp <- rainfall_sf %>%
  as_Spatial()

rainfall_sp@bbox <- sg_sp@bbox

grd <- as.data.frame(spsample(rainfall_sp, "regular", n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
# Add P's projection information to the empty grid
proj4string(rainfall_sp) <- proj4string(rainfall_sp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(rainfall_sp)

plot(grd)

P.idw <- gstat::idw(TMR ~ 1, rainfall_sp, newdata=grd, idp=2.0)
r <- raster(P.idw)
r.m <- mask(r, sg_sp)


tm_shape(r.m) +
  tm_raster(n=10,
            palette = "Blues",
            midpoint = TRUE,
            title="Predicted precipitation \n(in mm)") +
  tm_shape(rainfall_sp) + 
  tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)

#Load the DEM raster
#dem <- raster("Reprojected_Rasterized_DEM.tif")


  # Render the Leaflet map
  #output$map <- renderLeaflet({
    #leaflet() %>%
      #setView(lng = -2.141, lat = 54.648, zoom = 6) %>%
      #addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Colour") %>%
      #addProviderTiles(providers$Esri.WorldImagery,
                       #options = providerTileOptions(opacity = 0.3), group = "Colour") %>%
      #addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray-scale") %>%
      #addRasterImage(dem, opacity = 0.5)  # Add the DEM raster to the map
  #})
 

# Let's add a popup -------------------------------------------------------

popup_g <- paste0("<b>","Land Cover: ","</b>", LC_Grid_WGS84$Name, "<br>",
                  "<b>","County: ","</b>", LC_Grid_WGS84$County_Dis, "<br>",
                  "<b>","Method: ","</b>", LC_Grid_WGS84$Stdy_Meth1, "<br>",
                  "<b>","Scale: ","</b>", LC_Grid_WGS84$Stdy_Scale, "<br>",
                  "<b>","Erosion rate: Mean: ","</b>", LC_Grid_WGS84$Rslt_Mean, "<br>",
                  "<b>","Erosion rate: Median: ","</b>", LC_Grid_WGS84$Rslt_Med, "<br>",
                  "<b>","Erosion rate: Net: ","</b>", LC_Grid_WGS84$Rslt_Net, "<br>",
                  "<b>","Erosion volume: ","</b>", LC_Grid_WGS84$Rslt_Vol, "<br>")


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
colour_group <- LC_Grid_WGS84$Class
pal1 <- colorFactor(palette = colour_pal, domain = colour_group)

observe({
  
  colour_by <- input$point_colour # for the input (on ui) 'point_colour'

  if (colour_by == "Class") {
    colour_group <- LC_Grid_WGS84$Class
    pal1 <- colorFactor(palette = colour_pal, domain = colour_group)
  }
})
 
  
  
  
  leafletProxy("map") %>%  # Then adding it all to the map
    clearShapes() %>%
    addCircleMarkers(data = LC_Grid_WGS84, group = "Locations", radius = 8, fillOpacity = 0.8, weight = 0.8, fillColor = ~pal1(LC_Grid_WGS84$Class), popup = popup_g) %>%
    addLayersControl(
      options = layersControlOptions(collapsed = FALSE),
      overlayGroups = c("Locations"),
      baseGroups = c("Colour", "Gray-scale"),
      position = "topleft") 
  
 
    
      





