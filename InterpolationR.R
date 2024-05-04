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

