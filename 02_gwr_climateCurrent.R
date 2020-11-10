
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf, RSAGA, gtools)

# Function to use ---------------------------------------------------------
downscaling <- function(fle){
  
  print('Processing...!')
  fle <- temp[1]
  
  root.dem <- '..raster/srtm/90m/'
  root.tmp <- '../raster/climate/worldclim/baseline/1ha/'
  
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = paste0(root.dem, 'dem_zone.tif'),
                                         REGRESSION = paste0(root.tmp, basename(fle)),
                                         DEPENDENT = fle),
                            intern = TRUE,
                            display.command = TRUE,
                            env = env)
  
  print('Done!')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
zne <- shapefile('../shapefiles/base/mpios_zone.shp')
zne@data$gid <- 1

lim <- aggregate(zne, 'gid')
dem <- raster('../raster/srtm/90m/dem_zone.tif')

# Environment SAGA
env <- rsaga.env(path = 'C:/SAGA')

# List files temperature
temp <- list.files('E:/asesorias/dario/raster/climate/worldclim/baseline/1km', full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() %>% 
  grep('tm', ., value = TRUE)

# Making the downscaling --------------------------------------------------
for(i in 2:length(temp)){
  
  print(temp[i])
  downscaling(fle = temp[i])
  print('Done!')
    
}

fls <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.sdat$')
raster(fls[1])
testing <- read.sgrd(fls[1])

fle <- raster('E:/asesorias/diana/data/raster/climate/worldclim/baseline/tmax_1.sdat')
