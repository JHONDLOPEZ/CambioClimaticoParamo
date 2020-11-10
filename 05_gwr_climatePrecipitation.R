

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf, RSAGA, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

 # Functions to use --------------------------------------------------------
downscaling <- function(fle){
  
  print('Processing...!')
  
  # fle <- fls[1]
  
  otp <- gsub('.tif', '', basename(fle))
  otp <- paste0(otp, '.tif')
  print(otp)
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = 'E:/asesorias/dario/workspace/gwr/raster/dem/dem_zone.tif',
                                         REGRESSION = paste0('E:/asesorias/dario/workspace/gwr/raster/current/1ha/', otp),
                                         DEPENDENT = fle),
                            env = env)
  
  print('Done!')
  return(rsl)
  
}

# Environment SAGA --------------------------------------------------------
env <- rsaga.env(path = 'C:/SAGA')

# List files temperature --------------------------------------------------
prec <- list.files('E:/asesorias/dario/raster/climate/worldclim/baseline/1km', full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() %>% 
  grep('pr', ., value = TRUE)

# Apply the function ------------------------------------------------------
map(.x = prec, .f = downscaling)


