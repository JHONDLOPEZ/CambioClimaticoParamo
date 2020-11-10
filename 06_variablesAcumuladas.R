

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
aggregate_annual <- function(var, tpe){
  
  fle <- grep(var, fls, value = TRUE) 
  rst <- map(.x = fle, .f = raster)
  rst <- stack(rst)
  
  if(tpe == 'sum'){
    
    rsl <- sum(rst)
    
  } else if(tpe == 'mean') {
    
    rsl <- mean(rst)
    
  }
  
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
fls <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
fls <- mixedsort(fls)
vrs <- c('prec', 'tmax', 'tmean', 'tmin')
tps <- c('sum', 'mean', 'mean', 'mean')

# Apply the function ------------------------------------------------------
rst <- lapply(1:length(vrs), function(k){
  
  print(vrs[k])
  aggregate_annual(var = vrs[k], tpe = tps[k])
  
})

# To write the raster files -----------------------------------------------
writeRaster(rst[[1]],  '../raster/climate/worldclim/baseline/1ha/prec_total.tif')
writeRaster(rst[[2]],  '../raster/climate/worldclim/baseline/1ha/tmax_total.tif')
writeRaster(rst[[3]],  '../raster/climate/worldclim/baseline/1ha/tmean_total.tif')
writeRaster(rst[[4]],  '../raster/climate/worldclim/baseline/1ha/tmin_total.tif')


















