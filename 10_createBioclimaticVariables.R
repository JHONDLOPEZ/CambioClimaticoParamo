
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, dismo)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
root <- '../raster/climate/worldclim/baseline/1ha'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')
vars <- c(paste0('prec_', 1:12), paste0('tmax_', 1:12), paste0('tmin_', 1:12))
vars <- paste0(vars, '.tif')
fles <- grep(paste0(vars, collapse = '|'), fles, value = TRUE)

# Read the raster files like a stack
prec <- grep('prec_', fles, value = TRUE) %>% stack()
tmax <- grep('tmax_', fles, value = TRUE) %>% stack()
tmin <- grep('tmin_', fles, value = TRUE) %>% stack()

# To calculate the stack of the files
bioclim <- dismo::biovars(prec, tmin, tmax)

# Simple plots
plot(bioclim[[1]])
plot(bioclim[[12]])
plot(bioclim[[16]])

# To write the final bioclimatic variables for the baseline
Map('writeRaster', x = unstack(bioclim), filename = paste0('../raster/climate/worldclim/baseline/1ha/bio_', 1:19, '.tif'))

