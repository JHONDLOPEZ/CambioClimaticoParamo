
# Load libraries --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data -------------------------------------------------------------
dem <- raster('../raster/srtm/90m/dem_zone.tif')
fls <- list.files('../raster/climate/worldclim/baseline/1km', full.names = TRUE, pattern = '.tif$')
fls <- mixedsort(fls)
ppt <- grep('prec', fls, value = TRUE)

prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
geo <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Project the raster files ----------------------------------------------
dem.prj <- projectRaster(dem, crs = prj)
trn <- terrain(dem.prj, opt = c('slope', 'aspect', 'TPI', 'TRI', 'roughness'), unit = 'degrees')
trn.geo <- projectRaster(trn, crs = geo)

# Write the terrain rasters ---------------------------------------------
Map('writeRaster', x = unstack(trn.geo), 
    filename = paste0('../raster/srtm/90m/', names(trn.geo), '.tif'))

