
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, sf)

# Load data ---------------------------------------------------------------
zne <- shapefile('../shapefiles/base/mpios_zone.shp')
zne@data$gid <- 1

lim <- aggregate(zne, 'gid')

# Extracting the centroid -------------------------------------------------
cnt <- coordinates(lim) 
cnt <- as.data.frame(cnt)

# Download the current climate data ---------------------------------------
ppt <- raster::getData(name = 'worldclim', var = 'prec', res = 0.5, lon = cnt$V1, lat = cnt$V2)
tmx <- raster::getData(name = 'worldclim', var = 'tmax', res = 0.5, lon = cnt$V1, lat = cnt$V2)
tmn <- raster::getData(name = 'worldclim', var = 'tmin', res = 0.5, lon = cnt$V1, lat = cnt$V2)
tav <- raster::getData(name = 'worldclim', var = 'tmean', res = 0.5, lon = cnt$V1, lat = cnt$V2)

# Extract by mask ---------------------------------------------------------
ppt_cut <- raster::crop(ppt, lim)
ppt_cut <- raster::mask(ppt_cut, lim)

tmx_cut <- raster::crop(tmx, lim)
tmx_cut <- raster::mask(tmx_cut, lim)

tmn_cut <- raster::crop(tmn, lim)
tmn_cut <- raster::mask(tmn_cut, lim)

tav_cut <- raster::crop(tav, lim)
tav_cut <- raster::mask(tav_cut, lim)

# Write the final raster --------------------------------------------------
dir.create('../raster/climate/worldclim/baseline', recursive = TRUE)

Map('writeRaster', x = unstack(ppt_cut), filename = paste0('../raster/climate/worldclim/baseline/prec_', 1:12, '.tif'))
Map('writeRaster', x = unstack(tmx_cut), filename = paste0('../raster/climate/worldclim/baseline/tmax_', 1:12, '.tif'))
Map('writeRaster', x = unstack(tmn_cut), filename = paste0('../raster/climate/worldclim/baseline/tmin_', 1:12, '.tif'))
Map('writeRaster', x = unstack(tav_cut), filename = paste0('../raster/climate/worldclim/baseline/tmean_', 1:12, '.tif'))

# Download the Digital Elevation Model ------------------------------------
cnt <- cnt %>% setNames(c('lon', 'lat')) # Cambiar encabezado de las tablas (nombres de las columnas)
dem <- raster::getData(name = 'SRTM', lon = cnt$lon, lat = cnt$lat)

# Extract by mask DEM
dem <- raster::crop(dem, lim)
dem <- raster::mask(dem, lim)
dir.create('../raster/srtm/90m', recursive = TRUE)
writeRaster(x = dem, filename = '../raster/srtm/90m/dem_zone.tif', overwrite = TRUE)



