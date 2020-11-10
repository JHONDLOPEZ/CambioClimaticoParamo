
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data
ecst <- st_read('../shapefiles/ecosistemas/ecosistemas_mpios.shp')
typs <- unique(ecst$ECOS_SINTE)
mask <- raster('../raster/climate/worldclim/baseline/1ha/prec_1.tif')

# Filter paramos
prms <- filter(ecst, ECOS_SINTE == 'Paramo')

# Write paramos shapefile
st_write(obj = prms, dsn = '../shapefiles/ecosistemas', layer = 'paramos_mpios', driver = 'ESRI Shapefile')

# Create a mask
mask <- mask * 0 + 1

# Shapefile to raster
prms <- prms %>% mutate(gid = 1)
prms <- prms %>% group_by(gid) %>% dplyr::summarise(count = n()) %>% ungroup()
prms <- as(prms, 'Spatial')
prms.rst <- raster::rasterize(prms, mask, field = 'gid')
prms.rst <- raster::crop(prms.rst, prms) %>% raster::mask(., prms)

dir.create('../raster/paramos')
writeRaster(prms.rst, filename = '../raster/paramos/paramos_mask.tif')

# Shapefile to table
prms.tbl <- rasterToPoints(prms.rst, spatial = FALSE)
prms.tbl <- as.data.frame(prms.tbl)
prms.tbl <- as_tibble(prms.tbl)

# Taking a sample
pnts <- sample_n(tbl = prms.tbl, size = round(nrow(prms.tbl) * 0.2, 0), replace = FALSE)
points(pnts$x, pnts$y, pch = 16, col = 'red')

# Write the tables
dir.create('../tables/paramos')
write.csv(pnts, '../tables/paramos/paramos_sample.csv', row.names = FALSE)




