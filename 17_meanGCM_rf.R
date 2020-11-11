
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

vrs <- readRDS(file = '../rds/run1/vrs.rds')

# Load data
fls <- list.files('../rf/output/run1/results/raw/2050s', full.names = TRUE, pattern = '.asc$')
fls <- grep('Prob', fls, value = TRUE)
ftr <- raster::stack(fls)
ftr <- mean(ftr)
crn <- raster('../rf/output/run1/results/raw/RF_5Prob_current.asc')

# Get the threshold
load('../rData/run1/clustereddata.rData')
pnt <- clusteredpresdata[,c('x', 'y', 'gid')]
vls.crn <- as.data.frame(cbind(pnt, raster::extract(crn, pnt[,1:2])))
vls.crn <- as_tibble(vls.crn)
names(vls.crn) <- c('x', 'y', 'gid', 'values')
qnt_05 <- quantile(vls.crn$values, seq(0, 1, 0.01))
threshold <- 0.3069104

# A simple plot
par(mfrow = c(1,2))
crn_rcl <- raster::reclassify(crn, c(0, threshold, 0, threshold, 1, 1))
ftr_rcl <- raster::reclassify(ftr, c(0, threshold, 0, threshold, 1, 1))
plot(crn_rcl, main = 'Current')
plot(ftr_rcl, main = 'Future')

writeRaster(crn_rcl, '../rf/output/run1/results/reclassify/crn_rcl.tif')
writeRaster(ftr_rcl, '../rf/output/run1/results/reclassify/ftr_rcp45_50s.tif')
