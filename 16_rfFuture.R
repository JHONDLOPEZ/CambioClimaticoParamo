
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, tidyverse, parallel, foreach, doSNOW, rgdal, cclust, outliers, dismo, gtools, multcomp, sp, rgeos, outliers, FactoMineR, pROC, randomForest, stringr)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 9999)
run <- 'run1'
source('FunctionsRFclustering.R')
myproj <- CRS('+proj=longlat +datum=WGS84')

# Load data ---------------------------------------------------------------
load('../rData/run1/clusterdata.rData')
load('../rData/run1/clustereddata.rData')
load('../rData/run1/rflist_5.rdata')

NumberOfClusters <- 5
ar5biofolder <- '../raster/climate/ipcc/rcp_4.5'
yearlist <- list.files(ar5biofolder)
gcmlist <- list.files(paste0(ar5biofolder, '/', yearlist[1]))
resultsfolder <- paste0('../rf/output/run1/results/raw/') 
modelfolder <- '../rf/output/run1/models/'

rff <- do.call(randomForest::combine, rflist)

cl <- makeCluster(2)
registerDoSNOW(cl)

y <- 1
vrs <- readRDS('../rds/run1/vrs.rds')

foreach(i = 1:length(gcmlist), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'randomForest', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  print(gcmlist[i]) 
  
  
  gcmfiles <- paste(ar5biofolder, yearlist[y], gcmlist[i], sep = '/') %>%
    list.files(., full.names = T, pattern = '.tif$') %>% 
    grep('/ha_', ., value = TRUE) %>% 
    mixedsort()
  
  prec <- stack(grep('prec', gcmfiles, value = TRUE))
  tmax <- stack(grep('tmax', gcmfiles, value = TRUE))
  tmin <- stack(grep('tmin', gcmfiles, value = TRUE))
  gcmfiles <- dismo::biovars(prec, tmin, tmax)
  vars <- parse_number(vrs)
  gcmfiles <- subset(gcmfiles, vars)
  names(gcmfiles) <- vrs
  
  climatelayers <- raster::stack(gcmfiles)
  climatevalues <- data.frame(getValues(climatelayers))
  
  print('Climate values')
  
  rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- gcmfiles[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- gcmfiles[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- gcmfiles[[1]]
  values(rasterRFclass) <- rasterRF
  
  print("Write Raster...")
  writeRaster(rasterRFclass, paste0('../rf/output/run1/results/raw/',  yearlist[y], '/RF_', NumberOfClusters, 'Clust_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFprob, paste0('../rf/output/run1/results/raw/', yearlist[y], '/RF_', NumberOfClusters, 'Prob_',  gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFuncertainty, paste('../rf/output/run1/results/raw/', yearlist[y], '/RF_', NumberOfClusters, 'Unc_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  
  print('Done!')
  print(gcmlist[i])
  
}
#

mdl <- list.files('../_rf/_output/_run1/_results/_raw/_2050/', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Clust', ., value = T) %>% 
  stack()
mdl <- raster::modal(mdl)  
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Clust_future.asc')


prb <- list.files('../_rf/_output/_run1/_results/_raw/_2050/', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Prob_', ., value = T) %>% 
  stack()
prb <- mean(prb)
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Prob_future.asc', overwrite = TRUE)

