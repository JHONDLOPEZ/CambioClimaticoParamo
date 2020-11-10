
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data
bsl <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
rcp <- c('rcp_4.5', 'rcp_8.5')
prd <- c('2030s', '2050s', '2080s')
vrs <- c('prec', 'tmax', 'tmean', 'tmin')

bsl.ppt <- grep('prec_total', bsl, value = TRUE)
bsl.ppt <- raster(bsl.ppt)

bsl.tmx <- grep('tmax_total', bsl, value = TRUE)
bsl.tmx <- raster(bsl.tmx)

bsl.tmn <- grep('tmin_total', bsl, value = TRUE)
bsl.tmn <- raster(bsl.tmn)

# Function to use
check_future_data <- function(rc, vr, pr){
  
  rc <- rcp[1]
  vr <- vrs[2]
  pr <- prd[1]
  
  print('To starting')
  drs <- paste0('../raster/climate/ipcc/', rc, '/', pr) %>% 
    list.files(., full.names = TRUE)
  
  rsl <- map(.x = 1:length(drs), .f = function(k){
    
    print(basename(drs[k]))
    fls <- list.files(drs[k], full.names = TRUE, pattern = '.tif$') %>% 
      grep(vr, ., value = TRUE) 
    fls <- grep(paste0('/ha_', vr), fls, value = TRUE)
    fls <- mixedsort(fls)
    dfm <- data.frame(file = basename(fls), gcm = basename(drs[k]))
    print('Done!')
    return(dfm)
    
  })
}
createDifference <- function(rc, vr, pr){
  
  rc <- rcp[1]
  vr <- vrs[4]
  pr <- prd[1]
  
  print('To starting')
  drs <- paste0('../raster/climate/ipcc/', rc, '/', pr) %>% 
    list.files(., full.names = TRUE)
  
  rsl <- map(.x = 1:length(drs), .f = function(k){
    
    fls <- list.files(drs[k], full.names = TRUE, pattern = '.tif$') %>% 
      grep(vr, ., value = TRUE) %>% 
      grep('/ha_tmin_', ., value = TRUE) %>% 
      mixedsort() 
    stk <- stack(fls)
    stk <- mean(stk)
    dfr <- stk - bsl.tmn
    dfr <- dfr / 10
    # prc <- dfr / bsl.ppt * 100
    
    print('Done!')
    return(dfr)
    
  })
  
  rsl <- stack(rsl)
  avg <- mean(rsl)
  writeRaster(x = avg, filename = paste0('../raster/climate/difference/', vr, '_', gsub('\\.', '_', rc), '_', pr, '.tif'))
  saveRDS(object = mss, file = './mss.rds')
}



