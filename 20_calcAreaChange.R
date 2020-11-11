
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use
calc_change <- function(rs){
  
  print('To start')
  st <- stack(crn, rs)
  tb <- as_tibble(rasterToPoints(st, spatial = FALSE))
  names(tb) <- c('x', 'y', 'cr', 'ft')
  tb <- tb %>% 
    mutate(change = ifelse(cr == 0 & ft == 0, 'No paramo',
                           ifelse(cr == 0 & ft == 1, 'Ganancia paramo',
                                  ifelse(cr == 1 & ft == 0, 'Perdida paramo',
                                         ifelse(cr == 1 & ft == 1, 'Keep paramo', 'No pasa nada')))))
  tb <- inner_join(tb, lbl, by = c('change' = 'category'))
  rs <- rasterFromXYZ(tb[,c(1, 2, 6)])
  crs(rs) <- geo
  pr <- projectRaster(rs, crs = prj)
  re <- res(pr)[1] * res(pr)[2] / 10000
  sm <- tb %>% group_by(change) %>% dplyr::summarise(count = n()) %>% ungroup() %>% mutate(has = count * re)
  print('Done')
  return(sm)
  
}

# Load data
fls <- list.files('../rf/output/run1/results/reclassify', full.names = TRUE)
crn <- raster(grep('crn', fls, value = TRUE))

# RCP 45 
f30 <- raster(grep('rcp45_30s', fls, value = TRUE))
f50 <- raster(grep('rcp45_50s', fls, value = TRUE))
f80 <- raster(grep('rcp45_80s', fls, value = TRUE))

# Labels
lbl <- data.frame(category = c('Ganancia paramo', 'Keep paramo', 'No paramo', 'Perdida paramo'), values = 1:4)
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
geo <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Apply the function to calc the change
smm_f30 <- calc_change(rs = f30) %>% mutate(period = 'f30')
smm_f50 <- calc_change(rs = f50) %>% mutate(period = 'f50')
smm_f80 <- calc_change(rs = f80) %>% mutate(period = 'f80')
smm_r45 <- rbind(smm_f30, smm_f50, smm_f80) %>% dplyr::select(-count) %>% spread(period, has)
smm_r45[is.na(smm_r45)] <- 0
smm_r45 <- smm_r45 %>% mutate(rcp = 'rcp45')
write.csv(smm_r45, '../tables/paramos/difference_rcp45_paramos.csv', row.names = FALSE)
