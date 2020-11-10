

# Load libraries -----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data ---------------------------------------------------------------

rcp <- c('rcp_4.5', 'rcp_8.5')
prd <- c('2030s', '2050s', '2080s')
vrs <- c('prec', 'tmax', 'tmean', 'tmin')

# Checking downscaling and download data
check_raster <- function(rc, pr){
  
  rc <- rcp[1]
  pr <- prd[2]
  
  drs <- paste0('../raster/climate/ipcc/', rc, '/', pr) %>% 
    list.files(., full.names = TRUE)
  fls <- map(.x = 1:length(drs), .f = function(k){
    x <- list.files(drs[k], full.names = TRUE)
    # x <- x[-grep('ha_ha', x, value = FALSE)]
    x <- data.frame(x = basename(x), gcm = basename(drs[k]))
  })
  tbl <- bind_rows(fls)
  tbl <- as_tibble(tbl)
  var <- str_split(string = tbl$x, pattern = '_')
  var <- sapply(1:length(var), function(k) var[[k]][[2]])
  tbl <- tbl %>% mutate(variable = var)
  smm <- tbl %>% group_by(gcm) %>% dplyr::summarise(count = n()) %>% ungroup()
  write.csv(tbl, paste0('../tables/check_climate/check_', rc, '_', pr, '.csv'), row.names = FALSE)
  
  mss <- smm %>% filter(count < 48) %>% pull(1)
  saveRDS(object = mss, file = './mss.rds')
  tbl %>% filter(gcm %in% mss) %>% group_by(gcm, variable) %>% summarise(count = n()) %>% ungroup() %>% View()
}


