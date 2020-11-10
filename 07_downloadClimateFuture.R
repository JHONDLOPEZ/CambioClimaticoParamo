
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, ccafs, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to download ----------------------------------------------------
download_climate <- function(vr, pr, sc){
  
  # Proof
  vr <- 'prec'
  pr <- '2050s'
  sc <- 'RCP 4.5'
  
  print('Filtering the variable and the period')
  vr_nm <- vrs %>% filter(variable == vr) %>% pull(1)
  pr_nm <- prd %>% filter(period == pr) %>% pull(1)
  sn_nm <- scn %>% filter(scenario == sc) %>% pull(1)
  
  looking <- cc_search(
    file_set = 12,
    scenario = sn_nm,
    period = pr_nm,
    resolution = 1,
    variable = vr_nm,
    extent = 'region'
  )
  
  looking <- grep('b2_asc.zip', looking, value = TRUE)
  gcms <- str_sub(looking, start = 93, end = nchar(looking) - 10)
  gcms <- str_split(gcms, pattern = '/')
  gcms <- sapply(1:length(gcms), function(k) gcms[[k]][1])
  gcms <- c("bcc_csm1_1_m", "bnu_esm", "cccma_canesm2", "csiro_access1_0")
  
  looking <- grep(paste0(gcms, collapse = '|'), looking, value = TRUE)
  
  download <- lapply(1:length(looking), function(k){
    
    print(paste0('Start with ', looking[k]))
    rstr <- cc_data_fetch(key = looking[k])
    rstr <- cc_data_read(rstr)
    rstr <- raster::crop(rstr, shp) %>% raster::mask(., shp)
    
    dir_output <- paste0('../raster/climate/ipcc/rcp_', parse_number(sc), '/', pr, '/', gcms[k])
    ifelse(!dir.exists(dir_output), dir.create(dir_output, recursive = TRUE), 'Directory exist')
    Map('writeRaster', x = unstack(rstr), filename = paste0(dir_output, '/', names(rstr), '.tif'), overwrite = TRUE)
    
    rm(rstr, dir_output)
    
    cache <- list.files(rappdirs::user_cache_dir('ccafs'), full.names = TRUE)
    map(.x = 1:length(cache), .f = function(k) file.remove(cache[k]))
    cc_cache_delete_all(force = TRUE)
    print(paste0('Done ', looking[k]))
    
  })
  
}


# Load data ---------------------------------------------------------------
shp <- shapefile('../shapefiles/base/mpios_zone.shp')

# Labels
prd <- data.frame(
  value = c(4, 6, 9),
  period = c('2030s', '2050s', '2080s')
)

vrs <- data.frame(
  value = 2:5,
  variable = c('prec', 'tmax', 'tmean', 'tmin')
)

scn <- data.frame(
  value = c(8, 10), 
  scenario = c('RCP 4.5', 'RCP 8.5')
)

# To apply the function ---------------------------------------------------
lapply(1:2, function(i){
  
  lapply(1:4, function(j){
    
    lapply(1:3, function(k){
      
      print(paste0('------------------------- To starting... ', pull(vrs, 2)[j], ' ', pull(prd, 2)[k], ' ', pull(scn, 2)[i], ' -------------------------'))
      download_climate(vr = pull(vrs, 2)[j], pr = pull(prd, 2)[k], sc = pull(scn, 2)[i])
      print('Done!')
      
    })
    
  })
  
})

