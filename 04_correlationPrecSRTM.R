
# Load libraries --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
make_correlation <- function(v1, v2){
  
  # v1 <- vrb_ppt[1]
  # v2 <- vrb_trn[1]
  
  tbl <- vls %>% 
    filter(var %in% c(v1, v2)) %>% 
    dplyr::select(variable = var, value) 
  crl <- cor(tbl %>% filter(variable == v1) %>% pull(2), tbl %>% filter(variable == v2) %>% pull(2))
  rsl <- data.frame(variable_1 = v1, variable_2 = v2, correlation = round(crl, 2))
  
  print('Done!')
  return(rsl)
  
}

# Load data ------------------------------------------------------------
trn <- list.files('../raster/srtm/90m', full.names = TRUE, pattern = '.tif$')
fls <- list.files('../raster/climate/worldclim/baseline/1km', full.names = TRUE, pattern = '.tif')
ppt <- grep('prec', fls, value = TRUE)
ppt <- mixedsort(ppt)

# Make the extent of the terrain raster ---------------------------------
trn <- map(.x = trn, .f = raster)
ext <- map(.x = trn, .f = extent)
ext[2]; ext[1] # Checking the extent for the DEM and the aspect rasters
trn[[2]] <- raster::resample(x = trn[[2]], y = trn[[1]])
trn <- stack(trn)

# File to raster --------------------------------------------------------
trn <- stack(trn)
ppt <- stack(ppt)
crd <- rasterToPoints(ppt, spatial = FALSE)
crd <- as_tibble(crd)
crd <- crd[,c(1,2)]
crd$gid <- 1:nrow(crd)

n <- nrow(crd) * 0.2
n <- round(n, 0)

# Make a sample ----------------------------------------------------------
crd_sub <- sample_n(tbl = crd, size = n, replace = FALSE)

# Extracting the values for all the variables ----------------------------

# Precipitation
vls_ppt <- raster::extract(ppt, crd_sub[,1:2])
vls_ppt <- cbind(crd_sub, vls_ppt)
vls_ppt <- as_tibble(vls_ppt)
vls_ppt <- drop_na(vls_ppt)

# Terrain
vls_trn <- raster::extract(trn, crd_sub[,1:2])
vls_trn <- cbind(crd_sub, vls_trn)
vls_trn <- as_tibble(vls_trn)
vls_trn <- drop_na(vls_trn)
vls_trn <- vls_trn %>% dplyr::select(-x, -y)

# Join the two tables into only one
vls <- inner_join(vls_ppt, vls_trn, by = 'gid')
vls <- vls %>% gather(var, value, -x, -y, -gid)

vrb_ppt <- colnames(vls_ppt)[4:ncol(vls_ppt)]
vrb_trn <- colnames(vls_trn)[2:ncol(vls_trn)]

# To estimate the correlation between each pair of variables --------------
crl_tbl <- lapply(1:length(vrb_ppt), function(k){
  
  print(vrb_ppt[k])
  
  lapply(1:length(vrb_trn), function(j){
    
    print(vrb_trn[j])
    make_correlation(v1 = vrb_ppt[k], v2 = vrb_trn[j])
    
  })
  
})

crl_tbl <- flatten(crl_tbl)
crl_tbl <- bind_rows(crl_tbl)
crl_tbl <- crl_tbl %>% mutate(correlation_2 = correlation ^ 2)

# To extract the maximum corrleation value for each pair of variab --------
crl_max <- lapply(1:length(vrb_ppt), function(k){
  
  print(vrb_ppt[k])
  crl_tbl %>% 
    filter(variable_1 == vrb_ppt[k]) %>% 
    top_n(x = ., n = 1, wt = correlation_2)
  
})

crl_max <- bind_rows(crl_max)
dir.create('../tables/correlation_climate', recursive = TRUE)
write.csv(crl_max, '../tables/correlation_climate/correlation_max_prec.csv', row.names = FALSE)
