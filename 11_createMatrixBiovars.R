
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, usdm, gtools, stringr, sf, tidyverse, dismo)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
bios <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
bios <- grep('bio_', bios, value = TRUE)
bios <- mixedsort(bios)
bios <- stack(bios)

# Presence paramos data
pnts <- read_csv('../tables/paramos/paramos_sample.csv')
pnts <- pnts[,c(1:2)]
pnts$gid <- 1:nrow(pnts)

# Extract the values for the climate data ---------------------------------
vlss <- raster::extract(bios, pnts[,1:2])
vlss <- cbind(pnts, vlss)
vlss <- as.data.frame(vlss)
vlss <- drop_na(vlss)

# To make the analysis VIF ------------------------------------------------
vif.res <- vif(x = vlss[,4:ncol(vlss)])
vif.step <- vifstep(x = vlss[,4:ncol(vlss)], th = 10)
vrs <- vif.step@results$Variables %>% as.character()

saveRDS(object = vrs, file = '../rds/run1/vrs.rds')

# To select the variables from the dataframe
occ_fnl <- vlss %>% 
  as_tibble %>%  
  dplyr::select(x:gid, vrs)

write.csv(occ_fnl, '../tables/paramos/paramos_sample_vars.csv', row.names = FALSE)

