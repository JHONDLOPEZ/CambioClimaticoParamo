
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)

# Load data ---------------------------------------------------------------
fls <- list.files('../raster/climate/worldclim/baseline/1ha')
fls <- grep('prec')