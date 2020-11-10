

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, outliers, Hmisc, cclust, sf, randomForest, factoextra, multcomp, dismo, magrittr, ggpubr, corrplot)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('FunctionsRFclustering.R')
run <- 'run1'

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,4:ncol(occ)]
  print(nrow(datRF))
  
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  
  return(list(labelRF, clusterdata))
  
}


# Load data ---------------------------------------------------------------

# Bioclimatic variables
bios <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
bios <- grep('bio_', bios, value = TRUE)
bios <- mixedsort(bios)
bios <- stack(bios)

# Coordinate system
geo <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Points
pnts <- read_csv('../tables/paramos/paramos_sample_vars.csv')
vars <- colnames(pnts)[4:ncol(pnts)]

# Clustering using random forest ------------------------------------------
env_values <- as.matrix(pnts[,4:ncol(pnts)])
nrow(env_values)
datRF <- as.data.frame(pnts[,4:ncol(pnts)])
nrow(datRF)
d <- dist(datRF, method = 'euclidean')
rfClust <- rf.clust(occ = pnts, nforest = 25, ntrees = 100, nVars = 3, nclasses = 5)
labelRF <- rfClust[[1]]
clusterdata <- rfClust[[2]]
classdata <- cbind(pb = as.factor(labelRF), pnts[,4:ncol(pnts)])
clusteredpresdata <- cbind(pnts, cluster = labelRF) %>% na.omit() %>% tbl_df()
no.clusters <- 5
head(clusteredpresdata)

clusterdata
plot(clusterdata)

run
dir.create('../rData/run1', recursive = TRUE)
save(datRF, file = paste0('../rData/', run, '/datRF.rData'))
save(clusterdata, file = paste0('../rData/', run, '/clusterdata.rData'))
save(pnts, clusteredpresdata, no.clusters, labelRF, file = paste0('../rData/', run, '/clustereddata.rData'))

# To create the dendogram
fviz_dend(clusterdata)

pp <- fviz_dend(clusterdata, k = 5, # Cut in four groups
                cex = 0.5, # label size
                k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", '#868A08'),
                color_labels_by_k = TRUE, # color labels by groups
                rect = TRUE # Add rectangle around groups
                )


# Table cluster to shapefile 
shp.pnt <- clusteredpresdata
coordinates(shp.pnt) <- ~ x + y
shapefile(shp.pnt, '../shapefiles/rf/points_cluster_run1.shp')
