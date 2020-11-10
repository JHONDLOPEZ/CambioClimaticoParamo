

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, dismo)
pacman::p_load(tidyverse, raster, rgdal, cclust, dismo, gtools, sp, rgeos, FactoMineR, pROC, randomForest, Hmisc, rgeos)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('./FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
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
run <- 'run1'

# Points
pnts <- read_csv('../tables/paramos/paramos_sample.csv')
pnts <- pnts[,c(1:2)]
pnts$gid <- 1:nrow(pnts)

# Climate
bios <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
bios <- grep('bio_', bios, value = TRUE)
bios <- mixedsort(bios)
bios <- stack(bios)

# Variables to use
vars <- readRDS(file = '../rds/run1/vrs.rds')

# Coordinate system
geo <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

# Cluster analysis (Random Forest) / Load
load(file = '../rData/run1/clusterdata.rData')
load(file = paste0('../rData/', run, '/clustereddata.rData'))

# Background process ------------------------------------------------------
SPspecies <- SpatialPoints(pnts[,c(1:2)])
crs(SPspecies) <- geo
back_raster <- bios[[1]] * 0 + 1
speciescell <- raster::extract(back_raster, SPspecies, cellnumber = TRUE)
back_raster[speciescell[,1]] <- NA
samplesize <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) 
NumberOfClusters <- max(clusteredpresdata$cluster) 
numberofpresences <- nrow(clusteredpresdata) 
crs(back_raster) <- geo

back <- randomPoints(back_raster, 1*numberofpresences) %>%
  as_tibble()
coordinates(back) <- ~ x + y
back_swd  <- raster::extract(bios, back) %>% 
  cbind(coordinates(back), .)
back_swd <- back_swd[,c('x', 'y', vars)]
nrow(back_swd) == nrow(back_swd[complete.cases(back_swd),])

dir.create('../rf/input/points/run1', recursive = TRUE)
write.csv(back_swd, '../rf/input/points/run1/back_swd.csv', row.names = FALSE)
write.csv(pnts, '../rf/input/points/run1/occ_swd.csv', row.names = FALSE)

# Cluster analysis to pseudoabsences
bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 3, nclasses = 2)
datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
attach(datRF)
no.forests <- 50#raw = 25
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])

vrs <- readRDS('../rds/run1/vrs.rds')

presvalue_swd  <- clusteredpresdata[,4:ncol(clusteredpresdata)] %>%
  cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
  na.omit() %>%
  as.data.frame() %>%
  mutate(cluster = cluster + no.absenceclasses)
presvalue_swd <- dplyr::select(presvalue_swd, pb, vrs)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background

dim(classdata_2); dim(presvalue_swd)
presvalue_swd <- presvalue_swd %>% dplyr::select(-cluster)

allclasses_swd <- rbind(classdata_2, presvalue_swd[,1:ncol(classdata_2)])
unique(allclasses_swd$pb)
write.csv(allclasses_swd, '../rf/input/points/run1/all_classes_swd.csv', row.names = FALSE)

# To make the random forest analysis --------------------------------------
vrs <- gsub('.asc', '', vrs) 
vrs <- gsub('\\$', '', vrs)
model1 <- as.formula(paste('factor(pb) ~', paste(paste(vrs), collapse = '+', sep =' ')))
rflist <- vector('list', 50) 
auc <- vector('list', 50)

library(pROC)

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                     size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  save(rfmodel, file = paste('../rf/output/run1/models/', NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

save(rflist, file = paste('../rData/', run, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
save(importance, file = paste0('../rData/', run, '/importanceRF.rData'))
save(auc, file = paste0('../rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0('../rData/', run, '/rff_dist.rData'))

# Predict model

bios <- list.files('../raster/climate/worldclim/baseline/1ha', full.names = TRUE, pattern = '.tif$')
bios <- grep('bio_', bios, value = TRUE)
bios <- grep(paste0(vrs, collapse = '|'), bios, value = TRUE)
bios <- mixedsort(bios)
bios <- stack(bios)

climatevalues  <- data.frame(getValues(bios))
NumberOfClusters <- 5

rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
rasterProbs_na <- na.omit(rasterProbs)
sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)

rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
uncertainty <- apply(rasterProbs, 1, max)  

rasterRFprob <- bios[[1]]
values(rasterRFprob) <- rasterRF 

rasterRFuncertainty <- bios[[1]]
values(rasterRFuncertainty) <- uncertainty 

rasterRF <- max.col(rasterProbs, 'first')
rasterRFclass <- bios[[1]]
values(rasterRFclass) <- rasterRF

writeRaster(rasterRFclass, paste0('../rf/output/run1/results/raw/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFprob, paste0('../rf/output/run1/results/raw/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFuncertainty, paste0('../rf/output/run1/results/raw/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)

