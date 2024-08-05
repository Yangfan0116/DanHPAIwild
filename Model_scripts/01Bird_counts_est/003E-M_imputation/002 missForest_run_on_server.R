Sys.time()

library("pbapply")
library("dplyr")
library("tidyr")
library("missForest")
library("doParallel")
pboptions(type="timer")


files <- list.files(pattern = "glmmtruezerofull.csv$")
desired_order <- c("WSglmmtruezerofull.csv", "BGglmmtruezerofull.csv", "MSglmmtruezerofull.csv", "GGglmmtruezerofull.csv", "Mglmmtruezerofull.csv")
files <- files[match(desired_order, basename(files))]
GLMM_sp <- lapply(files, function(file) read.csv(file, sep = ","))

WS <- list(); BG=list(); MS=list(); GG<-list(); M<-list()
RFdata<-list(WS, BG, MS, GG, M); RFdata.imp<-RFdata

ncores <- 10L

for (sp in c(1:5)){
  for (Y in c(1:5)){
    GLMM_sp[[sp]]<-GLMM_sp[[sp]] %>% arrange(time)
    RFdata[[sp]][[Y]] <- GLMM_sp[[sp]][((Y-1)*614*52+1):(614*52*Y),] %>% mutate(time=as.character(GLMM_sp[[1]][((Y-1)*614*52+1):(614*52*Y),]$time)) %>% mutate(isoweek=substr(time, nchar(time) - 1, nchar(time))) %>% select(-time, -KN10kmDK)
    RFdata[[sp]][[Y]] <- RFdata[[sp]][[Y]] %>% mutate(isoweek=as.factor(RFdata[[sp]][[Y]]$isoweek))
    RFdata[[sp]][[Y]] <- RFdata[[sp]][[Y]] %>% mutate(DOF.max.count = replace_na(DOF.max.count, 0)) %>% select(counts, Value, DOF.max.count, coast_len, Artificial.surfaces, Agricultural.areas, Forest.and.semi.natural.areas, Wetlands, Water.bodies, isoweek)
    registerDoParallel(cores=ncores)
    RFdata.imp[[sp]][[Y]] <- missForest(RFdata[[sp]][[Y]], verbose = TRUE, parallelize = 'forests')
}}

session <- sessionInfo()
save(RFdata.imp, file = "missForest_RFdata.imputed_sp_Y_corrected.rda")

Sys.time()
session
