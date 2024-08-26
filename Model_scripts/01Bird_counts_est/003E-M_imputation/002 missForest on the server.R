Sys.time()

library("pbapply")
library("dplyr")
library("tidyr")
library("missForest")
library("doParallel")
pboptions(type="timer")

setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data")

files <- list.files(pattern = "RFdfwithtruezerofull.csv$")
desired_order <- c("WSRFdfwithtruezerofull.csv ", "BGRFdfwithtruezerofull.csv ", "MSRFdfwithtruezerofull.csv ", "GGRFdfwithtruezerofull.csv ", "MRFdfwithtruezerofull.csv ")
files <- files[match(desired_order, basename(files))]
RF_sp <- lapply(files, function(file) read.csv(file, sep = ","))

WS <- list(); BG=list(); MS=list(); GG<-list(); M<-list()
RFdata<-list(WS, BG, MS, GG, M); RFdata.imp<-RFdata

ncores <- 10L

for (sp in c(1:5)){
  for (Y in c(1:5)){
    RF_sp[[sp]]<-RF_sp[[sp]] %>% arrange(time)
    RFdata[[sp]][[Y]] <- RF_sp[[sp]][((Y-1)*614*52+1):(614*52*Y),] %>% mutate(time=as.character(RF_sp[[1]][((Y-1)*614*52+1):(614*52*Y),]$time)) %>% mutate(isoweek=substr(time, nchar(time) - 1, nchar(time))) %>% select(-time, -KN10kmDK)
    RFdata[[sp]][[Y]] <- RFdata[[sp]][[Y]] %>% mutate(isoweek=as.factor(RFdata[[sp]][[Y]]$isoweek))
    RFdata[[sp]][[Y]] <- RFdata[[sp]][[Y]] %>% mutate(DOF.max.count = replace_na(DOF.max.count, 0)) %>% select(counts, Value, DOF.max.count, coast_len, Artificial.surfaces, Agricultural.areas, Forest.and.semi.natural.areas, Wetlands, Water.bodies, isoweek)
    registerDoParallel(cores=ncores)
    RFdata.imp[[sp]][[Y]] <- missForest(RFdata[[sp]][[Y]], verbose = TRUE, parallelize = 'forests')
}}

session <- sessionInfo()
save(RFdata.imp, file = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data/missForest_RFdata.imputed_sp_Y.rda")

Sys.time()
session
