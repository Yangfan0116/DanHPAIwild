load("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data/missForest_RFdata.imputed_sp_Y.rda")

WS <- list(); BG=list(); MS=list(); GG<-list(); M<-list()
RFdata<-list(WS, BG, MS, GG, M); RFdata.complete<-RFdata
for (sp in seq_len(5)){
  for (Y in seq_len(5)){
    
    RFdata.complete[[sp]][[Y]] <- RFdata.imp[[sp]][[Y]]$ximp
  }
}

RFimp_list <- lapply(RFdata.complete, function(sp) {
  do.call(rbind, sp)
})

files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data" , pattern = "RFdfwithtruezerofull.csv$")
desired_order <- c("WSRFdfwithtruezerofull.csv", "BGRFdfwithtruezerofull.csv", "MSRFdfwithtruezerofull.csv", "GGRFdfwithtruezerofull.csv", "MRFdfwithtruezerofull.csv")
files <- files[match(desired_order, basename(files))]
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data")
GLMM_sp <- lapply(files, function(file) read.csv(file, sep = ","))

RF_list <- list()
for (sp in c(1:5)) {
  RF_list[[sp]] <- cbind(RFimp_list[[sp]] %>% mutate(counts = ifelse(counts < 0, 0, counts)) %>% dplyr::select(isoweek, counts), GLMM_sp[[sp]] %>% select(KN10kmDK, time))
}

saveRDS(RF_list, file = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data/missForest_RFdata.imputed_full_list.rda")