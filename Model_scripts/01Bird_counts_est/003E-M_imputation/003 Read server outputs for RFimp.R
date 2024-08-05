load("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Model_scripts/03Model_runs/Simulation outputs/missForest_RFdata.imputed_sp_Y_corrected.rda")
#load RFdata.imp

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

saveRDS(RFimp_list, file = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Data/Check zeros in GLMMdf/missForest_RFdata.imputed_list_corrected.rda")

files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM" , pattern = "glmmtruezerofull.csv$")
desired_order <- c("WSglmmtruezerofull.csv", "BGglmmtruezerofull.csv", "MSglmmtruezerofull.csv", "GGglmmtruezerofull.csv", "Mglmmtruezerofull.csv")
files <- files[match(desired_order, basename(files))]
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM")
GLMM_sp <- lapply(files, function(file) read.csv(file, sep = ","))

RF_list <- list()
for (sp in c(1:5)) {
  RF_list[[sp]] <- cbind(RFimp_list[[sp]] %>% mutate(counts = ifelse(counts < 0, 0, counts)) %>% dplyr::select(isoweek, counts), GLMM_sp[[sp]] %>% select(KN10kmDK, time))
}

saveRDS(RF_list, file = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Data/Check zeros in GLMMdf/missForest_RFdata.imputed_full_list_corrected.rda")