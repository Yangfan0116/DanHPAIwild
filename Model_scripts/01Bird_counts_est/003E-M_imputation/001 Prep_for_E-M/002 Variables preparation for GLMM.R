library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(sp)
library(ggplot2)
library(tibble)
library(plyr)

#load grid cells
setwd(".../DanHPAIwildModel/")

DKmapsf <- readRDS("Data/Shapefiles/DKmapsf.rds")
#DK map
DK <- read_sf("Data/Shapefiles/DK_shapefile/DK_outline.shp") 

#1.Ajusted NOVANA counts
#load True-zero raw data
files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM" , pattern = "full_idenTrue0.csv$")
desired_order <- c("WSfull_idenTrue0.csv", "BGfull_idenTrue0.csv", "MSfull_idenTrue0.csv", "GGfull_idenTrue0.csv", "Mfull_idenTrue0.csv")
files <- files[match(desired_order, basename(files))]
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM")
Truezeros_sp <- lapply(files, function(file) read.csv(file, sep = ","))

#2.mean temperature per cell per week
setwd(".../DanHPAIwildModel/")
Meantemp <- read.csv("Data/Bird counts/Bird counts raw/GLMM/mean temperature 609cells in study period.csv")
Meantemp$isoyear <- as.integer(Meantemp$isoyear)
Meantemp$isoweek <- as.integer(Meantemp$isoweek)
Meantemp$time <- Meantemp$isoyear*100+Meantemp$isoweek
#contains info for 609 cells out of 614 cells

#3. Compensational volunteers’ observations 
# source("01Bird-counts-est/003E-M_imputation/Prep_for_E-M/001 compensational observations by DOF.R")
files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM/Predictor DOF variable", pattern = "\\.csv$")
desired_order <- c("WSPredictor DOF column (590cells).csv", "BGPredictor DOF column (590cells).csv", "MSPredictor DOF column (590cells).csv", "GGPredictor DOF column (590cells).csv", "MPredictor DOF column (590cells).csv")
files <- files[match(desired_order, basename(files))]
#read DOF data of 5 species per day in 2016-2021
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM/Predictor DOF variable")
column.DOF_list <- lapply(files, function(file) read.csv(file))
#check true zeros
files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM" , pattern = "full_idenTrue0.csv$")
desired_order <- c("WSfull_idenTrue0.csv", "BGfull_idenTrue0.csv", "MSfull_idenTrue0.csv", "GGfull_idenTrue0.csv", "Mfull_idenTrue0.csv")
files <- files[match(desired_order, basename(files))]
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM")
Truezeros_sp <- lapply(files, function(file) read.csv(file, sep = ","))
#
Compen_dof <- list()
for(sp in c(1:5)){
  Compen_dof[[sp]] <- right_join(column.DOF_list[[sp]], Truezeros_sp[[sp]], by=c('KN10kmDK', "time")) %>% select(species, time, KN10kmDK, zerotype, DOF.max.count) %>% mutate(DOF.max.count = if_else(is.na(DOF.max.count)&zerotype==1, 0, DOF.max.count)) %>% select(-zerotype) %>% filter(!is.na(DOF.max.count))
}

#4. Proportions of land cover types
setwd(".../DanHPAIwildModel/")
land.cover <- read.csv("Data/Bird counts/Bird counts raw/GLMM/DKLine1kmCLC proportions.csv")

#5. coastline length
shape <- st_read("Data/Shapefiles/DK_shapefile/DK_outline.shp") %>%  
  dplyr::summarise() %>% 
  st_geometry() %>% 
  st_cast("POLYGON") %>% 
  st_cast("LINESTRING")
DKmapsf_modi <- DKmapsf %>% 
  select(-Stednavn.new)

intersection <- st_intersection(DKmapsf_modi, shape) %>% 
  mutate(lenght = st_length(.)) %>% 
  st_drop_geometry() # complicates things in joins later on
DKmapsf.coastlen <- DKmapsf_modi %>% 
  left_join(intersection, by = "KN10kmDK")

DKmapsf.coastlen_df <- as.data.frame(DKmapsf.coastlen)
DKmapsf.coastlen_df$lenght <- as.numeric(DKmapsf.coastlen_df$lenght)
#aggragte by the same cell name, and only keep the greatest value
DKmapsf.coastlen_df_agg <- aggregate(x=list(DKmapsf.coastlen_df$lenght), by=list(DKmapsf.coastlen_df$KN10kmDK), FUN=max)
colnames(DKmapsf.coastlen_df_agg)<- c("KN10kmDK", "coast_len")
DKmapsf.coastlen_df_agg[is.na(DKmapsf.coastlen_df_agg)] <- 0
DKmapsf.coastlen_df_agg.plot <- left_join(DKmapsf, DKmapsf.coastlen_df_agg, by = "KN10kmDK")

# Creating the dataframe containing above five variables
GLMMdf<-list()
file_names <- c("WS", "BG", "MS", "GG", "M")

for(sp in seq_along(1:5)){
  GLMMdf[[sp]] <- Truezeros_sp[[sp]] %>% 
    select(-X, -DOF.ratio.counts)
  GLMMdf[[sp]] <- left_join(GLMMdf[[sp]], Meantemp, by = c("time", "KN10kmDK")) %>% 
    select(KN10kmDK, time, counts, Value)
  GLMMdf[[sp]] <- left_join(GLMMdf[[sp]], Compen_dof[[sp]], by = c("KN10kmDK", "time")) %>% 
    select(KN10kmDK, time, counts, Value, DOF.max.count)
  GLMMdf[[sp]] <- left_join(GLMMdf[[sp]], DKmapsf.coastlen_df_agg, by = "KN10kmDK")
  GLMMdf[[sp]] <- left_join(GLMMdf[[sp]], land.cover, by='KN10kmDK') %>% 
    select(KN10kmDK, time, counts, Value, DOF.max.count, coast_len, Artificial.surfaces, Agricultural.areas, Forest.and.semi.natural.areas, Wetlands, Water.bodies)
  GLMMdf[[sp]]$KN10kmDK <- as.factor(GLMMdf[[sp]]$KN10kmDK)
  GLMMdf[[sp]]$time<-as.factor(GLMMdf[[sp]]$time)
  write.csv(GLMMdf[[sp]], file=paste0("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM/", file_names[[sp]], "glmmtruezerofull.csv"))
}



































