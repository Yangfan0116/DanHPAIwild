setwd(".../DanHPAIwild")
source("Data/Required pkgs.R")
DKmapsf <- readRDS("Data/Shapefiles/DKmapsf.rds")

#load rasterized NOVANA midwinter data, NOVANA data is not publicly available
# path <- "Nonpublic data/5spe_NOVANA_614cells.xlsx"
NOVANA_list <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

#DOF data is publicly available, can be downloaded from: https://www.gbif.org/dataset/95db4db8-f762-11e1-a439-00145eb45e9a
files <- list.files("Data", pattern = "\\.csv$")
desired_order <- c("Whooper Swan max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Barnacle Goose max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Mute Swan max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Greylag Goose max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Mallard max count pr day DOFbasen 10x10km 2016 - 2021 .csv")
files <- files[match(desired_order, basename(files))]
#read DOF data of 5 species per day in 2016-2021
DOF_list <- lapply(files, function(file) read.csv(file, sep = ";"))

DOF_list_agg_week <- list(); DOF_list_agg_weekratio <- list(); DOF_list_weekly_ratio.5seasons <- list()
Y1W2 <- list(); Y2W2 <- list(); Y3W2 <- list(); Y4W2 <- list(); Y5W2 <- list()
NOVANA_list_agg <- list(); NOVANA_list_agg2 <- list(); DOF_list.2016_17.weekly <- list(); season2016_17 <- list(); Ratio2016_17 <- list(); DOF_list.2017_18.weekly <- list(); season2017_18 <- list(); Ratio2017_18 <- list(); DOF_list.2018_19.weekly <- list(); season2018_19 <- list(); Ratio2018_19 <- list(); DOF_list.2019_20.weekly <- list(); season2019_20 <- list(); Ratio2019_20 <- list(); DOF_list.2020_21.weekly <- list(); season2020_21 <- list(); Ratio2020_21 <- list(); DOF_list_weekly_phenology_counts_in_5seasons <- list()

for (sp in seq_along(1:5)){
  DOF_list[[sp]]$date <- as.Date(DOF_list[[sp]]$date)
  # Give the simulation time step for each week
  DOF_list[[sp]] <- DOF_list[[sp]] %>%
    mutate(isoweek=lubridate::isoweek(date)) %>%
    mutate(isoyear=lubridate::isoyear(date)) %>%
    mutate(isoweek= ifelse(isoweek==53 & lubridate::month(date) == 12,52, if_else(isoweek==53 & lubridate::month(date) == 1, 1, isoweek))) %>% mutate(isoyear = ifelse(lubridate::year(date)!=isoyear,lubridate::year(date),isoyear)) %>%
    unite("week", isoyear:isoweek, remove = F )
  DOF_list[[sp]]$isoyear <- as.numeric(DOF_list[[sp]]$isoyear)
  DOF_list[[sp]]$isoweek <- as.numeric(DOF_list[[sp]]$isoweek)

  DOF_list_agg_week[[sp]] <- aggregate(DOF_list[[sp]]$max_count, by=list(DOF_list[[sp]]$species, DOF_list[[sp]]$isoyear, DOF_list[[sp]]$isoweek, DOF_list[[sp]]$week), FUN=sum)
  colnames(DOF_list_agg_week[[sp]]) <- c("species", "isoyear", "isoweek", "week", "sum_agg_max_count")

  #calculate ratios: for each week, we calculate ratios of sum_agg_max_count to the counts in week 2 of the same epi year, i.e. Week 2 has ratio of 1.
  DOF_list_agg_weekratio[[sp]] <-DOF_list_agg_week[[sp]][order(as.numeric(DOF_list_agg_week[[sp]]$isoweek)),]
  Y1W2[[sp]] <- DOF_list_agg_weekratio[[sp]]$sum_agg_max_count[which(DOF_list_agg_weekratio[[sp]]$week==c("2017_2"))]
  Y2W2[[sp]] <- DOF_list_agg_weekratio[[sp]]$sum_agg_max_count[which(DOF_list_agg_weekratio[[sp]]$week==c("2018_2"))]
  Y3W2[[sp]] <- DOF_list_agg_weekratio[[sp]]$sum_agg_max_count[which(DOF_list_agg_weekratio[[sp]]$week==c("2019_2"))]
  Y4W2[[sp]] <- DOF_list_agg_weekratio[[sp]]$sum_agg_max_count[which(DOF_list_agg_weekratio[[sp]]$week==c("2020_2"))]
  Y5W2[[sp]] <- DOF_list_agg_weekratio[[sp]]$sum_agg_max_count[which(DOF_list_agg_weekratio[[sp]]$week==c("2021_2"))]

  DOF_list_weekly_ratio.5seasons[[sp]] <- DOF_list_agg_weekratio[[sp]] %>%
    dplyr::arrange(isoyear) %>% dplyr::mutate(X=row_number(), .before = species) %>%
    filter(X %in% c(39:298)) %>% mutate(Ratio=case_when(X %in% c(39:90) ~ sum_agg_max_count/Y1W2[[sp]],
                                                        X %in% c(91:142) ~ sum_agg_max_count/Y2W2[[sp]],
                                                        X %in% c(143:194) ~ sum_agg_max_count/Y3W2[[sp]],
                                                        X %in% c(195:246) ~ sum_agg_max_count/Y4W2[[sp]],
                                                        X %in% c(247:298) ~ sum_agg_max_count/Y5W2[[sp]]))

  #average the counts over 6 calendar years.
  NOVANA_list_agg[[sp]] <- aggregate(NOVANA_list[[sp]]$cell.Antal.NOVANA, by=list(NOVANA_list[[sp]]$Species, NOVANA_list[[sp]]$KN10kmDK, NOVANA_list[[sp]]$Month), FUN=sum)
  colnames(NOVANA_list_agg[[sp]]) <- c("Species", "KN10kmDK", "Week", "yearmean.cell.Antal.NOVANA")
  #take the average of 6 years.
  NOVANA_list_agg[[sp]]$yearmean.cell.Antal.NOVANA <- NOVANA_list_agg[[sp]]$yearmean.cell.Antal.NOVANA/6

  #multiplied by ratios, and then obtained 5 years.
  #year 1
  season2016_17[[sp]] <- subset(DOF_list_weekly_ratio.5seasons[[sp]][1:52,])
  Ratio2016_17[[sp]] <- list(season2016_17[[sp]]$Ratio)
  NOVANA_list_agg2[[sp]] <- list()
  for(i in 1:52){
    NOVANA_list_agg2[[sp]][[i]] <- NOVANA_list_agg[[sp]] %>%
      mutate(Week =replace(Week, Week == 1, i)) %>%
      mutate(DOF.ratio.counts = yearmean.cell.Antal.NOVANA*Ratio2016_17[[sp]][[1]][i])
  }
  DOF_list.2016_17.weekly[[sp]] <- bind_rows(NOVANA_list_agg2[[sp]])
  #year 2
  season2017_18[[sp]] <- subset(DOF_list_weekly_ratio.5seasons[[sp]][53:104,])
  Ratio2017_18[[sp]] <- list(season2017_18[[sp]]$Ratio)
  NOVANA_list_agg2[[sp]] <- list()
  for(i in 1:52){
    NOVANA_list_agg2[[sp]][[i]] <- NOVANA_list_agg[[sp]] %>%
      mutate(Week =replace(Week, Week == 1, i)) %>%
      mutate(DOF.ratio.counts = yearmean.cell.Antal.NOVANA*Ratio2017_18[[sp]][[1]][i])
  }
  DOF_list.2017_18.weekly[[sp]] <- bind_rows(NOVANA_list_agg2[[sp]])
  #year 3
  season2018_19[[sp]] <- subset(DOF_list_weekly_ratio.5seasons[[sp]][105:156,])
  Ratio2018_19[[sp]] <- list(season2018_19[[sp]]$Ratio)
  NOVANA_list_agg2[[sp]] <- list()
  for(i in 1:52){
    NOVANA_list_agg2[[sp]][[i]] <- NOVANA_list_agg[[sp]] %>%
      mutate(Week =replace(Week, Week == 1, i)) %>%
      mutate(DOF.ratio.counts = yearmean.cell.Antal.NOVANA*Ratio2018_19[[sp]][[1]][i])
  }
  DOF_list.2018_19.weekly[[sp]] <- bind_rows(NOVANA_list_agg2[[sp]])
  #year 4
  season2019_20[[sp]] <- subset(DOF_list_weekly_ratio.5seasons[[sp]][157:208,])
  Ratio2019_20[[sp]] <- list(season2019_20[[sp]]$Ratio)
  NOVANA_list_agg2[[sp]] <- list()
  for(i in 1:52){
    NOVANA_list_agg2[[sp]][[i]] <- NOVANA_list_agg[[sp]] %>%
      mutate(Week =replace(Week, Week == 1, i)) %>%
      mutate(DOF.ratio.counts = yearmean.cell.Antal.NOVANA*Ratio2019_20[[sp]][[1]][i])
  }
  DOF_list.2019_20.weekly[[sp]] <- bind_rows(NOVANA_list_agg2[[sp]])
  #year 5
  season2020_21[[sp]] <- subset(DOF_list_weekly_ratio.5seasons[[sp]][209:260,])
  Ratio2020_21[[sp]] <- list(season2020_21[[sp]]$Ratio)
  NOVANA_list_agg2[[sp]] <- list()
  for(i in 1:52){
    NOVANA_list_agg2[[sp]][[i]] <- NOVANA_list_agg[[sp]] %>%
      mutate(Week =replace(Week, Week == 1, i)) %>%
      mutate(DOF.ratio.counts = yearmean.cell.Antal.NOVANA*Ratio2020_21[[sp]][[1]][i])
  }
  DOF_list.2020_21.weekly[[sp]] <- bind_rows(NOVANA_list_agg2[[sp]])
  
  #assign seasons/epi years
  colnames(DOF_list.2016_17.weekly[[sp]]) <- c("Species", "KN10kmDK", "Seasonal.week", "yearmean.cell.Antal.NOVANA", "DOF.ratio.counts")
  DOF_list.2016_17.weekly[[sp]]$Season <- "2016/17"
  colnames(DOF_list.2017_18.weekly[[sp]]) <- c("Species", "KN10kmDK", "Seasonal.week", "yearmean.cell.Antal.NOVANA", "DOF.ratio.counts")
  DOF_list.2017_18.weekly[[sp]]$Season <- "2017/18"
  colnames(DOF_list.2018_19.weekly[[sp]]) <- c("Species", "KN10kmDK", "Seasonal.week", "yearmean.cell.Antal.NOVANA", "DOF.ratio.counts")
  DOF_list.2018_19.weekly[[sp]]$Season <- "2018/19"
  colnames(DOF_list.2019_20.weekly[[sp]]) <- c("Species", "KN10kmDK", "Seasonal.week", "yearmean.cell.Antal.NOVANA", "DOF.ratio.counts")
  DOF_list.2019_20.weekly[[sp]]$Season <- "2019/20"
  colnames(DOF_list.2020_21.weekly[[sp]]) <- c("Species", "KN10kmDK", "Seasonal.week", "yearmean.cell.Antal.NOVANA", "DOF.ratio.counts")
  DOF_list.2020_21.weekly[[sp]]$Season <- "2020/21"
  
  #row bind all DOF_list counts
  DOF_list_weekly_phenology_counts_in_5seasons[[sp]] <- bind_rows(DOF_list.2016_17.weekly[[sp]], DOF_list.2017_18.weekly[[sp]], DOF_list.2018_19.weekly[[sp]], DOF_list.2019_20.weekly[[sp]], DOF_list.2020_21.weekly[[sp]])
}

setwd(".../DanHPAIwild/Data")
file_names <- c("WS_weekly_phenology_counts_in_5seasons", "BG_weekly_phenology_counts_in_5seasons", "MS_weekly_phenology_counts_in_5seasons", "GG_weekly_phenology_counts_in_5seasons", "M_weekly_phenology_counts_in_5seasons")
for (i in seq_along(DOF_list_weekly_phenology_counts_in_5seasons)) {
  write.csv(DOF_list_weekly_phenology_counts_in_5seasons[[i]], file = paste0(file_names[i], ".csv"), row.names = FALSE)
}
