#Obtain the variable B_{c,w}: compensational volunteers’ observations from DOFbasen data
setwd(".../DanHPAIwild")
source("Data/Required pkgs.R")
DKmapsf <- readRDS("Data/Shapefiles/DKmapsf.rds")

#read DOF data
files <- list.files("Data", pattern = "\\.csv$")
desired_order <- c("Whooper Swan max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Barnacle Goose max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Mute Swan max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Greylag Goose max count pr day DOFbasen 10x10km 2016 - 2021 .csv", "Mallard max count pr day DOFbasen 10x10km 2016 - 2021 .csv")
files <- files[match(desired_order, basename(files))]
file_names <- c("WS", "BG", "MS", "GG", "M")

#load DOF data of 5 species per day in 2016-2021
setwd(".../DanHPAIwild/Data")
DOF_list <- lapply(files, function(file) read.csv(file, sep = ";"))

#Give the simulation time step for each observation
a <- data.frame(time=c(201639:201652, 201701:201752, 201801:201852, 201901:201952, 202001:202052, 202101:202138), week=c(0:259))
df_struc <- data.frame(KN10kmDK = rep(c(unique(DKmapsf$KN10kmDK)), 52*5),
                       isoweek = c(rep(c(rep(39:52, each = 614), rep(1:38, each = 614)), 5)), 
                       isoyear = c(rep(2016, 14*614), rep(2017, 52*614), rep(2018, 52*614), rep(2019, 52*614), rep(2020, 52*614), rep(2021, 38*614))) %>%  mutate(time=isoyear*100+isoweek)

DOF_list_agg_week <- list(); DOFaggweek<-list()
for(sp in seq_along(1:5)){
  DOF_list[[sp]]$date <- as.Date(DOF_list[[sp]]$date)
  DOF_list[[sp]]  <- DOF_list[[sp]]%>% filter(between(date, as.Date('2016-09-26'), as.Date('2021-09-26')))
  
  DOF_list[[sp]] <- DOF_list[[sp]] %>%
    mutate(isoweek=lubridate::isoweek(date)) %>%
    mutate(isoyear=lubridate::isoyear(date)) %>%
    mutate(isoweek= ifelse(isoweek==53 & lubridate::month(date) == 12,52, if_else(isoweek==53 & lubridate::month(date) == 1, 1, isoweek))) %>% mutate(isoyear = ifelse(lubridate::year(date)!=isoyear,lubridate::year(date),isoyear)) %>%
    unite("week", isoyear:isoweek, remove = F ) %>% mutate(time= isoyear*100+isoweek)
  DOF_list[[sp]]$isoyear <- as.numeric(DOF_list[[sp]]$isoyear)
  DOF_list[[sp]]$isoweek <- as.numeric(DOF_list[[sp]]$isoweek)
  
  DOF_list_agg_week[[sp]] <- aggregate(DOF_list[[sp]]$max_count, by=list(DOF_list[[sp]]$species, DOF_list[[sp]]$time, DOF_list[[sp]]$KN10kmDK), FUN=sum)
  colnames(DOF_list_agg_week[[sp]]) <- c("species", "time", "KN10kmDK", "DOF max count")
  
  DOFaggweek[[sp]] <- left_join(DOF_list_agg_week[[sp]], a, by='time')
  DOFaggweek[[sp]] <- left_join(df_struc, DOFaggweek[[sp]], by=c("KN10kmDK", "time"))
  
  write.csv(DOFaggweek[[sp]], file = paste0(".../DanHPAIwild/Data/", file_names[sp], "Predictor DOF column (590cells).csv"))
}

