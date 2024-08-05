files <- list.files("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw", pattern = "\\.csv$")
desired_order <- c("WS_weekly_phenology_counts_in_5seasons.csv", "BG_weekly_phenology_counts_in_5seasons.csv", "MS_weekly_phenology_counts_in_5seasons.csv", "GG_weekly_phenology_counts_in_5seasons.csv", "M_weekly_phenology_counts_in_5seasons.csv")
files <- files[match(desired_order, basename(files))]
#read DOF data of 5 species per day in 2016-2021
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw")
GLMMdf_list <- lapply(files, function(file) read.csv(file))

DKmapsf <- readRDS("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Shapefiles/DKmapsf.rds")

df_struc <- data.frame(KN10kmDK = rep(c(unique(DKmapsf$KN10kmDK)), 52*5),
                       isoweek = c(rep(c(rep(39:52, each = 614), rep(1:38, each = 614)), 5)), 
                       isoyear = c(rep(2016, 14*614), rep(2017, 52*614), rep(2018, 52*614), rep(2019, 52*614), rep(2020, 52*614), rep(2021, 38*614))) %>% group_by(KN10kmDK, isoyear, isoweek) %>% arrange(KN10kmDK)

GLMMdf_list_2 <- list()
for (sp in seq_along(1:5)){
  GLMMdf_list[[sp]]$Seasonal.week <- as.numeric(GLMMdf_list[[sp]]$Seasonal.week)
  GLMMdf_list_2[[sp]] <- GLMMdf_list[[sp]] %>% 
    mutate(isoyear = ifelse(Seasonal.week < 15, str_extract(Season, '\\d+'), paste0("20",str_extract(Season, "(\\w*\\s)?\\w*$")))) %>% 
    mutate(isoweek = ifelse(Seasonal.week < 15, Seasonal.week+38, Seasonal.week-14))
  GLMMdf_list_2[[sp]]$isoweek <- as.integer(GLMMdf_list_2[[sp]]$isoweek)
  GLMMdf_list_2[[sp]]$isoyear <- as.integer(GLMMdf_list_2[[sp]]$isoyear) 
  
  GLMMdf_list_2[[sp]] <- left_join(df_struc, GLMMdf_list_2[[sp]], by=c('KN10kmDK', 'isoweek', 'isoyear'))
}
GLMMdf_list <- GLMMdf_list_2

DKmapsf <- readRDS("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Shapefiles/DKmapsf.rds")

WaterbirdTrip <- read.csv('H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Data/13102022Timme/Anatidae trips per 10x10 km square per day 2016-2021.csv', sep=";")
WaterbirdTrip$date <- as.Date(WaterbirdTrip$date, format = "%Y-%m-%d")
WaterbirdTrip <- WaterbirdTrip %>% filter(between(date, as.Date('2016-09-26'), as.Date('2021-09-26')))
a <- WaterbirdTrip %>%
  mutate(isoweek=lubridate::isoweek(date)) %>% 
  mutate(isoyear=lubridate::isoyear(date)) %>% 
  mutate(isoweek= ifelse(isoweek==53 & lubridate::month(date) == 12,52, if_else(isoweek %in% c(53, 52) & lubridate::month(date) == 1, 1, isoweek))) %>% mutate(isoyear = ifelse(lubridate::year(date)!=isoyear,lubridate::year(date),isoyear )) %>%
  mutate(isoyear=as.numeric(as.character(isoyear)), isoweek=as.numeric(isoweek)) %>% mutate(time= isoyear*100+isoweek)
a_agg <- aggregate(x=list(a$anatidae_trips),
                   by = list(a$KN10kmDK, a$time),
                   FUN = sum)
colnames(a_agg)<- c("KN10kmDK", "time", "anatidae_trips")
a_agg$anatidae_trips[a_agg$anatidae_trips >0] <- 1
a_agg <- a_agg %>% group_by(time, KN10kmDK)
a_agg <- right_join(a_agg, DKmapsf, by="KN10kmDK") %>% select(KN10kmDK, time, anatidae_trips)

a_agg <- right_join(a_agg, GLMMdf_list[[1]] %>% mutate(time=isoyear*100+isoweek), by=c('KN10kmDK', 'time')) %>% select(KN10kmDK, time, anatidae_trips)
a_agg$anatidae_trips <- if_else(is.na(a_agg$anatidae_trips), 0, ceiling(a_agg$anatidae_trips))
list_name <- c('WS', 'BG', 'MS', 'GG', 'M')

#5 species
zerocheck <- list(); tmp<-list(); count_trip <- list(); finalGLMMdf_list <- list()
for(sp in 1:5) {
  zerocheck[[sp]] <- GLMMdf_list[[sp]] %>% select(KN10kmDK, DOF.ratio.counts, isoyear, isoweek) %>%  group_by(isoyear, isoweek) %>% group_by(KN10kmDK) %>% mutate(isoyear=as.numeric(as.character(isoyear)), isoweek=as.numeric(isoweek)) %>% mutate(time= isoyear*100+isoweek)
  
  zerocheck[[sp]]$DOF.ratio.counts <- if_else(is.na(zerocheck[[sp]]$DOF.ratio.counts), 0, ceiling(zerocheck[[sp]]$DOF.ratio.counts))
  
  #find out true zeros
  tmp[[sp]] <- zerocheck[[sp]] %>% mutate(birdcount=if_else(DOF.ratio.counts==0, 0, 1))
  count_trip[[sp]] <- left_join(a_agg, tmp[[sp]], by=c("KN10kmDK", "time")) %>%
    mutate(zerotype = case_when(
      birdcount==0 & anatidae_trips==0 ~ 1,
      TRUE ~ 0
    )) %>% 
    select(KN10kmDK, time, zerotype)
  
  finalGLMMdf_list[[sp]] <- left_join(zerocheck[[sp]], count_trip[[sp]], by=c("KN10kmDK", "time")) %>% select(KN10kmDK, time, DOF.ratio.counts, zerotype) %>% mutate(counts = case_when(
    zerotype==1 & DOF.ratio.counts==0 ~ 0,
    zerotype==0 & DOF.ratio.counts==0 ~ NA,
    TRUE ~ DOF.ratio.counts
  ))
  write.csv(finalGLMMdf_list[[sp]], paste0("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Data/Bird counts/Bird counts raw/GLMM/", list_name[sp], "full_idenTrue0.csv"))
}
