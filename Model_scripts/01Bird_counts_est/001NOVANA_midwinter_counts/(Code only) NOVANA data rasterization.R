setwd(".../DanHPAIwild")
source("Data/Required pkgs.R")
DKmapsf <- readRDS("Data/Shapefiles/DKmapsf.rds")

#NOVANA data is not publicly available
Raw_NOVANA <- read_excel("Nonpublic data/5speciesNOVANA.xlsx")

#full (all species) dataframe
fulldf <- Raw_NOVANA %>% 
  dplyr::select(Year, Month, OptaelID, Observatoer, Dato
                , ArtDK, Antal, Alder, Latitude, Longitude ) %>% 
  filter(!is.na(Latitude))

#whooper swan----------------------------- 
fulldf_WS <- fulldf %>% 
  filter(ArtDK=="Sangsvane")
#set lat-long-df as a sf
fulldf_WS_sf <- st_as_sf(fulldf_WS, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

int <- sf::st_intersects(fulldf_WS_sf, DKmapsf)
fulldf_WS$KN10kmDK <- DKmapsf$KN10kmDK[unlist(int)]
fulldf_WS$cell_geometry <- DKmapsf$geometry.x[unlist(int)]
#aggregated the number of WS from NOVANA in the same cell
fulldf_WS_agg <-aggregate(x=list(fulldf_WS$Antal),
                          by = list(fulldf_WS$KN10kmDK, fulldf_WS$Year, fulldf_WS$Month),
                          FUN = sum)
#build a dataset of DKmapsf and aggregated numbers in cells
colnames(fulldf_WS_agg)<- c("KN10kmDK", "Year", "Month", "cell.Antal.NOVANA")

fulldf_WS_agg_grid <- merge(DKmapsf, fulldf_WS_agg,by="KN10kmDK")
#filtered rows with the count equal to 0.
fulldf_WS_agg_grid <- fulldf_WS_agg_grid %>% 
  filter(cell.Antal.NOVANA!= c(0)) %>% mutate(Species="WS", .before = KN10kmDK) %>% as.data.frame() %>% select(-geometry)

#barnacle goose----------------------------- 
fulldf_BG <- fulldf %>% 
  filter(ArtDK=="Bramgås")
#set lat-long-df as a sf
fulldf_BG_sf <- st_as_sf(fulldf_BG, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

int <- sf::st_intersects(fulldf_BG_sf, DKmapsf)
fulldf_BG$KN10kmDK <- DKmapsf$KN10kmDK[unlist(int)]
fulldf_BG$cell_geometry <- DKmapsf$geometry.x[unlist(int)]
#aggregated the number of BG from NOVANA in the same cell
fulldf_BG_agg <-aggregate(x=list(fulldf_BG$Antal),
                          by = list(fulldf_BG$KN10kmDK, fulldf_BG$Year, fulldf_BG$Month),
                          FUN = sum)
#build a dataset of DKmapsf and aggregated numbers in cells
colnames(fulldf_BG_agg)<- c("KN10kmDK", "Year", "Month", "cell.Antal.NOVANA")

fulldf_BG_agg_grid <- merge(DKmapsf, fulldf_BG_agg,by="KN10kmDK")
#filtered rows with the count equal to 0.
fulldf_BG_agg_grid <- fulldf_BG_agg_grid %>% 
  filter(cell.Antal.NOVANA!= c(0)) %>% mutate(Species="BG", .before = KN10kmDK) %>% as.data.frame() %>% select(-geometry)

#mute swan----------------------------- 
fulldf_MS <- fulldf %>% 
  filter(ArtDK=="Knopsvane")
#set lat-long-df as a sf
fulldf_MS_sf <- st_as_sf(fulldf_MS, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

int <- sf::st_intersects(fulldf_MS_sf, DKmapsf)
fulldf_MS$KN10kmDK <- DKmapsf$KN10kmDK[unlist(int)]
fulldf_MS$cell_geometry <- DKmapsf$geometry.x[unlist(int)]

#aggregated the number of MS from NOVANA in the same cell
fulldf_MS_agg <-aggregate(x=list(fulldf_MS$Antal),
                          by = list(fulldf_MS$KN10kmDK, fulldf_MS$Year, fulldf_MS$Month),
                          FUN = sum)
#build a dataset of DKmapsf and aggregated numbers in cells
colnames(fulldf_MS_agg)<- c("KN10kmDK", "Year", "Month", "cell.Antal.NOVANA")

fulldf_MS_agg_grid <- merge(DKmapsf, fulldf_MS_agg,by="KN10kmDK")
#filtered rows with the count equal to 0.
fulldf_MS_agg_grid <- fulldf_MS_agg_grid %>%  
  filter(cell.Antal.NOVANA!= c(0)) %>% mutate(Species="MS", .before = KN10kmDK) %>% as.data.frame() %>% select(-geometry)

#greylag goose----------------------------- 
fulldf_GG <- fulldf %>% 
  filter(ArtDK=="Grågås")
#set lat-long-df as a sf
fulldf_GG_sf <- st_as_sf(fulldf_GG, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

int <- sf::st_intersects(fulldf_GG_sf, DKmapsf)
fulldf_GG$KN10kmDK <- DKmapsf$KN10kmDK[unlist(int)]
fulldf_GG$cell_geometry <- DKmapsf$geometry.x[unlist(int)]

#aggregated the number of GG from NOVANA in the same cell
fulldf_GG_agg <-aggregate(x=list(fulldf_GG$Antal),
                          by = list(fulldf_GG$KN10kmDK, fulldf_GG$Year, fulldf_GG$Month),
                          FUN = sum)
#build a dataset of DKmapsf and aggregated numbers in cells
colnames(fulldf_GG_agg)<- c("KN10kmDK", "Year", "Month", "cell.Antal.NOVANA")

fulldf_GG_agg_grid <- merge(DKmapsf, fulldf_GG_agg,by="KN10kmDK")
#filtered rows with the count equal to 0.
fulldf_GG_agg_grid <- fulldf_GG_agg_grid %>%  
  filter(cell.Antal.NOVANA!= c(0)) %>% mutate(Species="GG", .before = KN10kmDK) %>% as.data.frame() %>% select(-geometry)

#mallard ----------------------------- 
fulldf_M <- fulldf %>% 
  filter(ArtDK=="Gråand")
#set lat-long-df as a sf
fulldf_M_sf <- st_as_sf(fulldf_M, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

int <- sf::st_intersects(fulldf_M_sf, DKmapsf)
fulldf_M$KN10kmDK <- DKmapsf$KN10kmDK[unlist(int)]
fulldf_M$cell_geometry <- DKmapsf$geometry.x[unlist(int)]

#aggregated the number of M from NOVANA in the same cell
fulldf_M_agg <-aggregate(x=list(fulldf_M$Antal),
                         by = list(fulldf_M$KN10kmDK, fulldf_M$Year, fulldf_M$Month),
                         FUN = sum)
#build a dataset of DKmapsf and aggregated numbers in cells
colnames(fulldf_M_agg)<- c("KN10kmDK", "Year", "Month", "cell.Antal.NOVANA")

fulldf_M_agg_grid <- merge(DKmapsf, fulldf_M_agg,by="KN10kmDK")
#filtered rows with the count equal to 0.
fulldf_M_agg_grid <- fulldf_M_agg_grid %>%  
  filter(cell.Antal.NOVANA!= c(0)) %>% mutate(Species="M", .before = KN10kmDK) %>% as.data.frame() %>% select(-geometry)

#write the individual species dataset (of 6 years) to excel sheets
require(openxlsx)
list_of_datasets <- list("WS" = fulldf_WS_agg_grid,
                         "BG" = fulldf_BG_agg_grid, 
                         "MS" = fulldf_MS_agg_grid, 
                         "GG" = fulldf_GG_agg_grid, 
                         "M" = fulldf_M_agg_grid)
write.xlsx(list_of_datasets, file = ".../DanHPAIwild/Data/5spe_NOVANA_614cells.xlsx")
















