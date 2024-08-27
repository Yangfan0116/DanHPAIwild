setwd(".../DanHPAIwild")
source("Data/Required pkgs.R")
DKmapsf <- readRDS(file = "Data/Shapefiles/DKmapsf.rds")

load("Model_scripts/03Model_runs/003 Simulation outputs/Estimated_Baseline_output.rda")
#prevalence
#prevalence
m <- matrix(rep(0, 105*614), nrow=614)
S_bird <- array(unlist(list(m ,m ,m ,m, m)), dim = (c(614, 105, 400)))
I_bird <- S_bird
R_bird <- S_bird
for(w in 1:400){
  S_bird[, , w] <- apply(results[[w]]$S_list_A , c(1, 2), sum)
}
for(w in 1:400){
  I_bird[, , w] <- apply(results[[w]]$I_list_A , c(1, 2), sum)
}
for(w in 1:400){
  R_bird[, , w] <- apply(results[[w]]$Rec_list_A , c(1, 2), sum)
}

median_I_kj <- apply(I_bird, c(1, 2), median)
q5_I_kj <- apply(I_bird, c(1, 2),  quantile, probs = 0.05)
q95_I_kj <- apply(I_bird, c(1, 2),  quantile, probs = 0.95)

Total_kj <- S_bird + I_bird + R_bird

Prevalence <- I_bird/Total_kj
Prevalence[is.na(Prevalence)] <- 0

Median_Prevalence <- apply(Prevalence, c(1, 2), median)
q5_Prevalence <- apply(Prevalence, c(1, 2), quantile, probs = 0.05)
q95_Prevalence <- apply(Prevalence, c(1, 2), quantile, probs = 0.95)



setwd(".../DanHPAIwild")
Sphelp <- read.csv("Data/Data for output analysis/DKmapsf_ID corrsponds to KN10kmDK.csv")
DKmapsf <- readRDS(file = "Data/Shapefiles/DKmapsf.rds")
sf_DK_Region <- read_sf("Data/Shapefiles/DK-geographical-regions/SHAPEFILE-DK", layer = "REGION")
DK_outline <- read_sf("Data/Shapefiles/DK_shapefile/DK_outline.shp")
DK_outline_UTM <- st_transform(DK_outline, "EPSG:32632")

Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID") #q95_Prevalence

Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))

my_palette <- c("grey", colorRampPalette(c("yellow", "red"))(20))

q95_Prevalence <- lapply(c(2:105), function(w) {
  q95_Prevalence_weekw <- left_join(DKmapsf %>% mutate(id=row_number()), as.data.frame(q95_Prevalence[, w]) %>% mutate(id=row_number()), by="id")
  Spatial_UTM <- st_transform(q95_Prevalence_weekw, "EPSG:32632")
  intersection <- st_intersection(DK_outline_UTM, Spatial_UTM)
  
  tm_shape(DK_outline_UTM)+
    tm_polygons()+
    tm_shape(sf_DK_Region)+
    tm_polygons()+
    tm_shape(intersection)+
    tm_polygons('q95_Prevalence...w.', palette = my_palette, title= "Prevalence(I/total)", style = "cont", colorNA=NULL, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), alpha = 0.5)+
    tm_layout(frame = F, title = paste0("Epi week ", w-1), title.size = 0.6)
})

tmap_animation(q95_Prevalence, filename = ".../DanHPAIwild/Model_scripts/03Model_runs/002 Analyze model simulations/Figures/Prevalence_changes.mp4", width = 3000, height = 2000, dpi=300, delay = 50)
