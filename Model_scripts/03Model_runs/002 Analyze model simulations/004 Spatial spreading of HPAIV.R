setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")

load("Model_scripts/03Model_runs/003 Simulation outputs/Estimated_Baseline_output.rda")
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

#median prevalence
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
Sphelp <- read.csv("Data/Data for output analysis/DKmapsf_ID corrsponds to KN10kmDK.csv")
DKmapsf <- readRDS(file = "Data/Shapefiles/DKmapsf.rds")
sf_DK_Region <- read_sf("Data/Shapefiles/DK-geographical-regions/SHAPEFILE-DK", layer = "REGION")
DK_outline <- read_sf("Data/Shapefiles/DK_shapefile/DK_outline.shp")
DK_outline_UTM <- st_transform(DK_outline, "EPSG:32632")
Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID") #q95_Prevalence

Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
sf_DK_Region_UTM <- st_transform(sf_DK_Region, "EPSG:32632")
Regions <- data.frame(name = c("North Jutland", "Central", "South Denmark", "Zealand", "Capital", "Bornholm"),
                      x = c(9.9182, 9.4, 9.3, 11.7500, 12.3, 14.8756),
                      y = c(57.0291, 56.3, 55.3722, 55.4277, 56, 55.1382)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) 
Regions$name_wrapped <- str_wrap(Regions$name, width = 10)

colors <- c("grey", colorRampPalette(c("yellow", "red"))(20))
limits <- range(0, Spatial.df %>% select(V1:V105))
max(Spatial.df %>% select(V1:V105))
breaks <- seq(0, 1, by=0.1) #1 for Q95

#Figure a
t1=54
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
intersection1 <- st_intersection(DK_outline_UTM, Spatial_UTM1)

plot54 <-  ggplot() +
  geom_sf(data=sf_DK_Region_UTM)+
  geom_sf(data=intersection1, aes(fill=!!sym(column_name1)), alpha=0.5) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  geom_path(color="white") +
  ggthemes::theme_map() +
  scale_fill_gradientn(colors = colors, limits = limits, breaks=breaks, na.value = "black")+
  labs(fill="Prevalence (I/Total)")+
  theme(
    legend.position = c("bottom"),
    legend.title=element_text(size=10),
    legend.key.size = unit(1, "line"),
    legend.key.width = unit(0.8, "cm"),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 15, hjust = 0.1))
# plot54

#Figure e
t2=105
column_name2 <- paste0("V", t2)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM2 <- st_transform(Spatial %>% select(!!sym(column_name2)), "EPSG:32632")
intersection2 <- st_intersection(DK_outline_UTM, Spatial_UTM2)

plot105 <-  ggplot() +
  geom_sf(data=sf_DK_Region_UTM)+
  geom_sf(data=intersection2, aes(fill=!!sym(column_name2)), alpha=0.5) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  geom_path(color="white") +
  ggthemes::theme_map()+
  scale_fill_gradientn(colors = colors, limits = limits, breaks=breaks, na.value = "black")+
  labs(subtitle = "e) End (week 52)", fill="Prevalence (I/Total)")+
  theme(legend.position = "none",
        legend.title=element_blank(),
        legend.text = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.1))
# plot105

#Figure b
t1=67
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
intersection1 <- st_intersection(DK_outline_UTM, Spatial_UTM1)

plot67 <-  ggplot() +
  geom_sf(data=sf_DK_Region_UTM)+
  geom_sf(data=intersection1, aes(fill=!!sym(column_name1)), alpha=0.5) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  geom_path(color="white") +
  ggthemes::theme_map() +
  scale_fill_gradientn(colors = colors, limits = limits, breaks=breaks, na.value = "black")+
  labs(fill="Prevalence (I/Total)")+
  theme(
    legend.title=element_text(size=10),
    legend.key.size = unit(1, "line"),
    legend.key.width = unit(0.8, "cm"),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 15, hjust = 0.1))
# plot67

#Figure c
t1=80
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
intersection1 <- st_intersection(DK_outline_UTM, Spatial_UTM1)

plot80 <-  ggplot() +
  geom_sf(data=sf_DK_Region_UTM)+
  geom_sf(data=intersection1, aes(fill=!!sym(column_name1)), alpha=0.5) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  geom_path(color="white") +
  ggthemes::theme_map() +
  scale_fill_gradientn(colors = colors, limits = limits, breaks=breaks, na.value = "black")+
  labs( fill="Prevalence (I/Total)")+
  theme(
    legend.title=element_text(size=10),
    legend.key.size = unit(1, "line"),
    legend.key.width = unit(0.8, "cm"),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 15, hjust = 0.1))
# plot80

#Figure d
t1=93
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
intersection1 <- st_intersection(DK_outline_UTM, Spatial_UTM1)

plot93 <-  ggplot() +
  geom_sf(data=sf_DK_Region_UTM)+
  geom_sf(data=intersection1, aes(fill=!!sym(column_name1)), alpha=0.5) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  geom_path(color="white") +
  ggthemes::theme_map() +
  scale_fill_gradientn(colors = colors, limits = limits, breaks=breaks, na.value = "black")+
  labs(fill="Prevalence (I/Total)")+
  theme(
    legend.title=element_text(size=10),
    legend.key.size = unit(1, "line"),
    legend.key.width = unit(0.8, "cm"),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 15, hjust = 0.1))
# plot93

#Figure f (confidential information about the coordinates, please see the map in Figure 5)
Detection_raw <- readRDS("Data/Data for output analysis/Surveillance detections_with weeks.rda")
tmp.agg_UTM <- st_as_sf(Detection_raw, coords = c("X", "Y"), crs = 32632)
tmp.agg_UTM <- st_transform(tmp.agg_UTM, "EPSG:32632")
tmp.agg_UTM$HPAIcases <- as.factor(tmp.agg_UTM$HPAIcases)
tmp.agg_UTM$Week <- as.numeric(tmp.agg_UTM$Week)
tmp.agg_UTM <- tmp.agg_UTM %>% mutate(Week=Week-1) %>% mutate(Quater=case_when(
  Week >= 0 & Week < 3*51/12 ~ "Q1",
  Week >= 3*51/12 & Week < 6*51/12 ~ "Q2",
  Week >= 6*51/12 & Week < 9*51/12 ~ "Q3",
  Week >= 9*51/12 ~ "Q4",
))

surveildet <- ggplot() +
  geom_sf(data = sf_DK_Region_UTM) +
  geom_sf(data = tmp.agg_UTM %>% filter(HPAIcases==1), aes(color=Quater), alpha=0.8) +
  geom_sf_text(data = Regions, aes(label = name_wrapped), color="black", size=3)+
  ggthemes::theme_map() +
  scale_color_discrete(name = "Quater")+
  theme(plot.title = element_text(size = 15, hjust = 0.1),
        legend.position = c("right"))
# surveildet

library(cowplot)
legend1 <- get_legend(plot54)

P_simualtion <- plot_grid(plot54 +
            theme(legend.position = "none") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) 
          , plot67 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
            theme(legend.position = "none")
          , plot80 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
            theme(legend.position = "none")
          , plot93 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
            theme(legend.position = "none"), 
          nrow=2,
          labels = c("A. Oct.(Q1)", "B. Jan.(Q2)", "C. Apr.(Q3)", "D. Jul.(Q4)"))

Tmp <- plot_grid(P_simualtion, surveildet, ncol=2, labels = c("", "E. Surveillance detections in 2020/21"))
plot_grid(Tmp, legend1, ncol = 1, rel_heights = c(1, .2))

ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyses on model simulations/Figures/Figure2_Q95_RF.png", width = 12, height = 8, dpi = 300)

