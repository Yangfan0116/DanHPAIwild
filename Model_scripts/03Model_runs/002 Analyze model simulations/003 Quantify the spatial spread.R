setwd(".../DanHPAIwild")
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

#quantification the spread
Spatial.df <- as.data.frame(Median_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=54
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V54!=0)
nrow(poscells)/nrow(Spatial_UTM1) #1, 0.001628664
mean(poscells$V54) #0.002958008
##5%
Spatial.df <- as.data.frame(q5_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=54
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V54!=0)
nrow(poscells)/nrow(Spatial_UTM1) #0 cells, 0
mean(poscells$V54) #0
##95%
Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=54
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V54!=0)
nrow(poscells)/nrow(Spatial_UTM1) ##189 cells, 0.3078176
mean(poscells$V54) #0.01738743

t2=67
Spatial.df <- as.data.frame(Median_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
column_name2 <- paste0("V", t2)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM2 <- st_transform(Spatial %>% select(!!sym(column_name2)), "EPSG:32632")
poscells <- Spatial_UTM2 %>% filter(V67!=0)
nrow(poscells)/nrow(Spatial_UTM2) #112 cells, 0.1824104
mean(poscells$V67) #0.02123868
##5%
Spatial.df <- as.data.frame(q5_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=67
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V67!=0)
nrow(poscells)/nrow(Spatial_UTM1) #0
mean(poscells$V67) #0
##95%
Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=67
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V67!=0)
nrow(poscells)/nrow(Spatial_UTM1) #443 cells, 0.7214984
mean(poscells$V67) #0.02794584

t2=80
Spatial.df <- as.data.frame(Median_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
column_name2 <- paste0("V", t2)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM2 <- st_transform(Spatial %>% select(!!sym(column_name2)), "EPSG:32632")
poscells <- Spatial_UTM2 %>% filter(V80!=0)
nrow(poscells)/nrow(Spatial_UTM2) #86 cells, 0.1400651
mean(poscells$V80) #0.01434541
##5%
Spatial.df <- as.data.frame(q5_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=80
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V80!=0)
nrow(poscells)/nrow(Spatial_UTM1) #0
mean(poscells$V80) #0
##95%
Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=80
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V80!=0)
nrow(poscells)/nrow(Spatial_UTM1) #348 cells, 0.5667752
mean(poscells$V80) #0.01478233

t2=93
Spatial.df <- as.data.frame(Median_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
column_name2 <- paste0("V", t2)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM2 <- st_transform(Spatial %>% select(!!sym(column_name2)), "EPSG:32632")
poscells <- Spatial_UTM2 %>% filter(V93!=0)
nrow(poscells)/nrow(Spatial_UTM2) #41 cells, 0.06677524
mean(poscells$V93) #0.0903058
##5%
Spatial.df <- as.data.frame(q5_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=93
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V93!=0)
nrow(poscells)/nrow(Spatial_UTM1) #0
mean(poscells$V93) #0
##95%
Spatial.df <- as.data.frame(q95_Prevalence) %>% mutate(ID=c(1:614), .before=1) %>%
  left_join(Sphelp[, 4:5], by="ID")
Spatial <- left_join(DKmapsf, Spatial.df, by=c("KN10kmDK"="DKmapsf.KN10kmDK"))
t1=93
column_name1 <- paste0("V", t1)  # Generate the column name (e.g., "V1" or "V2")
Spatial_UTM1 <- st_transform(Spatial %>% select(!!sym(column_name1)), "EPSG:32632")
poscells <- Spatial_UTM1 %>% filter(V93!=0)
nrow(poscells)/nrow(Spatial_UTM1) #191 cells, 0.3110749
mean(poscells$V93) #0.04872713
