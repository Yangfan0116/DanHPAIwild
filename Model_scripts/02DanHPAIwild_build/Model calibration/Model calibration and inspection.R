setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")
load("Data/Core data/DanHPAIwild parameters_Raw.RData")
remotes::install_github("Yangfan0116/DanHPAIwild")
library(DanHPAIwild)

#Alter 3 parameter values and obtain 64 sets
N_Env_vector <- c(N_Env / 10^9, N_Env / 10^8, N_Env / 10^7, N_Env / 10^6)
ID50_l_vector <- list(lapply(ID50, function(x) x*10), lapply(ID50, function(x) x*100), lapply(ID50, function(x) x*1000), lapply(ID50, function(x) x*10000))
sigma_list_vector <- list(sigma, sigma / 10, sigma / 20, sigma / 30)

#for each set, 10 iterations were simulated to obtain the model output:
xx <- seq_len(4) #ID50
yy <- seq_len(4) #sigma
zz <- seq_len(4) #N_Env

results <- list()
index <- 1
iters <- 10L
for(xx in seq_len(4)){
  for(yy in seq_len(4)){
    for(zz in seq_len(4)){
      results[[index]] <- list()
      for(i in seq_len(iters)){
        results[[index]][[i]] <- DanHPAIwildModel(passur, V_M, ID50_l_vector[[xx]], S_list_A, dD_list_A, V_disp_A, disp_prob_L, gamma, Rec_list_A, dRec_list_A, mf, theta_A_list, sigma_list_vector[[yy]], I_list_A, epsilon, eta, mu, D_list_A, prob_A, N_Env_vector[zz], contact, xi, foraging)
      }
      index <- index + 1
  }
  }
  }

#save(results, file = "Model_scripts/03Model_runs/003 Simulation outputs/calibration results.rda")

calculate_metrics <- function(results, type) {
  m <- matrix(rep(0, 105*614), nrow=614)
  SI2R_bird <- array(unlist(replicate(5, m, simplify = FALSE)), dim = c(614, 105, 10))
  SI2R_DK <- list()
  transposed_data <- matrix(nrow=10, ncol=105, 0)
  for (w in 1:10) {
    SI2R_bird[, , w] <- apply(results[[w]][[paste0(type, '_list_A')]], c(1, 2), sum)
    #sum across DK first, and then take median from 10 iterations
    SI2R_DK[[w]] <- colSums(SI2R_bird[, , w])
    transposed_data[w,] <- (sapply(SI2R_DK[[w]], as.numeric))
  }
  median_SI2R_DK <- apply(transposed_data, 2, median)
  q5_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.05)
  q95_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.95)
  return(list(median = median_SI2R_DK, q5 = q5_SI2R_DK, q95 = q95_SI2R_DK))
}

#organise the outputs (weekly death, infections, and accumulated death)
dD <- lapply(results, function(list){
  calculate_metrics(list, "dD")
})
I<- lapply(results, function(list){
  calculate_metrics(list, "I")
})
D<- lapply(results, function(list){
  calculate_metrics(list, "D")
})

passur <- read.csv("Data/passur_species_matrix.csv", sep = ";") %>% mutate(HPAIcases=WS+BG+MS+GG+M) %>% select(time, HPAIcases)

# Criterion 1: time deviation between the simulated peak and the peak observed in surveillance
I_list <- NULL
for (i in seq_len(64)) {
  I_list[[i]] <- lapply(I[[i]], function(x) x[54:105])
}

data<-as.data.frame(t(do.call(rbind, lapply(I_list, `[[`, 1)))) %>% mutate(id=row_number())
data_long <- data %>% pivot_longer(cols = -id, names_to = "par_combo", values_to = "I") %>% mutate(id=id-1)

breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

ggplot(data_long, aes(x = id, y = I, color = par_combo, group = par_combo)) +
  facet_wrap(~par_combo)+
  geom_point(size=1) +
  geom_line(linewidth=0.5) +
  scale_y_continuous(labels = scales::comma) +
  theme_hc()+
  labs(title = "Multiple Line Plot (10 iters)", x = "Month", y = "I")+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  theme(legend.position = "none")+
  annotate(geom='point', x=seq(0, 51),y=passur$HPAIcases[-1]*1000, color="red4", size=0.8)

ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyze model simulations/Figures/10iter_I using 64 par_combo.png", width = 50, height = 35, units = "cm")

#2. the difference between simulated and real weekly deaths at peak when assuming a 0.2 percent detection effort 

dD_list <- NULL
for (i in seq_len(64)) {
  dD_list[[i]] <- lapply(dD[[i]], function(x) x[54:105])
}
data<-as.data.frame(t(do.call(rbind, lapply(dD_list, `[[`, 1)))) %>% mutate(id=row_number())
data_long <- data %>% pivot_longer(cols = -id, names_to = "par_combo", values_to = "dD") %>% mutate(id=id-1)

dD_list <- NULL
for (i in seq_len(64)) {
  dD_list[[i]] <- lapply(dD[[i]], function(x) x[54:105])
}
median_dD_list <- lapply(dD_list, `[[`, 1)
peaktime_dD <- lapply(median_dD_list, function(df) {which.max(df)})
dist_peak <- unlist(lapply(peaktime_dD, function(x) abs(22-x)))
which.min(dist_peak)

# Criterion 2: the visual inspection of temporal trajectories between simulated and detected weekly deaths
N_Env <- N_Env_vector
tmp <- unlist(ID50_l_vector); ID50 <- c(mean(tmp[1:5]), mean(tmp[6:10]), mean(tmp[11:15]), mean(tmp[16:20]))
sigma <- sapply(sigma_list_vector, mean)

dD_list <- lapply(lapply(dD_list, `[[`, 1), function(x) max(x))
combos3 <- expand.grid(N_Env = N_Env, sigma = sigma, ID50 = ID50)
combos3$Results <- unlist(dD_list)
write.csv(combos3, "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/02DanHPAIwild_build/Model calibration/10iters_Max_dD (64 para sets).csv")


breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

ggplot(data_long, aes(x = id, y = dD, color = par_combo, group = par_combo)) +
  facet_wrap(~par_combo)+
  geom_point(size=1) +
  geom_line(linewidth=0.5) +
  scale_y_continuous(labels = scales::comma) +
  theme_hc()+
  labs(title = "Multiple Line Plot (10 iters)", x = "Month", y = "dD")+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  theme(legend.position = "none")+
  annotate(geom='point', x=seq(0, 51),y=passur$HPAIcases[-1]*500, color="red4", size=0.8)
ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyze model simulations/Figures/10iter_dD using 64 par_combo.png", width = 50, height = 35, units = "cm")

library(plotly)

Peak_list<- lapply(lapply(I_list, `[[`, 1), function(x) max(x))

combos <- expand.grid(N_Env = N_Env, sigma = sigma, ID50 = ID50)
combos$Results <- unlist(Peak_list)
combos_log <- combos%>%
  mutate(across(.cols = -sigma, .fns = ~log(.x+1)))
print(combos_log)

scattered_plot_I <- plot_ly(
  data = combos, 
  x = ~log(N_Env+1), 
  y = ~log(ID50+1), 
  z = ~sigma, 
  marker = list(color = ~Results, colorscale = c("blue", "red"), showscale = TRUE)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = 'log(N_Env+1)'),
      yaxis = list(title = 'Mean log(ID50+1)'),
      zaxis = list(title = 'Mean sigma')
    ),
    annotations = list(
      x = 1.13,
      y = 1.03,
      text = 'Peak number of infectious birds',
      showarrow = FALSE
    ))
scattered_plot_I

#Criterion 3: the visual inspection of temporal trajectories between simulated and detected weekly deaths
D_list <- NULL
for (i in seq_len(64)) {
  D_list[[i]] <- lapply(D[[i]], function(x) x[53:103])
}
D_list <- lapply(lapply(D_list, `[[`, 1), function(x) max(x))
combos2 <- expand.grid(N_Env = N_Env, sigma = sigma, ID50 = ID50)
combos2$Results <- unlist(D_list)

scattered_plot_D <- plot_ly(
  data = combos2, 
  x = ~log(N_Env+1), 
  y = ~log(ID50+1), 
  z = ~sigma, 
  marker = list(color = ~Results, colorscale = c("blue", "red"), showscale = TRUE)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = 'log(N_Env+1)'),
      yaxis = list(title = 'Mean log(ID50+1)'),
      zaxis = list(title = 'Mean sigma')
    ),
    annotations = list(
      x = 1.13,
      y = 1.03,
      text = 'Cumulative number of dead birds',
      showarrow = FALSE
    ))
scattered_plot_D

