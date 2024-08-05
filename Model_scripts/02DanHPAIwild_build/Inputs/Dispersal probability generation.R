Weibull_param <- readRDS("Data/DanHPAIwild params/Dispersal params/Fandos et al. Weibull params.rds")
# --- built five Weibull distributions for 5 species ---
Weibull_param_data <-
  Weibull_param %>%
  mutate(data = purrr::map2(mean_parameter1 , mean_parameter2,
                            ~tibble(x = seq(0, 10, length.out = 1000),
                                    y = dweibull(x, .x, .y)))) %>%
  unnest(data)
#2. build function list for 5 species
Disp_kernel <- function(shape, scale) {
  function(x) dweibull(x, shape = shape, scale = scale)
}

Disp_kernel_list <- list(WS=Disp_kernel(Weibull_param$mean_parameter1[5], Weibull_param$mean_parameter2[5]),
                         BG=Disp_kernel(Weibull_param$mean_parameter1[1], Weibull_param$mean_parameter2[1]),
                         MS=Disp_kernel(Weibull_param$mean_parameter1[4], Weibull_param$mean_parameter2[4]),
                         GG=Disp_kernel(Weibull_param$mean_parameter1[2], Weibull_param$mean_parameter2[2]),
                         M=Disp_kernel(Weibull_param$mean_parameter1[3], Weibull_param$mean_parameter2[3]))

#3. -------outside of the loop, calculate the disp_prob
# using dweibull to integrate and obtain dis_prob for i_th species of k_th cell at j+1_th time step
iden_point_list <- readRDS("Data/DanHPAIwild params/Dispersal params/cell identification_each species_with German cells.rds")
dist_point <- readRDS("Data/DanHPAIwild params/Dispersal params/Pairwise distance (dis to the same cell is 5 instead of 0).rds")
speciesi <- c("WS","BG","MS","GG","M")

disp_prob <- iden_point_list
for (i in 1:5) {
  for (s in 1:614) {
    for (k in 1:nrow(disp_prob[[i]])) {
      
      disp_prob[[speciesi[i]]][k, s] <-  ifelse(iden_point_list[[speciesi[i]]][k, s] == 0, 0, 
                                                ifelse(dist_point[[i]][k, s] %% 1 != 0, integrate(Disp_kernel_list[[speciesi[i]]], (dist_point[[i]][, s]-sqrt(2*5^2))[k], (dist_point[[i]][, s]+sqrt(2*5^2))[k])$value, 
                                                       ifelse(k==s, integrate(Disp_kernel_list[[speciesi[i]]], (dist_point[[i]][, s]-5)[k], (dist_point[[i]][, s])[k])$value, integrate(Disp_kernel_list[[speciesi[i]]], (dist_point[[i]][, s]-5)[k], (dist_point[[i]][, s]+5)[k])$value))) 
    }}}# remember, for cells not on the diagonal, the range used in integration was approximate. 
