setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")
load("Model_scripts/03Model_runs/003 Simulation outputs/Estimated_sa_list.rda")

passur <- read.csv("Data/passur_species_matrix.csv", sep = ";") %>% mutate(HPAIcases=WS+BG+MS+GG+M) %>% select(time, HPAIcases)

sa_calculation <- function(sa_list){
  m <- matrix(rep(0, 105*614), nrow=614)
  sa_bird <- array(unlist(replicate(5, m, simplify = FALSE)), dim = c(614, 105, 400))
  sa_bird <- list(Dead_one=sa_bird, Dead_two=sa_bird, Dead_three=sa_bird, Dead_four=sa_bird, Dead_five=sa_bird, Dead_six=sa_bird, Dead_seven=sa_bird, Dead_base=sa_bird)
  m2 <- matrix(rep(0, 105*400), nrow=400)
  colsum_DK <- list(one=m2, two=m2, three=m2, four=m2, five=m2, six=m2, seven=m2, base=m2)
  transposed_data <- colsum_DK
  median_sa_DK <- list()
  q5_sa_DK <- list()
  q95_sa_DK <- list()
    for (n in seq_along(sa_bird)) {
      for(w in 1:400){
        sa_bird[[n]][, , w]  <- apply(sa_list[[n]][[w]]$dD_list_A, c(1, 2), sum)
        #sum across DK first, and then take median from 400 iterations
        colsum_DK[[n]][w,] <- colSums(sa_bird[[n]][, , w])
        transposed_data[[n]][w,] <- sa_DK[[n]][w,]
      }
      median_sa_DK[[n]] <- apply(transposed_data[[n]], 2, median)
      q5_sa_DK[[n]] <- apply(transposed_data[[n]], 2, quantile, probs=0.05)
      q95_sa_DK[[n]] <- apply(transposed_data[[n]], 2, quantile, probs=0.95)
    }
  return(list(median = median_sa_DK, q5 = q5_sa_DK, q95 = q95_sa_DK))
}

calculate_metrics <- function(results) {
  m <- matrix(rep(0, 105*614), nrow=614)
  SI2R_bird <- array(unlist(replicate(5, m, simplify = FALSE)), dim = c(614, 105, 400))
  SI2R_DK <- list()
  transposed_data <- matrix(nrow=400, ncol=105, 0)
  for (w in 1:400) {
    SI2R_bird[, , w] <- apply(results[[w]]$dD_list_A, c(1, 2), sum)
    #sum across DK first, and then take median from 400 iterations
    SI2R_DK[[w]] <- colSums(SI2R_bird[, , w])
    transposed_data[w,] <- (sapply(SI2R_DK[[w]], as.numeric))
  }
  median_SI2R_DK <- apply(transposed_data, 2, median)
  q5_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.05)
  q95_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.95)
  return(list(median = median_SI2R_DK, q5 = q5_SI2R_DK, q95 = q95_SI2R_DK))
}

sim.dD <- list()
sim.dD[[1]] <- calculate_metrics(sa_list[[1]])
sim.dD[[2]] <- calculate_metrics(sa_list[[2]])
sim.dD[[3]] <- calculate_metrics(sa_list[[3]])
sim.dD[[4]] <- calculate_metrics(sa_list[[4]])
sim.dD[[5]] <- calculate_metrics(sa_list[[5]])
sim.dD[[6]] <- calculate_metrics(sa_list[[6]])
sim.dD[[7]] <- calculate_metrics(sa_list[[7]])
sim.dD[[8]] <- calculate_metrics(sa_list[[8]])

for(n in seq_along(sim.dD)){
  sim.dD[[n]] <- data.frame(Time=c(0:52), median=sim.dD[[n]]$median[-c(2:53)], q5=sim.dD[[n]]$q5[-c(2:53)], q95=sim.dD[[n]]$q95[-c(2:53)], Detected=passur$HPAIcases)
}

sim.dD_name <- lapply(seq_along(sim.dD), function(i) {
  mutate(sim.dD[[i]], sa = i)
})
combined_dD <- do.call(rbind, sim.dD_name)
# save(combined_dD, file="Model_scripts/03Model_runs/Simulation outputs/SAcombined_dD.rda")

breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

tmp <- combined_dD %>% filter(sa==8)
base=tmp$median
ggplot(combined_dD %>% filter(sa!=8), aes(x = Time, y = median, group=sa, color=factor(sa)))+
  # geom_ribbon(aes(ymin = q5, ymax = q95, group=sa, fill=factor(sa)), alpha=0.3)+
  geom_line(linewidth=1)+
  theme_bw()+
  facet_wrap(~sa, ncol=4, labeller=as_labeller(c("1"="ID50 1/10-fold", "2"="ID50, 10-fold", "3"="Conta. rate of D, 0",  "4"="Conta. rate of D, halved", "5"="Peak at Oct.", "6"="Peak at Dec.", "7"="Peak at Jan.")))+
  annotate(geom='line', x=seq(0, 52),y=base, size=1, alpha=0.6)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 150000, by=25000))+
  labs(x = "Month", y = "Dead birds per week", color="Sensitivity analysis scenario") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=10), axis.text.y.left = element_text(size=10))

ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyze model simulations/Figures/SA facet plot.png", width = 12, height = 5, dpi = 300)

#calculate reduction level from Oct to April
three <- combined_dD %>% filter(sa==3)
eight <- combined_dD %>% filter(sa==8)
(sum(eight$median[1:26])-sum(three$median[1:26]))/sum(eight$median[1:26])

#calculate for 4 and 8: the average reduction in deaths
four <- combined_dD %>% filter(sa==4)
eight <- combined_dD %>% filter(sa==8)
(sum(eight$median)-sum(four$median))/sum(eight$median)

#calculate for 5 and 8: the average reduction in deaths
five <- combined_dD %>% filter(sa==5)
eight <- combined_dD %>% filter(sa==8)
(sum(eight$median)-sum(five$median))/sum(eight$median)

