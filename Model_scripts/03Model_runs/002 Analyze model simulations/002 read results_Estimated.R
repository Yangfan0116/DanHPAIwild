setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")

load("Model_scripts/03Model_runs/003 Simulation outputs/Estimated_Baseline_output.rda")

calculate_metrics <- function(results, type) {
  m <- matrix(rep(0, 105*614), nrow=614)
  SI2R_bird <- array(unlist(replicate(5, m, simplify = FALSE)), dim = c(614, 105, 400))
  SI2R_DK <- list()
  transposed_data <- matrix(nrow=400, ncol=105, 0)
  for (w in 1:400) {
    SI2R_bird[, , w] <- apply(results[[w]][[paste0(type, '_list_A')]], c(1, 2), sum)
    #sum across DK first, and then take median from 400 iterations
    SI2R_DK[[w]] <- colSums(SI2R_bird[, , w])
    transposed_data[w,] <- (sapply(SI2R_DK[[w]], as.numeric))
  }
  median_SI2R_DK <- apply(transposed_data, 2, median)
  q5_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.05)
  q95_SI2R_DK <- apply(transposed_data, 2, quantile, probs=0.95)
  return(list(median = median_SI2R_DK, q5 = q5_SI2R_DK, q95 = q95_SI2R_DK))
}
S <- calculate_metrics(results, "S")
I <- calculate_metrics(results, "I")
Rec <- calculate_metrics(results, "Rec")
dRec <- calculate_metrics(results, "dRec")
D <- calculate_metrics(results, "D")
dD <- calculate_metrics(results, "dD")

#load total bird
load("Data/M595_estimated_Totalbird_2ConseYear.rds")
time <- 1:105
sim400 <- data.frame(Time=time,
                     median_S=S$median, q5_S=S$q5, q95_S=S$q95,
                     median_I=I$median, q5_I=I$q5, q95_I=I$q95,
                     median_D=D$median, q5_D=D$q5, q95_D=D$q95,
                     median_dD=dD$median, q5_dD=dD$q5, q95_dD=dD$q95,
                     median_R=Rec$median, q5_R=Rec$q5, q95_R=Rec$q95,
                     median_dR=dRec$median, q5_dR=dRec$q5, q95_dR=dRec$q95,
                     median_Total_bird=median_Total_bird, q5_Total_bird=q5_Total_bird, q95_Total_bird=q95_Total_bird) %>%
  slice(-1) %>%mutate(Time=0:103) %>%
  pivot_longer(cols = c(median_S, median_I, median_D, median_dD, median_R, median_dR, median_Total_bird),
               names_to = "Compartment",
               values_to = "median")
sim400$Compartment <- factor(sim400$Compartment , levels = c("median_S", "median_I", "median_D", "median_dD", "median_R", "median_dR", "median_Total_bird"))
new_rows <- list()
for (i in 1:nrow(sim400)) {
  Time <- sim400$Time[i]
  Compartment <- as.character(sim400$Compartment[i])
  median <- sim400$median[i]
  q5_col <- paste0("q5_", substr(Compartment, 8, nchar(Compartment)))
  q95_col <- paste0("q95_", substr(Compartment, 8, nchar(Compartment)))
  q5 <- sim400[i, q5_col]
  q95 <- sim400[i, q95_col]
  new_row <- c(Time, Compartment, median, q5, q95)
  new_rows[[i]] <- new_row
}
sim400.new <- do.call(rbind, new_rows)
colnames(sim400.new) <- c("Time", "Compartment", "median", "q5", "q95")
sim400.new <- as.data.frame(sim400.new)
replace_values <- function(x) {
  x <- gsub("median_S", "S", x)
  x <- gsub("median_I", "I", x)
  x <- gsub("median_D", "D", x)
  x <- gsub("median_dD", "dD", x)
  x <- gsub("median_R", "R", x)
  x <- gsub("median_dR", "dR", x)
  x <- gsub("median_Total_bird", "Total", x)
  return(x)
}
sim400.new <-  sim400.new %>%
  mutate_all(replace_values) %>%
  mutate_at(vars(median, q5, q95, Time), as.numeric)
sim400.new$Compartment <- factor(sim400.new$Compartment , levels = c("S", "I", "D", "dD", "R", "dR", "Total"))
sim400.new <- sim400.new %>% filter(Time %in% c(52:103)) %>% mutate(Time = Time - 52)

passur <- read.csv("Data/DanHPAIwild params/passur_species_matrix.csv", sep = ";") %>% mutate(HPAIcases=WS+BG+MS+GG+M) %>% select(time, HPAIcases)

#facet plots with detections
breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
p <- ggplot(sim400.new, aes(x = Time, y = median)) +
  facet_wrap(~factor(Compartment, levels=c('S', 'I', 'D', 'dD', 'R', 'dR', 'Rem', 'dRem', 'Total')), scales = "free_y", labeller = as_labeller(c("S" = "a. Susceptible", "I" = "b. Infectious", "D" = "c. Dead", "dD" = "d. Dead/week", "R" = "e. Recovered", "dR" = "f. Recovered/week", "Total" = "g. Total"))) +
  geom_point(size=1) +
  geom_line(linewidth=1) +
  geom_ribbon(data = sim400.new, aes(ymin = q5, ymax = q95, group = Compartment), alpha = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  theme_hc()+
  labs(y="Bird counts")+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(x = "Month") +
  theme(legend.position = "none")+
  # scale_color_manual(values=c("S"="#009E73", "I"="#D55E00", "D"="#581845", "dD"="#BB8FCE", "R"="#56B4E9", "dR"="#33FFC4", "Rem"="#CC79A7", "dRem"="#999999", "Total"="black"))+
  geom_point(data = data.frame(median = passur$HPAIcases[-1]*1000, Time = seq(0, 51), Compartment = "dD"), colour="red4")
p
ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyses on model simulations/Figures/RF_Baseline model facet plot with passur.png", width = 12, height = 5, dpi = 400)
