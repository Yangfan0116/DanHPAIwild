setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")
load("Model_scripts/03Model_runs/003 Simulation outputs/Estimated_Baseline_output.rda")

# Use weekly death as the indication
dD <- matrix(0, ncol=5, nrow=105)
speciespecific_Dead <- array(unlist(list(dD, dD, dD, dD, dD)), dim = (c(105, 5, 400)))

for(w in 1:400){
    speciespecific_Dead[,, w] <- apply(results[[w]]$dD_list_A , c(2,3), sum)
}

median <- apply(speciespecific_Dead, MARGIN = c(1,2), median)
q5 <- apply(speciespecific_Dead, MARGIN = c(1,2),  quantile, probs = 0.05)
q95 <- apply(speciespecific_Dead, MARGIN = c(1,2),  quantile, probs = 0.95)
qmin <- apply(speciespecific_Dead, MARGIN = c(1,2), min)
qmax <- apply(speciespecific_Dead, MARGIN = c(1,2), max)
passur <- read.csv("Data/passur_species_matrix.csv", sep = ";")

WS_sim.dD <- data.frame(Time=c(0:52),
                          WSmedian=median[-c(2:53), 1], WSq5=q5[-c(2:53), 1], WSq95=q95[-c(2:53), 1], Detected=passur$WS)
BG_sim.dD <- data.frame(Time=c(0:52),
                          BGmedian=median[-c(2:53), 2], BGq5=q5[-c(2:53), 2], BGq95=q95[-c(2:53), 2], Detected=passur$BG)
MS_sim.dD <- data.frame(Time=c(0:52),
                          MSmedian=median[-c(2:53), 3], MSq5=q5[-c(2:53), 3], MSq95=q95[-c(2:53), 3], Detected=passur$MS)
GG_sim.dD <- data.frame(Time=c(0:52),
                          GGmedian=median[-c(2:53), 4], GGq5=q5[-c(2:53), 4], GGq95=q95[-c(2:53), 4], Detected=passur$GG)
M_sim.dD <- data.frame(Time=c(0:52),
                         Mmedian=median[-c(2:53), 5], Mq5=q5[-c(2:53), 5], Mq95=q95[-c(2:53), 5], Detected=passur$M)

WScoef <- 10*(sum(WS_sim.dD$Detected))/(sum(WS_sim.dD$WSmedian))
BGcoef <- 10*(sum(BG_sim.dD$Detected))/(sum(BG_sim.dD$BGmedian))
MScoef <- 10*(sum(MS_sim.dD$Detected))/(sum(MS_sim.dD$MSmedian))
GGcoef <- 10*(sum(GG_sim.dD$Detected))/(sum(GG_sim.dD$GGmedian))
Mcoef <- 10*(sum(M_sim.dD$Detected))/(sum(M_sim.dD$Mmedian))

breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

WS <- ggplot(WS_sim.dD, aes(x = Time)) +
  geom_line(aes(y = WSmedian), linewidth=1) +
  geom_point(aes(y = WSmedian), size=1)+
  geom_ribbon(aes(ymin = WSq5, ymax = WSq95), alpha = 0.3) +
  geom_point(aes(y = Detected / (1/3.5*WScoef)), color="red3")+
  scale_y_continuous(
    name = "Deaths / week",
    sec.axis = sec_axis(trans = ~.*1/3.5*WScoef, name = "Surveillance detection"), labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(title = "WS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), axis.title.x = element_blank(), axis.title.y.right = element_text(colour = "red3", size=8), axis.title.y.left = element_text(size=8)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=8), axis.text.y.left = element_text(size=8), plot.title = element_text(size=8))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))+  theme_linedraw()
WS

BG <- ggplot(BG_sim.dD, aes(x = Time)) +
  geom_line(aes(y = BGmedian), linewidth=1) +
  geom_point(aes(y = BGmedian), size=1)+
  geom_ribbon(aes(ymin = BGq5, ymax = BGq95), alpha = 0.3) +
  geom_point(aes(y = Detected / (1/5*BGcoef)), color="red3")+
  scale_y_continuous(
    name = "Deaths / week",
    sec.axis = sec_axis(trans = ~.*1/5*BGcoef, name = "Surveillance detection"), labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(title = "BG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), axis.title.x = element_blank(), axis.title.y.right = element_text(colour = "red3", size=8), axis.title.y.left = element_text(size=8)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=8), axis.text.y.left = element_text(size=8), plot.title = element_text(size=8))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))+  theme_linedraw()
BG

MS <- ggplot(MS_sim.dD, aes(x = Time)) +
  geom_line(aes(y=MSmedian), linewidth=1)+
  geom_point(aes(y = MSmedian), size=1)+
  geom_ribbon(aes(ymin = MSq5, ymax = MSq95),alpha = 0.3)+
  geom_point(aes(y = Detected / (1/5*MScoef)), color="red3")+
  scale_y_continuous(
    name = "Deaths / week",
    sec.axis = sec_axis(trans = ~.*1/5*MScoef, name = "Surveillance detection"), labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(title = "MS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), axis.title.x = element_blank(), axis.title.y.right = element_text(colour = "red3", size=8), axis.title.y.left = element_text(size=8)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=8), axis.text.y.left = element_text(size=8), plot.title = element_text(size=8))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))+  theme_linedraw()
MS

GG <- ggplot(GG_sim.dD, aes(x = Time)) +
  geom_line(aes(y=GGmedian), linewidth=1)+
  geom_point(aes(y = GGmedian), size=1)+
  geom_ribbon(aes(ymin = GGq5, ymax = GGq95), alpha = 0.3)+
  geom_point(aes(y = Detected / (1/2*GGcoef)), color="red3")+
  scale_y_continuous(
    name = "Deaths / week",
    sec.axis = sec_axis(trans = ~.*1/2*GGcoef, name = "Surveillance detection"), labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(title = "GG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), axis.title.x = element_blank(), axis.title.y.right = element_text(colour = "red3", size=8), axis.title.y.left = element_text(size=8)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=8), axis.text.y.left = element_text(size=8), plot.title = element_text(size=8))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))+  theme_linedraw()
GG

M <- ggplot(M_sim.dD, aes(x = Time)) +
  geom_line(aes(y=Mmedian), linewidth=1)+
  geom_point(aes(y = Mmedian), size=1)+
  geom_ribbon(aes(ymin = Mq5, ymax = Mq95), alpha = 0.3)+
  geom_point(aes(y = Detected / (5*Mcoef)), color="red3")+
  scale_y_continuous(
    name = "Deaths / week",
    sec.axis = sec_axis(trans = ~.*5*Mcoef, name = "Surveillance detection"), labels = scales::comma)+
  scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = labels) +
  labs(title = "M") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), axis.title.x = element_blank(), axis.title.y.right = element_text(colour = "red3", size=8), axis.title.y.left = element_text(size=8)) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(size=8), axis.text.y.left = element_text(size=8), plot.title = element_text(size=8))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))+  theme_linedraw()
M

ggarrange(BG, WS, MS, GG, M,
          ncol = 2, nrow = 3)

ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyze model simulations/Figures/Five species dD.png", width = 10, height = 7, dpi = 300)

#quantification of the contribution of specific species
mean(BG_sim.dD$BGmedian)/(mean(WS_sim.dD$WSmedian)+mean(BG_sim.dD$BGmedian)+mean(MS_sim.dD$MSmedian)+mean(GG_sim.dD$GGmedian)+mean(M_sim.dD$Mmedian)) #0.5145218

allmedian <- (WS_sim.dD$WSmedian)+(BG_sim.dD$BGmedian)+(MS_sim.dD$MSmedian)+(GG_sim.dD$GGmedian)+(M_sim.dD$Mmedian)
plot(allmedian)
max(allmedian)

