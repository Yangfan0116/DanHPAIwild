#get firstdf from H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Model_scripts/01Bird_counts_est/003E-M_imputation/004 Summary of missForest imputed bird abundance.R
missForest_firstdf <- firstdf
#get firstdf from H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Model_scripts/01Bird_counts_est/003E-M_imputation/004.2 Summary of raw bird abundance.R
ture_zero_raw_firstdf <- firstdf

missForest_firstdf$type <- c('Completed population')
ture_zero_raw_firstdf$type <- c("Raw population")

final_df <- rbind(missForest_firstdf, ture_zero_raw_firstdf)

breaks <- c(0, 51/12, 2*51/12, 3*51/12, 4*51/12, 5*51/12, 6*51/12, 7*51/12, 8*51/12, 9*51/12, 10*51/12, 11*51/12)
labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

Time_series_graphs_com_1 <- ggplot(final_df %>% filter(Year=="2016/17"), aes(x = time, y = n_across_all_cells, color = Year)) +
  geom_line(aes(group=type, color=type)) +
  geom_point(aes(group=type, color=type), size=0.7) +
  theme_hc() +
  facet_wrap(~factor(Species, levels = c("BG", "WS", "MS", "GG", "M", "Total")), scales = "free_y") +
  labs(x = "Month", color = "Population type") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_color_manual(labels = c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"), values = brewer.pal(5, "Set1")) +
  ylab("Bird abundance")
Time_series_graphs_com_1

Time_series_graphs_com_2 <- ggplot(final_df %>% filter(Year=="2017/18"), aes(x = time, y = n_across_all_cells, color = Year)) +
  geom_line(aes(group=type, color=type)) +
  geom_point(aes(group=type, color=type), size=0.7) +
  theme_hc() +
  facet_wrap(~factor(Species, levels = c("BG", "WS", "MS", "GG", "M", "Total")), scales = "free_y") +
  labs(x = "Month", color = "Population type") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_color_manual(labels = c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"), values = brewer.pal(5, "Set1")) +
  ylab("Bird abundance")

Time_series_graphs_com_2

Time_series_graphs_com_3 <- ggplot(final_df %>% filter(Year=="2018/19"), aes(x = time, y = n_across_all_cells, color = Year)) +
  geom_line(aes(group=type, color=type)) +
  geom_point(aes(group=type, color=type), size=0.7) +
  theme_hc() +
  facet_wrap(~factor(Species, levels = c("BG", "WS", "MS", "GG", "M", "Total")), scales = "free_y") +
  labs(x = "Month", color = "Population type") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_color_manual(labels = c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"), values = brewer.pal(5, "Set1")) +
  ylab("Bird abundance")
Time_series_graphs_com_3

Time_series_graphs_com_4 <- ggplot(final_df %>% filter(Year=="2019/20"), aes(x = time, y = n_across_all_cells, color = Year)) +
  geom_line(aes(group=type, color=type)) +
  geom_point(aes(group=type, color=type), size=0.7) +
  theme_hc() +
  facet_wrap(~factor(Species, levels = c("BG", "WS", "MS", "GG", "M", "Total")), scales = "free_y") +
  labs(x = "Month", color = "Population type") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_color_manual(labels = c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"), values = brewer.pal(5, "Set1")) +
  ylab("Bird abundance")
Time_series_graphs_com_4

Time_series_graphs_com_5 <- ggplot(final_df %>% filter(Year=="2020/21"), aes(x = time, y = n_across_all_cells, color = Year)) +
  geom_line(aes(group=type, color=type)) +
  geom_point(aes(group=type, color=type), size=0.7) +
  theme_hc() +
  facet_wrap(~factor(Species, levels = c("BG", "WS", "MS", "GG", "M", "Total")), scales = "free_y") +
  labs(x = "Month", color = "Population type") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_color_manual(labels = c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21"), values = brewer.pal(5, "Set1")) +
  ylab("Bird abundance")
Time_series_graphs_com_5

library(cowplot)
plot_grid(Time_series_graphs_com_1, Time_series_graphs_com_2, Time_series_graphs_com_3, Time_series_graphs_com_4, Time_series_graphs_com_5, ncol=1)
ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/DanHPAIWild/DanHAPIwildModel/Model_scripts/03Model_runs/Analyses on model simulations/Figures/missForest_vs_raw_Time_series_graphs.png", width = 12, height = 25, dpi=300)
