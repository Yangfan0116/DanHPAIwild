#Obtain the estimated bird abundance 
missForest_list <- readRDS("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data/missForest_RFdata.imputed_full_list.rda")
glimpse(missForest_list[[1]])
species_names <- c("WS", "BG", "MS", "GG", "M")
names(missForest_list) <- species_names

aggregated_list <- lapply(missForest_list, function(df) {
  aggregate(counts ~ time, data = df, sum)
})

#ground rounding
aggregated_list <- lapply(aggregated_list, function(df) {
  df %>% mutate(counts = as.integer(df$counts))
})

# Combine aggregated results into a single data frame
combined_df <- do.call(rbind, aggregated_list)
# Rename columns for clarity
combined_df <- aggregate(x=list(combined_df$"counts"),
                         by = list(combined_df$time),
                         FUN = sum)
colnames(combined_df) <- c("time", "n_across_all_cells")
combined_df<-combined_df %>% mutate(Species="Total")

#give the time step for the abundance
a <- data.frame(time=c(201639:201652, 201701:201752, 201801:201852, 201901:201952, 202001:202052, 202101:202138), week=c(1:260))
combined_df <- left_join(combined_df, a, by='time')
combined_df$time <- dense_rank(as.numeric(gsub("//D", "", combined_df$time)))

#add species column
aggregated_list <- map2_df(aggregated_list, names(missForest_list), ~ mutate(.x, Species = .y))
aggregated_list <- left_join(aggregated_list, a, by='time')
aggregated_list$time <- dense_rank(as.numeric(gsub("//D", "", aggregated_list$time)))
names(aggregated_list)[names(aggregated_list) == 'counts'] <- 'n_across_all_cells'

#total and each species combined
firstdf <- rbind(aggregated_list, combined_df)
firstdf <- firstdf %>% mutate(Year=case_when(
  between(time, 1, 52) ~ "2016/17",
  between(time, 53, 104) ~ "2017/18",
  between(time, 105, 156) ~ "2018/19",
  between(time, 157, 208) ~ "2019/20",
  between(time, 209, 260) ~ "2020/21")) %>% 
  mutate(time = if_else(time%%52!=0, time%%52-1, 51))
#the final dataset
missForest_firstdf <- firstdf


#Obtain the raw bird abundance
#line of number of birds in 2020 week 39 to 2021 week 38
#read raw bird counts
myfiles <- list.files(path = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data", pattern = "full_idenTrue0.csv$", full.names = TRUE)
desired_order <- c("WSfull_idenTrue0.csv", "BGfull_idenTrue0.csv", "MSfull_idenTrue0.csv", "GGfull_idenTrue0.csv", "Mfull_idenTrue0.csv")
myfiles <- myfiles[match(desired_order, basename(myfiles))]
Truezeroraw_list <- lapply(myfiles, function(file) read.csv(file, sep = ","))
species_names <- c("WS", "BG", "MS", "GG", "M")
names(Truezeroraw_list) <- species_names

aggregated_list <- lapply(Truezeroraw_list, function(df) {
  aggregate(counts ~ time, data = df, sum)
})
# Combine aggregated results into a single data frame
combined_df <- do.call(rbind, aggregated_list)
# Rename columns for clarity
combined_df <- aggregate(x=list(combined_df$"counts"),
                         by = list(combined_df$time),
                         FUN = sum)
colnames(combined_df) <- c("time", "n_across_all_cells")
combined_df<-combined_df %>% mutate(Species="Total")

a <- data.frame(time=c(201639:201652, 201701:201752, 201801:201852, 201901:201952, 202001:202052, 202101:202138), week=c(1:260))

combined_df <- left_join(combined_df, a, by='time')
combined_df$time <- dense_rank(as.numeric(gsub("//D", "", combined_df$time)))

#replace na with 0
replace_na_with_0 <- function(df) {
  df %>%
    mutate_all(~ ifelse(is.na(.), 0, .))
}
Truezeroraw_list <- map(Truezeroraw_list, replace_na_with_0)
Truezeroraw_list <- map2_df(Truezeroraw_list, names(Truezeroraw_list), ~ mutate(.x, Species = .y))#add species coloum
aggregated_list <- aggregate(x=list(Truezeroraw_list$counts), by=list(Truezeroraw_list$time, Truezeroraw_list$Species), FUN=sum)
colnames(aggregated_list) <- c("time", "Species", "n_across_all_cells")#accross all cells
aggregated_list <- left_join(aggregated_list, a, by='time')
aggregated_list$time <- dense_rank(as.numeric(gsub("//D", "", aggregated_list$time)))
#row bind all and species-specific
firstdf <- rbind(aggregated_list, combined_df)

firstdf <- firstdf %>% mutate(Year=case_when(
  between(time, 1, 52) ~ "2016/17",
  between(time, 53, 104) ~ "2017/18",
  between(time, 105, 156) ~ "2018/19",
  between(time, 157, 208) ~ "2019/20",
  between(time, 209, 260) ~ "2020/21")) %>% 
  mutate(time = if_else(time%%52!=0, time%%52-1, 51))
#the final dataset
ture_zero_raw_firstdf <- firstdf

missForest_firstdf$type <- c('estimated population')
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
ggsave("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/002 Analyses on model simulations/Figures/missForest_vs_raw_Time_series_graphs.png", width = 12, height = 25, dpi=300)
