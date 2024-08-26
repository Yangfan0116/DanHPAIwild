# Calculate the final bird abundance of the raw population 
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
Total_bird_fiveyears <- readRDS("Data/list of birdcount_A_raw_2ConseYear.rds")

calculate_sum <- function(array_list) {
  result <- lapply(array_list,  function(arr) {
    apply(arr, c(1, 2), sum) # Apply median function on dimensions 1 and 2
  })
  return(result)
}

Total_bird_fiveyears_sum <- calculate_sum(Total_bird_fiveyears)

Total_bird_fiveyears_sum_array <- array(unlist(Total_bird_fiveyears_sum), dim = c(nrow(Total_bird_fiveyears_sum[[1]]),ncol(Total_bird_fiveyears_sum[[1]]), length(Total_bird_fiveyears_sum)))

# Sum up the rows of each matrix
Total_bird_fiveyears_sum_array_summed_rows <- apply(Total_bird_fiveyears_sum_array, c(3, 2), sum)

median_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, median)
q5_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, quantile, prob=0.05)
q95_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, quantile, probs = 0.95)

save(median_Total_bird, q5_Total_bird, q95_Total_bird, file = "Data/M595_raw_Totalbird_2ConseYear.rds")


# Calculate the final bird abundance of the estimated population 
setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
Total_bird_fiveyears <- readRDS("Data/list of birdcount_A_estimated_2ConseYear.rds")

calculate_sum <- function(array_list) {
  result <- lapply(array_list,  function(arr) {
    apply(arr, c(1, 2), sum) # Apply median function on dimensions 1 and 2
  })
  return(result)
}

Total_bird_fiveyears_sum <- calculate_sum(Total_bird_fiveyears)

Total_bird_fiveyears_sum_array <- array(unlist(Total_bird_fiveyears_sum), dim = c(nrow(Total_bird_fiveyears_sum[[1]]),ncol(Total_bird_fiveyears_sum[[1]]), length(Total_bird_fiveyears_sum)))

# Sum up the rows of each matrix
Total_bird_fiveyears_sum_array_summed_rows <- apply(Total_bird_fiveyears_sum_array, c(3, 2), sum)

median_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, median)
q5_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, quantile, prob=0.05)
q95_Total_bird <- apply(Total_bird_fiveyears_sum_array_summed_rows, 2, quantile, probs = 0.95)

save(median_Total_bird, q5_Total_bird, q95_Total_bird, file = "Data/M595_estimated_Totalbird_2ConseYear.rds")
