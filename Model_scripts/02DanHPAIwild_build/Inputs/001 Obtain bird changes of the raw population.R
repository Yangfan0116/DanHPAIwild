setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")
#read the shape file
pixelnumber <- read.csv("Data/raster in kilometer.csv") %>% 
  dplyr::rename("KN10kmDK"="DKmapsf.KN10kmDK")
pixel.sf <- st_as_sf(pixelnumber, coords = c("x", "y"))

#read raw bird counts
myfiles <- list.files(path = "H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Data", pattern = "full_idenTrue0.csv$", full.names = TRUE)
desired_order <- c("WSfull_idenTrue0.csv", "BGfull_idenTrue0.csv", "MSfull_idenTrue0.csv", "GGfull_idenTrue0.csv", "Mfull_idenTrue0.csv")
myfiles <- myfiles[match(desired_order, basename(myfiles))]
Truezeroraw_list <- lapply(myfiles, function(file) read.csv(file, sep = ","))
species_names <- c("WS", "BG", "MS", "GG", "M")
names(Truezeroraw_list) <- species_names

#select columns
Truezeroraw_list <- lapply(Truezeroraw_list, function(x) x %>% select(KN10kmDK, time, counts) %>% replace(is.na(.), 0))

#combine the list and the sf
Truezeroraw_list_wide_sf <- list()
for (i in 1:length(Truezeroraw_list)) {
  Truezeroraw_list_wide <- lapply(1:length(Truezeroraw_list), function(x) (pivot_wider(Truezeroraw_list[[x]], names_from = time, values_from = counts)))
  
  Truezeroraw_list_wide_sf[[i]] <- left_join(pixel.sf, Truezeroraw_list_wide[[i]], by="KN10kmDK")
}

species_names <- c("WS", "BG", "MS", "GG", "M")
names(Truezeroraw_list_wide_sf) <- species_names

#Obtain a list of bird abundance by species-week-cell
Truezeroraw_list_wide_sf <- lapply(Truezeroraw_list_wide_sf, function(sf) as.data.frame(sf) %>% select(3:262))
library(abind)
Birdcounts <- abind(Truezeroraw_list_wide_sf, along = 3)

#calculate the changes by species-week-cell (theta_i,w,c)
one <- Birdcounts[, 1:104, ]
two <- Birdcounts[, 53:156, ]
three <- Birdcounts[, 105:208, ]
four <- Birdcounts[, 157:260, ]

add_column <- function(matrix) {
  new_matrix <- cbind(0, matrix)
  return(new_matrix)
}

one_A <- array(0, dim = c(614, 105, 5))
for(z in 1:5){
  one_A[, , z] <- add_column(one[, , z])
}
two_A <- array(0, dim = c(614, 105, 5))
for(z in 1:5){
  two_A[, , z] <- add_column(two[, , z])
}
three_A <- array(0, dim = c(614, 105, 5))
for(z in 1:5){
  three_A[, , z] <- add_column(three[, , z])
}
four_A <- array(0, dim = c(614, 105, 5))
for(z in 1:5){
  four_A[, , z] <- add_column(four[, , z])
}

#replace NA with 0
one_A[is.na(one_A)] <- 0
two_A[is.na(two_A)] <- 0
three_A[is.na(three_A)] <- 0
four_A[is.na(four_A)] <- 0

birdcount_A_list_raw <- list(one_A, two_A, three_A, four_A)
saveRDS(birdcount_A_list_raw, "Data/list of birdcount_A_raw_2ConseYear.rds")

#theta calcualation
theta_one_A <- array(0, dim = dim(one_A))
theta_two_A <- array(0, dim = dim(two_A))
theta_three_A <- array(0, dim = dim(three_A))
theta_four_A <- array(0, dim = dim(four_A))

for (i in 1:614) {
  for (j in 2:105) {
    theta_one_A[i, j, ] <- one_A[i, j, ] - one_A[i, j-1, ]
    theta_two_A[i, j, ] <- two_A[i, j, ] - two_A[i, j-1, ]
    theta_three_A [i, j, ] <- three_A [i, j, ] - three_A [i, j-1, ]
    theta_four_A[i, j, ] <- four_A[i, j, ] - four_A[i, j-1, ]
  }
}

theta_A_raw_list <- list(theta_one_A, theta_two_A, theta_three_A, theta_four_A)

saveRDS(theta_A_raw_list, "Data/list of theta_A_raw_2ConseYear.rds")

