setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild")
source("Data/Required pkgs.R")
# Load data (incl. birds changed number, bird dispersal probability, neccessary function)
compartment_sf <- readRDS("Data/compartment_sf.rds") # SIR compartments for 5 species
V_disp <- lapply(compartment_sf, function(x) x$S)
source("Model_scripts/02DanHPAIwild_build/Inputs/Truncated_rnultinom function.R")
theta_A_list <- readRDS("Data/list of theta_A_raw_2ConseYear.rds")
passur <- read.csv("Data/passur_species_matrix.csv", sep = ";")
passur <- as.matrix(passur[, -1])
#Proportion of water areas by cell (xi)
CLC <- read.csv("Data/DKLine1kmCLC proportions.csv")
xi <- CLC$Water.bodies+CLC$Wetlands

#load disp_prob 
source("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/02DanHPAIwild_build/Inputs/Dispersal probability generation.R")
prob <- lapply(compartment_sf, function(x) x$S)

# Data frame to record simulated numbers
S_list <- lapply(compartment_sf, function(x) x$S)
I_list <- lapply(compartment_sf, function(x) x$I)
dD_list <- lapply(compartment_sf, function(x) x$R)
D_list <- lapply(compartment_sf, function(x) x$R)
Rem_list <- lapply(compartment_sf, function(x) x$R)
Rec_list <- lapply(compartment_sf, function(x) x$R)
dRem_list <- lapply(compartment_sf, function(x) x$R)

S_list <- lapply(S_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
I_list <- lapply(I_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
dD_list <- lapply(dD_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
D_list <- lapply(D_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
Rem_list <- lapply(Rem_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
Rec_list <- lapply(Rec_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})
dRem_list <- lapply(dRem_list, function(l){l %>% select(!(3:158)) %>% mutate(X156=0, .before = 3)})

#make df/list into matrix/array
m <- matrix(rep(0, 105*614), nrow=614)
theta_A <- array(unlist(list(m ,m ,m ,m, m)), dim = (c(614, 105, 5)))
S_list_A <- theta_A; I_list_A <- theta_A; dD_list_A <- theta_A; D_list_A <- theta_A; Rem_list_A <- theta_A; Rec_list_A <- theta_A; dRem_list_A <- theta_A; V_disp_A <- theta_A; prob_A <- theta_A

for (i in 1:5) {
  S_list_A[,,i] <- as.matrix(as.data.frame(S_list[[i]][, 3:ncol(S_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  I_list_A[,,i] <- as.matrix(as.data.frame(I_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  dD_list_A[,,i] <- as.matrix(as.data.frame(dD_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  D_list_A[,,i] <- as.matrix(as.data.frame(D_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  Rem_list_A[,,i] <- as.matrix(as.data.frame(Rem_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  Rec_list_A[,,i] <- as.matrix(as.data.frame(Rec_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  dRem_list_A[,,i] <- as.matrix(as.data.frame(dRem_list[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  V_disp_A[,,i] <- as.matrix(as.data.frame(V_disp[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

for (i in 1:5) {
  prob_A[,,i] <- as.matrix(as.data.frame(prob[[i]][, 3:ncol(I_list[[i]])])[1:614, 1:105])
}

V_M <- m

M1 <- as.matrix(disp_prob[[1]])
M2 <- as.matrix(disp_prob[[2]])
M3 <- as.matrix(disp_prob[[3]])
M4 <- as.matrix(disp_prob[[4]])
M5 <- as.matrix(disp_prob[[5]])
disp_prob_L <- list(M1, M2, M3, M4, M5)

tmp_S_list_A <- S_list_A
tmp_I_list_A <- I_list_A
tmp_Rec_list_A <- Rec_list_A
dRec_list_A <- dD_list_A

# Parameters
N_Env <- 2*10^14
# weight.ratio <- list(WS=10, BG=2, MS=11, GG=3, M=1)

#Peak values are equal to the weight ratios, when we assume the peak contact rate of mallards is 1. The contact rates peak at the beginning of Nov. in the baseline model.
WS <- function(x) 5 * (1 + cos(2 * pi / 52 * x - pi / 6))
BG <- function(x) 1 * (1 + cos(2 * pi / 52 * x - pi / 6))
MS <- function(x) 5.5 * (1 + cos(2 * pi / 52 * x - pi / 6))
GG <- function(x) 1.5 * (1 + cos(2 * pi / 52 * x - pi / 6))
M <-  function(x) 0.5 * (1 + cos(2 * pi / 52 * x - pi / 6))

# Create a list of functions
contact <- list(WS=WS, BG=BG,MS=MS,GG=GG,M=M)

ID50 <- list(WS = 5*10^3, BG = 5*10^2, MS = 5*10^3, GG = 5*10^2, M = 5*10^1)
gamma <- list(WS = 7/4, BG = 7/3, MS = 7/4, GG = 7/3, M = 7/5.8)
mu <- list(WS = 7/(10*5), BG = 7/(2*5), MS = 7/(11*5), GG = 7/(3*5), M = 7/5)
mf <- list(WS=1, BG=0.75, MS=1, GG=0.75, M=0.6)
eta <- list(WS = (7*10^4.25)/2, BG = (7*10^3.05)/2, MS = (7*10^4.46)/2, GG = (7*10^3.05)/2, M = (7*4.5*10^3)/2)
epsilon <- list(WS = eta$WS *1/7, BG = eta$BG *1/7, MS = eta$MS *1/7, GG = eta$GG *1/7, M = eta$M *1/7)
sigma <- c(0,rep(c(rep(1.6, 13), rep(0.46, 13), rep(1.6, 13), rep(5.4, 13)), 2))
foraging <- list(WS = 0.28, BG = 0.35, MS = 0.21, GG = 0.22, M = 0.07)

save(S_list_A, I_list_A, dD_list_A, D_list_A, Rem_list_A, Rec_list_A, dRem_list_A, V_disp_A, prob_A, V_M, disp_prob_L, tmp_S_list_A, tmp_I_list_A, tmp_Rec_list_A, dRec_list_A, theta_A_list, passur, N_Env, contact, ID50, gamma, mu, mf, eta, epsilon, sigma, foraging, xi, file="Data/Core data/DanHPAIwild parameters_Raw.RData")

