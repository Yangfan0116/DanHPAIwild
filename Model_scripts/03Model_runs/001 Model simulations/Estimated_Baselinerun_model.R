Sys.time()

library("remotes")
remotes::install_github("Yangfan0116/DanHPAIwild")
library("DanHPAIwild")
library("pbapply")
library("dplyr")
pboptions(type="timer")

setwd(".../DanHPAIwild")#change to your working directory

stopifnot(file.exists("Data/Core data/DanHPAIwild parameters_Estimated.RData"))

load("Data/Core data/DanHPAIwild parameters_Estimated.RData")
N_Env <- N_Env/10^9
ID50 <- lapply(ID50, function(x) x*10)
sigma <- sigma

iters <- 400
cores <- 100L
if(cores > 1L){
  library("parallel")
  if(.Platform$OS.type=="unix"){
    cl <- makeForkCluster(nnodes=cores)
  }else{
    cl <- makeCluster(detectCores()-1)
    clusterEvalQ(cl, {
      library(DanHPAIwild)
    })
    clusterExport(cl, varlist = c("DanHPAIwildModel", "passur", "V_M", "ID50", "S_list_A", "dD_list_A", "V_disp_A", "disp_prob_L", "gamma", "Rec_list_A", "dRec_list_A", "mf", "theta_A_list", "sigma", "I_list_A", "epsilon", "eta", "mu", "D_list_A", "prob_A", "N_Env", "contact", "xi", "foraging"))
  }
  
  vector('list', iters) |>
    pblapply(function(x){
      DanHPAIwildModel(passur, V_M, ID50, S_list_A, dD_list_A, V_disp_A, disp_prob_L, gamma, Rec_list_A, dRec_list_A, mf, theta_A_list, sigma, I_list_A, epsilon, eta, mu, D_list_A, prob_A, N_Env, contact, xi, foraging)
    }, cl=cl) ->
    results
  
  stopCluster(cl)
  
}else{
  
  for(i in seq_len(iters)){
    # This will find passur and contents of load() in the global env:
    print(i)
    results[[i]] <- DanHPAIwildModel(passur, V_M, ID50, S_list_A, dD_list_A, V_disp_A, disp_prob_L, gamma, Rec_list_A, dRec_list_A, mf, theta_A_list, sigma, I_list_A, epsilon, eta, mu, D_list_A, prob_A, N_Env, contact, xi, foraging)
  }
  
}

session <- sessionInfo()
save(results, session, file = ".../DanHPAIwild/Model_scripts/03Model_runs/003 Simulation outputs/Estimated_Baseline_output.rda")

Sys.time()
session
