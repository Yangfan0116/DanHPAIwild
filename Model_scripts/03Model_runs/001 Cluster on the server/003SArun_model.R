Sys.time()

library("DanHPAIwild")
library("pbapply")
library("dplyr")
pboptions(type="timer")

# setwd(".../Data/DanHPAIwild params/")
stopifnot(file.exists("DanHPAIwild parameters_RF.RData"))

load("DanHPAIwild parameters_RF.RData")
N_Env <- N_Env/10^9
ID50 <- lapply(ID50, function(x) x*10)
sigma <- sigma

#Altering model parameter values
epsilon <- list(WS=0,
                BG=0,
                MS=0,
                GG=0,
                M=0)

iters <- 400
cores <- 100L
if(cores > 1L){
  library("parallel")
  if(.Platform$OS.type=="unix"){
    cl <- makeForkCluster(nnodes=cores)
  }else{
    cl <- makeCluster(detectCores()-1)
    clusterEvalQ(cl, {
      library(DanHPAIwildModel)
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
save(results, session, file = "003epsilon model output.rda")

Sys.time()
session
