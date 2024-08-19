#' Simulation model for the HPAI transmission through environmental contamination (vectorised version, for speed)
#'
#' @param passur The passive surveillance data set by species in an epidemiological year starting from October
#' @param V_M A matrix for recording environmental viral load per cell per week
#' @param disp_prob_L A list for recording the probability of dispersal to cells by species for each pair of cells
#' @param ID50 Median infection dose per species
#' @param S_list_A A array for recording cumulative susceptible birds by species per cell per week
#' @param dD_list_A A array for recording dead birds by species per cell per week
#' @param V_disp_A A array for recording viral sheds by species per cell per week while dispersing
#' @param gamma Rate of leaving the infectious state per species
#' @param Rec_list_A A array for recording cumulative recovered birds by species per cell per week
#' @param dRec_list_A A array for recording recovered birds by species per cell per week
#' @param mf Case fatality rate per species
#' @param theta_A_list A list for providing bird counts changes by species per cell per week
#' @param sigma Viral decay rate in the environment per week
#' @param I_list_A A array for recording cumulative infectious birds by species per cell per week
#' @param epsilon Contamination rate of an intact dead bird per species
#' @param eta Virus shedding rate of an infectious bird per species
#' @param mu Decay rate of dead birds in the environment
#' @param D_list_A A array for recording cumulative dead birds by species per cell per week
#' @param prob_A A array for recording the probability of infection by species per cell per week
#' @param N_Env The baseline environmental dilution factor
#' @param contact Contact rate functions per species depending on the week number
#' @param xi The proportion of the cell occupied the water area per cell
#' @param foraging The proportion of time spent on foraging per species
#'
#' @export
DanHPAIwildModel <- function(passur, V_M, ID50, S_list_A, dD_list_A, V_disp_A, disp_prob_L, gamma, Rec_list_A, dRec_list_A, mf, theta_A_list, sigma, I_list_A, epsilon, eta, mu, D_list_A, prob_A, N_Env, contact, xi, foraging){

  # set.seed(1)
  theta_A <- sample(theta_A_list, 1)[[1]]

  n_species <- 5
  n_weeks <- 53
  stopifnot(nrow(passur)==n_weeks)
  stopifnot(ncol(passur)==n_species)
  birdcounts <- matrix(0, nrow = n_weeks, ncol=n_species)
  probability <- birdcounts
  for(week in seq_len(n_weeks)){
    for(species in seq_len(n_species)){
      birdcounts[week, species] <- sum(theta_A[, seq_len(week), species])
      probability[week, species] <- passur[week, species]/birdcounts[week, species]
    }
  }
  probability <- rbind(c(0,0,0,0,0), probability[-1,], matrix(0, ncol=5, nrow=52))

  half_vector <- c(rep('A', 307), rep('B', 307))
  ninetysix_vector <- c(rep('A', 589), rep('B', 25))
  # Shuffle the vector randomly
  half <- sample(half_vector)
  ninetysix <- sample(ninetysix_vector)

  n_timesteps <- 104
  n_cells <- 614

  for (j in seq_len(n_timesteps)) {
    for (i in seq_len(n_species)){
      
      k <- seq_len(n_cells)
      
      ################
      #Movement------: turnover has only susceptibles, newly arrivals follow PS:PI:PR
      ################
      # print(i);print(k);print(j)
      
      #replace removed birds of the last time step with newly arrived birds
      theta_A[k, j+1, i] <- theta_A[k, j+1, i] + dD_list_A[k, j, i]
      
      ### START OPTIMISED CODE:
      if (is_migration_A(i,j)){
        
        k <- which(theta_A[, j+1, i] < 0)
        
        if(any(abs(theta_A[k, j+1, i]) > (S_list_A[k, j, i]+I_list_A[k, j, i]+Rec_list_A[k, j, i]))) browser()
        samp1 <- sample_no_replacement(S_list_A[k, j, i], I_list_A[k, j, i], Rec_list_A[k, j, i], abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] - samp1$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] - samp1$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] - samp1$numR
        
        k <- which(theta_A[, j+1, i] > 0)
        
        ## You re-use this code in a few places, but remember DRY - do not repeat yourself...
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        if(is.na(P_S) || P_S==0){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] + samp2$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] + samp2$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] + samp2$numR
        
        k <- which(theta_A[, j+1, i] == 0)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i]
        I_list_A[k, j+1, i] <- I_list_A[k, j, i]
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i]
        
      } else if (is_migration_B(i,j)){
        
        k <- which(theta_A[, j+1, i] < 0 & half[] == "A")
        
        samp1 <- sample_no_replacement(S_list_A[k, j, i], I_list_A[k, j, i], Rec_list_A[k, j, i], abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] - samp1$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] - samp1$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] - samp1$numR
        
        k <- which(theta_A[, j+1, i] < 0 & half[] != "A")
        
        All <- S_list_A[k,j,i] + I_list_A[k,j,i] + Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs=c(1,0,0), All)
        samp2 <- sample_no_replacement(samp3$numS, 0L, samp3$numR, abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- samp3$numS - samp2$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR - samp2$numR
        
        k <- which(theta_A[, j+1, i] > 0 & half[] == "A")
        
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        
        if(is.na(P_S)){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] + samp2$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] + samp2$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] + samp2$numR
        
        k <- which(theta_A[, j+1, i] > 0 & half[] != "A")
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i] + I_list_A[k,j,i] + Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,1,1, probs, All)
        
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        
        if(is.na(P_S)){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- samp3$numS + samp2$numS
        I_list_A[k, j+1, i] <- samp3$numI + samp2$numI
        Rec_list_A[k, j+1, i] <- samp3$numR + samp2$numR
        
        k <- which(theta_A[, j+1, i] == 0 & half[] == "A")
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i]
        I_list_A[k, j+1, i] <- I_list_A[k, j, i]
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i]
        
        k <- which(theta_A[, j+1, i] == 0 & half[] != "A")
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i] + I_list_A[k,j,i] + Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs, All)
        S_list_A[k, j+1, i] <- samp3$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR
      } else if (is_migration_C(i,j)){
        
        
        k <- which(theta_A[, j+1, i] < 0 & ninetysix[] == "A")
        
        samp1 <- sample_no_replacement(S_list_A[k, j, i], I_list_A[k, j, i], Rec_list_A[k, j, i], abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] - samp1$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] - samp1$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] - samp1$numR
        
        k <- which(theta_A[, j+1, i] < 0 & ninetysix[] != "A")
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs, All)
        
        samp2 <- sample_no_replacement(samp3$numS, samp3$numI, samp3$numR, abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- samp3$numS - samp2$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR - samp2$numR
        
        k <- which(theta_A[, j+1, i] > 0 & ninetysix[] == "A")
        
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        
        if(is.na(P_S)){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] + samp2$numS
        I_list_A[k, j+1, i] <- I_list_A[k, j, i] + samp2$numI
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i] + samp2$numR
        
        k <- which(theta_A[, j+1, i] > 0 & ninetysix[] != "A")
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,1,1, probs, All)
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        if(is.na(P_S)){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- samp3$numS + samp2$numS
        I_list_A[k, j+1, i] <- samp3$numI + samp2$numI
        Rec_list_A[k, j+1, i] <- samp3$numR + samp2$numR
        
        k <- which(theta_A[, j+1, i] == 0 & ninetysix[] == "A")
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i]
        I_list_A[k, j+1, i] <- I_list_A[k, j, i]
        Rec_list_A[k, j+1, i] <- Rec_list_A[k, j, i]
        
        k <- which(theta_A[, j+1, i] == 0 & ninetysix[] != "A")
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs, All)
        S_list_A[k, j+1, i] <- samp3$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR
        
      } else if (is_migration_D(i,j)){
        k <- which(theta_A[, j+1, i] < 0)
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs, All)
        
        samp2 <- sample_no_replacement(samp3$numS, 0L, samp3$numR, abs(theta_A[k, j+1, i]))
        
        S_list_A[k, j+1, i] <- samp3$numS - samp2$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR - samp2$numR
        
        k <- which(theta_A[, j+1, i] > 0)
        
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,1,1, probs, All)
        P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
        P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
        if(is.na(P_S)){
          probs <- c(1-probability[j+1,i],probability[j+1,i],0)
        } else{
          probs <- c(P_S, P_I, P_Rec)
        }
        
        samp2 <- sample_with_replacement(1, 1, 1, theta_A[k, j+1, i], probs)
        
        S_list_A[k, j+1, i] <- samp3$numS + samp2$numS
        I_list_A[k, j+1, i] <- samp3$numI + samp2$numI
        Rec_list_A[k, j+1, i] <- samp3$numR + samp2$numR
        
        k <- which(theta_A[, j+1, i] == 0)
        probs <- c(1,0,0)
        All <- S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i]
        
        samp3 <- sample_with_changes(1,0,1, probs, All)
        S_list_A[k, j+1, i] <- samp3$numS
        I_list_A[k, j+1, i] <- 0L
        Rec_list_A[k, j+1, i] <- samp3$numR
        
      } else if (is_migration_E(i,j)) {
        
        k <- which(theta_A[, j+1, i] < 0)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] + I_list_A[k, j, i] + Rec_list_A[k, j, i] - abs(theta_A[k, j+1, i])
        I_list_A[k, j+1, i] <- 0
        Rec_list_A[k, j+1, i] <- 0
        
        k <- which(theta_A[, j+1, i] > 0)
        
        S_list_A[k, j+1, i] <- S_list_A[k, j, i] + I_list_A[k, j, i] + Rec_list_A[k, j, i] + theta_A[k, j+1, i]
        I_list_A[k, j+1, i] <- 0
        Rec_list_A[k, j+1, i] <- 0
      }
      
      ################
      #SIR+V------
      ################
      
      k <- seq_len(n_cells)

      prob_A[k, j+1, i] <- if_else(xi[k]==0, 0, 1 - exp(-contact[[i]](j+1)*(1- exp((-log(2)/ID50[[i]])*V_M[k, j]/(N_Env*xi[k])))))

      dI <- rbinom(n_cells, S_list_A[k, j+1, i], prob=prob_A[k, j, i])
      dRD <- rbinom(n_cells, I_list_A[k, j+1, i], prob=1 - exp(-gamma[[i]]))
      dD_list_A[k, j+1, i] <- round(dRD * mf[[i]])
      dRec_list_A[k, j+1, i] <- dRD - dD_list_A[k, j+1, i]

      I_list_A[k, j+1, i]<- I_list_A[k, j+1, i] + dI-dRD
      S_list_A[k, j+1, i] <- S_list_A[k, j+1, i] - dI
      D_list_A[k, j+1, i] <- D_list_A[k, j, i] + dD_list_A[k, j+1, i]
      Rec_list_A[k, j+1, i] <- Rec_list_A[k, j+1, i] + dRec_list_A[k, j+1, i]

      ################
      #Dispersion------
      ################
      # using dweibull to integrate and obtain dis_prob for i_th species of k_th cell at j+1_th time step (extra land border cells were taken into account)
      # random assign the number of I bird dispersal to neighboring cells


      trail <- I_list_A[k, j+1, i]
      num.of.dbirds <- vapply(k, function(x) rmultinom(1L, size = trail[x], prob = disp_prob_L[[i]][x, ]), numeric(n_cells))

      #calculate V due to dispersal. We assume the fraction of time budget spent on foraging of a day is the fraction that a bird spends time in the neighboring cells
      V_disp_A[, j+1, i]  <-  foraging[[i]]*eta[[i]]*apply(num.of.dbirds,1,sum)
    }#loop for 5 bird species
    #Sum up all V_disp_A shedded by 5 species to obtain Disp_V
    Disp_V <- V_disp_A[,,1]+V_disp_A[,,2]+V_disp_A[,,3]+V_disp_A[,,4]+V_disp_A[,,5]

    #Sum up all V shedded by 5 species to obtain V and decayed in the env
    V_M[, j+1] <- V_M[, j] * exp(-sigma[j+1]) +
      ((1-foraging[[1]])*eta[[1]]*I_list_A[, j+1, 1] + (1-foraging[[2]])*eta[[2]]*I_list_A[, j+1, 2] + (1-foraging[[3]])*eta[[3]]*I_list_A[, j+1, 3] + (1-foraging[[4]])*eta[[4]]*I_list_A[, j+1, 4] + (1-foraging[[5]])*eta[[5]]*I_list_A[, j+1, 5] + Disp_V[, j+1]) * exp(-sigma[j+1]) +
      (epsilon[[1]]*D_list_A[, j+1, 1] * exp(-mu[[1]]) + epsilon[[2]]*D_list_A[, j+1, 2] * exp(-mu[[2]]) + epsilon[[3]]*D_list_A[, j+1, 3] * exp(-mu[[3]]) + epsilon[[4]]*D_list_A[, j+1, 4] * exp(-mu[[4]]) + epsilon[[5]]*D_list_A[, j+1, 5] * exp(-mu[[5]]))*exp(-sigma[j+1]) 
  }

  return(list(half=half, ninetysix=ninetysix, theta_A=theta_A, S_list_A=S_list_A, I_list_A=I_list_A, Rec_list_A=Rec_list_A, dRec_list_A=dRec_list_A, D_list_A=D_list_A, dD_list_A=dD_list_A, prob_A=prob_A, V_M=V_M))

}
