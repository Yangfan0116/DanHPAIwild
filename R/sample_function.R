#' Designated long-distance bird migration pattern per species per cell per week
#'
#' @param S The number of susceptible birds
#' @param I The number of infectious birds
#' @param R The number of recovered birds
#' @param tA The absolute number of bird counts difference between two consecutive weeks
#' @param probs The introduction probability of different bird types
#' @param All The total number of birds at time j per species
#'
#' @export
sample_no_replacement <- function(S, I, R, tA){

  nk <- length(tA)
  if(nk==0L) return(list(numS=numeric(0L), numI=numeric(0L), numR=numeric(0L)))
  stopifnot(length(S)%in%c(1L,nk), length(I)%in%c(1L,nk), length(R)%in%c(1L,nk))
  stopifnot(all(tA <= S+I+R))

  numS <- rhyper(nk, S, I+R, tA)
  numI <- rhyper(nk, I, R, tA-numS)
  numR <- tA-(numS+numI)

  return(list(numS=numS, numI=numI, numR=numR))
}

## Sampling with replacement has unequal probabilities:

# P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# P_I <- sum(I_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])+probability[j+1,i]
# P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# if(is.na(P_S)){
#   probs <- c(1,0,0)
# } else{
#   probs <- c(P_S, P_I, P_Rec)
# }

sample_with_replacement <- function(S, I, R, tA, probs){

  nk <- length(tA)
  if(nk==0L) return(list(numS=numeric(0L), numI=numeric(0L), numR=numeric(0L)))
  stopifnot(length(S)==1L, length(I)==1L, length(R)==1L, length(probs)==3L)
  samps <- matrix(0, nrow=length(tA), ncol=length(probs))
  
  # Use rmultinom for each count in tA
  for (i in 1:length(tA)) {
    samps[i, ] <- rmultinom(1, size=tA[i], prob=probs*c(S,I,R))
  }
  # samps <- rmultinom(nk, tA, probs*c(S,I,R))

  return(list(numS=samps[, 1L], numI=samps[, 2L], numR=samps[, 3L]))

}




## sampling with replacement changes individuals into S and R (outflow):

# P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# P_I <- 0
# P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# if(is.na(P_S)){
#   probs <- c(1,0,0)
# } else{
#   probs <- c(P_S, P_I, P_Rec)
# }
# All <- sum(S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i])

sample_with_changes <- function(S, I, R, probs, All){

  if(length(All)==0L) return(list(numS=numeric(0L), numI=numeric(0L), numR=numeric(0L)))
  stopifnot(length(S)==1L, length(I)==1L, length(R)==1L, length(probs)==3L)

  samps <- matrix(0, nrow=length(All), ncol=length(probs))
  
  # Use rmultinom for each count in tA
  for (i in 1:length(All)) {
    samps[i, ] <- rmultinom(1, size=All[i], prob=probs*c(S,I,R))
  }
  # samps <- rmultinom(length(All), All, probs*c(S,I,R))

  return(list(numS=samps[, 1L], numI=samps[, 2L], numR=samps[, 3L]))

}

## sampling with replacement changes individuals into S and R (inflow)
# P_S <-  sum(S_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# P_I <- probability[j+1,i]
# P_Rec <- sum(Rec_list_A[,j,i])/sum(S_list_A[,j,i]+I_list_A[,j,i]+Rec_list_A[,j,i])
# if(is.na(P_S)){
#   probs <- c(1,0,0)
# } else{
#   probs <- c(P_S, P_I, P_Rec)
# }
# All <- sum(S_list_A[k,j,i]+I_list_A[k,j,i]+Rec_list_A[k,j,i])



