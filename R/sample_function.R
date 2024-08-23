#' Sample 1
#'
#' @param S 
#' @param I 
#' @param R 
#' @param tA 

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

#' Sample 2
#'
#' @param S 
#' @param I 
#' @param R 
#' @param tA 
#' @param probs 
#'
#' @return
#' @export
#'
#' @examples
sample_with_replacement <- function(S, I, R, tA, probs){

  nk <- length(tA)
  if(nk==0L) return(list(numS=numeric(0L), numI=numeric(0L), numR=numeric(0L)))
  stopifnot(length(S)==1L, length(I)==1L, length(R)==1L, length(probs)==3L)
  samps <- matrix(0, nrow=length(tA), ncol=length(probs))
  
  # Use rmultinom for each count in tA
  for (i in 1:length(tA)) {
    samps[i, ] <- rmultinom(1, size=tA[i], prob=probs*c(S,I,R))
  }

  return(list(numS=samps[, 1L], numI=samps[, 2L], numR=samps[, 3L]))

}

#' Sample 3
#'
#' @param S 
#' @param I 
#' @param R 
#' @param probs 
#' @param All 
#'
#' @return
#' @export
#'
#' @examples
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