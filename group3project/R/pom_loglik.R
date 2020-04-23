#' POM likelihood
#' 
#' This function returns the log likelihood of the ordered logit model, allowing
#' an arbitrary number of J ordered response categories (J>2), referencing Woodridge (2002, p656).
#' 
#' 
#' @param x a N x P data matrix, with no intercept, where categorical variables will have been coded into indicator variables
#' @param y a vector of ordered factor responses with J levels
#' @param param initial values of the alpha and beta parameters of length (J-1+P)
#'
#' @return the log likelihood of proportional odds model
#' 
#' @examples
#'
#' loglike.pom(y, X, c(a_vec, beta_vec))
#'
#' 
  
# loglikelihood function
loglik.pom = function(y, X, param){
  
  a_vec = param[1:(length(unique(y)) - 1)]
  beta = param[-(1:length(unique(y)) - 1)]
  
  loglikesum = 0
  catgry = length(a_vec)
  
  X1 = as.matrix(X[y == min(y),])
  XJ = as.matrix(X[y == max(y),])
  
  #likelihood for the first and last threshold values
  value1 = sum(log(logistic(a_vec[1] - X1 %*% beta))) + 
    sum(log(1 - logistic(a_vec[catgry] - XJ %*% beta)))
  
  #likelihood for the middle parts
  value2 = 0 
  for (i in 2: length(a_vec) ){
    
    X.mid = as.matrix(X[y == as.numeric(levels(y)[i]),])
    subtrac = logistic(a_vec[i] - X.mid %*% beta) - 
                    logistic(a_vec[i - 1] - X.mid %*% beta)
    subtrac = ifelse(subtrac < 0, 0, subtrac) # bound the lilkelihood to avoid NaN
    mid.ll = sum(log(subtrac)) # same set of X rows for two cutoff values
    value2 = value2 + mid.ll
    
  }
    
  loglikesum = value1 + value2
  return(loglikesum)
}
