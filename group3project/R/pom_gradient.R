#' Gradient function for proportional odds model
#' 
#' This function returns the gradient of log likelihood of the proportional odds model, allowing for
#' an arbitrary number of J ordered response categories (J>2).
#' 
#' @param x a N x P data matrix, with no intercept, where categorical variables need to be coded into indicator variables.
#' @param y a vector of ordered factor responses with J levels.
#' @param param curent values of the alpha and beta parameters of length (J-1+P).
#'
#' @return The gradient of the log likelihood of proportional odds model.
#' 
#' @examples
#' Given y, X, and the current values of alpha and beta, the gradient of a proportional odds model 
#' at a specific interation can be calculated as:
#' gradient.pom(c(alpha, beta), y, X)
#'

gradient.pom = function(param, y, X){
  #column vector of length p, p = # of beta + # of a
  a_vec = param[1:(length(unique(y)) - 1)]
  beta = param[-(1:length(unique(y)) - 1)]
  
  k = length(beta)
  catgry = length(a_vec)
  d1.beta = rep(NA, k)
  d1.a = rep(NA, catgry)
  
  #beta
  #loop through betas and each predictor, result needs to be column summed
  for (kk in 1:k){
    
    #d1 for the first and last threshold values
    X1 = as.matrix(X[y == min(y),])
    XJ = as.matrix(X[y == max(y),])
    
    value1 = 0
    value1 =  sum(- X1[,kk] * exp(X1 %*% beta - a_vec[1]) / (1 + exp(X1 %*% beta - a_vec[1]))) +
       sum(XJ[,kk] / (1 + exp(XJ %*% beta - a_vec[catgry])))
      
    #d1 for the middle threshold values
    value2 = 0 
    for (i in 2: length(a_vec) ){
      
      X.mid = as.matrix(X[y == as.numeric(levels(y)[i]),])
      mid.d1 = sum( 1 / (logistic(a_vec[i] - X.mid %*% beta ) - logistic(a_vec[i-1] - X.mid %*% beta) ) * 
        ( X.mid[,kk] * exp(X.mid %*% beta - a_vec[i-1]) / (1 + exp(X.mid %*% beta - a_vec[i-1]))^2 - 
            X.mid[,kk] * exp(X.mid %*% beta - a_vec[i]) / (1 + exp(X.mid %*% beta - a_vec[i]))^2 ) )
        
      value2 = value2 + mid.d1
    }
    d1.beta[kk] = value1 + value2
  }
  
  #alpha
  #d1 for the first and last threshold values
  
  X1.a = as.matrix(X[y == min(y),])
  X2.a = as.matrix(X[y == as.numeric(levels(y)[2]),])
  XJ.a = as.matrix(X[y == max(y),])
  XJm1.a = as.matrix(X[y == as.numeric( levels(y)[length(unique(y))-1] ),])
  
  value1 = 0
  value1 = sum( exp(X1.a %*% beta - a_vec[1]) / (1 + exp(X1.a %*% beta - a_vec[1]))) - 
                sum( logistic(a_vec[1] - X2.a %*% beta) * (1 - logistic(a_vec[1] - X2.a %*% beta)) / 
                  ( logistic(a_vec[2] - X2.a %*% beta) - logistic(a_vec[1] - X2.a %*% beta) ) )
  valuej = 0
  valuej = sum( logistic(a_vec[catgry] - XJm1.a %*% beta)*(1-logistic(a_vec[catgry] - XJm1.a %*% beta) ) / 
                  (logistic(a_vec[catgry] - XJm1.a %*% beta) - logistic(a_vec[catgry-1] - XJm1.a %*% beta)) ) - 
                  sum( 1 / (1 + exp( XJ.a %*% beta - a_vec[catgry])) )
  
  #d1 for the middle threshold values
  if (catgry > 2){
    
  valuem = rep(0, catgry - 2)
  for (i in 2: (catgry -1) ){
    
    Xmid.a = as.matrix(X[y == as.numeric( levels(y)[i]) ,])
    Xmid.a.p1 = as.matrix(X[y == as.numeric( levels(y)[i+1] ) ,])
    valuem[i-1] = sum( ( exp(Xmid.a %*% beta - a_vec[i]) / (1 + exp(Xmid.a %*% beta - a_vec[i]))^2 ) / 
                         (logistic(a_vec[i] - Xmid.a %*% beta) - logistic(a_vec[i-1] - Xmid.a %*% beta)) ) - 
      sum( logistic(a_vec[i] - Xmid.a.p1 %*% beta) * (1 - logistic(a_vec[i] - Xmid.a.p1 %*% beta)) / 
        (logistic(a_vec[i+1] - Xmid.a.p1 %*% beta) - logistic(a_vec[i] - Xmid.a.p1 %*% beta) ) )
  }
  d1.a = c(value1, valuem, valuej)} else {
    d1.a = c(value1, valuej)
  }
  
  return(c(d1.a, d1.beta))
}
