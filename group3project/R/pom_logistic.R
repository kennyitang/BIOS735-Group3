#' Standard logistic function
#' 
#' This is the standard logistic function to transform real numbers into the (0,1) range.
#' 
#' The standard logistic function is given by f(t) = 1 / (1 + exp(-t)). 
#' 
#' @param t a number or a numeric vector.
#' 
#' @return The transformed t.
#' 
#' @examples
#' For a logistic regression model, the estimated probability of an event associated 
#' with covariates Xi can be calculated as:
#' logistic(a + Xi %*% beta_hat)
#'
#' 

logistic = function(t){
  logis = 1 / (1 + exp(-t))
  return(logis)
}

