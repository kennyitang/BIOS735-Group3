#' 
#' This is the logistic function to transform the linear function into the (0,1) range 
#' 
#' @param t a linear function
#' 
#' @return the logistic function
#' 
#' @examples
#'
#' logistic( a + X %*% beta)
#'
#' 

logistic = function(t){
  logis = 1 / (1 + exp(-t))
  return(logis)
}

