#' POM gradient
#' 
#' This function spans the data matrix according to the user input formula and removes the intercept, as intercepts
#' are modeled in the likelihood.
#' It uses the built in model.matrix function so that logical and string variables are converted to indicator variables.
#' 
#' @param formula a formula object connected by "~", using the column names of the data matrix
#' @param data N x C data matrix without intercept. C is the number of variables
#' 
#' @return the model matrix coded with indicator variables and without intercepts.
#' 
#' @examples
#'
#' X2 = recode.factor(Severity_c ~ Source + Traffic_Signal + Sunrise_Sunset + weekday + interstate, data = tst_data)
#' 
#'
recode.factor = function(formula, data){
  # dependent variable needs to be factor
  A = model.matrix(formula, data = as.data.frame(data))
  X = A[,-1] # intecept already in the likelihood
  return(X) 
}
