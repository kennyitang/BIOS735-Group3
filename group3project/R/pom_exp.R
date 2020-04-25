#' Proportional odds model parameter estimation 
#' 
#' This function returns the intercept and coefficient estimates of the ordered logit model, allowing
#' an optional standard error output.
#' 
#' @param pom.formula a formula object connected with "~" using the column names of the data matrix
#' @param data.input N x C data matrix without intercept. C is the number of variables. The output gives P coefficients
#' after recoding categorical variables into indicator variables. The function only accept ordered factor as the response variable type.
#' The function will check KKT optimality conditions for the second derivative, and the output will suggest that continuous predictors be scaled first, if necessary, 
#' in order to stablize the Hessian matrix approximation.
#' @param SE logical; if TRUE, ouputs standard error
#' @param details logical; if TRUE, gives detailed optim outcome
#'
#' @return A coefficient estimate matrix and optionally the SE with P + J - 1 rows, with J being the number of response categories.
#' 
#' @examples
#'
#' trn_data$Severity_c = factor(trn_data$Severity, levels = c(1,2,3), 
#' labels = c(1:length(unique(trn_data$Severity))), ordered = T)
#' trn_data$temperature_sc = scale(trn_data$`Temperature(F)`)
#' trn_data$humdity_sc = scale(trn_data$`Humidity(%)`)
#' trn_data$pressure_sc = scale(trn_data$`Pressure(in)`)
#' trn_data$windspeed_sc = scale(trn_data$`Wind_Speed(mph)`)
#' trn_data$Visibility_sc = scale(trn_data$`Visibility(mi)`)
#' export.try = pom.est(Severity_c ~ Source + Side + temperature_sc + humdity_sc + pressure_sc + 
#'                     Visibility_sc + windspeed_sc + Crossing + Traffic_Signal +
#'                     Sunrise_Sunset + weekday + interstate, data = trn_data, SE = T, details = F)
#' export.try
#' 
#' @importFrom optimx optimx
#'
#' @export

#need to use ordered factor y and scaled x

pom.est = function(pom.formula, data.input, SE = T, details = T){
  
  #extract y and convert to ordered factors
  y = data.input[, which(colnames(data.input) == all.vars(pom.formula)[1])]
  if (is.ordered(y) == F ){ stop("Dependent variable must be an ordered factor.")}
  
  #convert formula to data matrix
  X = recode.factor(formula = pom.formula, data = data.input)
  
  # Initial values, can work to allow for better initial values
  init.a_vec = seq(-1, 0.5, length.out = length(unique(y)) - 1)
  init.beta = rep(-0.05, ncol(X))
  tol = 10^-6
  maxit = 50
  
  # Fitting
  fit = optimx::optimx(
    #par = c(a = coef.polr[14:15,1], b = coef.polr[1:13,1]), #polr() use fitted values.
    par = c(a = init.a_vec, b = init.beta), 
    fn = function(x, X, y){loglik.pom(param = x, y=y, X=X)}, # log likelihood
    gr = function(x, X, y){gradient.pom(param = x, y=y, X=X)}, # gradient/1st derivative
    method = "BFGS",
    y = y,
    X = X,
    hessian = SE,
    control = list(
      trace = ifelse(details, 1000, 0), # higher number print more detailed output
      maximize = T, # default is to minimize
      abstol= tol,
      kkt = SE
    )
  )
  
  length.param = length( c(a = init.a_vec, b = init.beta))
  est = fit[,1:length.param]
  
  if (SE == T){
    if ( fit$kkt2 == F ){warning("Hessian optimality conditions failed. Try scaling continuous variables first.")}
    optim.hess = attr(fit, "details")[,"nhatend"][[1]]
    optim.se = sqrt(diag(solve(-optim.hess)))
    
    est.mat = data.frame(matrix(NA, nrow = length.param, ncol = 2))
    colnames(est.mat) = c("Estimates", "SE")
    rownames(est.mat) = c(paste0("Intercept", 1:length(init.a_vec)), colnames(X))
    est.mat$Estimates = as.numeric(est)
    est.mat$SE = optim.se
  } else {
    
    est.mat = data.frame(matrix(NA, nrow = length.param, ncol = 1))
    colnames(est.mat) = c("Estimates")
    rownames(est.mat) = c(paste0("Intercept", 1:length(init.a_vec)), colnames(X))
    est.mat$Estimates = as.numeric(est)
    message("Standard error not computed. Set SE = T to recompute.")
  }
  return(est.mat)
}
