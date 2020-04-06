# need a function wrapper around the whole code chunk below

# from user input function: X & beta dimension P, y, link variable, maxit?, other things? 
        # option to print iteration history? 
        # by link do you mean family/distribution?
# need checks on data dimensions and range
X
P 
y
link

tol = 10^-4
maxit = 50
iter = 0
eps = 10^10

if (tolower(link) == "logit"){
  
  log.lik = function(pi, y){
    
    update = sum( y * log(pi / (1 - pi) + log(1 - pi) ))
    return(update)
    
  }
  
  beta = c(log(mean(y) / (1 - mean(y))), rep(0, P - 1)) # initial logistic parameter values
  
  while (eps > tol & iter < maxit){
    
    beta0 = beta # updating beta
    
    eta_t = X %*% beta0 # N*1 eta vector = theta = XB
    
    e = 1 / (eta_t * (1 - eta_t)) * (y - 1 / (1 + exp(-eta_t))) # N*1 error vector
    
    z = eta_t + e # N*1 linear model term
    
    w = solve((exp(eta_t)+exp(-eta_t)+2)^2 * exp(-eta_t) * (1+exp(-eta_t)) ^(-2))
    
    W = diag(as.vector(w)) # weight matrix
    
    #solve for new beta estimate
    beta = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z #p*1 vector, copied from Poisson case
    
    #updating log likelihood and convergence
    eta_t1 = X %*% beta
    logL = log.lik(1 / (1 + exp(-eta_t1)) , y)
    eps = sqrt(sum((beta - beta0)^2))
    
    #iteration
    iter = iter +1
    if(iter == maxit) warning("Iteration limit reached without convergence")
    
    cat(sprintf("Iter: %d logL: %.2f eps:%f\n",iter, logL, eps))
    
  }
  
  
} else if (tolower(link) == "pois" ){
  
  log.lik = function(lambda, y){
    update = sum(dpois(y, lambda = lambda, log = T))
    return(update)
  }
  
  beta = c(log(mean(y)), rep(0, P - 1)) # initial poisson parameter values
  
  while (eps > tol & iter < maxit){
    
    beta0 = beta # updating beta
    
    eta_t = X %*% beta0 # N*1 eta vector = theta = XB
    
    e = exp(-eta_t) * (y - exp(eta_t)) # N*1 error vector
    
    z = eta_t + e # N*1 linear model term
    
    w = exp(eta_t)
    
    W = diag(as.vector(w)) # weight matrix
    
    #solve for new beta estimate
    beta = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z #p*1 vector
    
    #updating log likelihood and convergence
    eta_t1 = X %*% beta
    logL = log.lik(exp(eta_t1), y)
    eps = sqrt(sum((beta - beta0)^2))
    
    #iteration
    iter = iter +1
    if(iter == maxit) warning("Iteration limit reached without convergence")
    
    cat(sprintf("Iter: %d logL: %.2f eps:%f\n",iter, logL, eps))
    
  }
  
} else {
  stop("Link function unavailable")
}

return(beta) 
