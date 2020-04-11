#POM likelihood and gradient, reference Woodridge

#yi = 0, 1, ..., J (J+1 response categories)
#a = a1, ..., aj (J cutoff parameters)

#Example data: dat$pol.ideology. IV: dat$party

#recode data to numbers starting from 0
party <- factor(rep(c("Rep","Dem"), c(407, 428)), 
                levels=c("Rep","Dem"))  
rpi <- c(30, 46, 148, 84, 99) # cell counts
dpi <- c(80, 81, 171, 41, 55) # cell counts
ideology <- c("Very Liberal","Slightly Liberal","Moderate","Slightly Conservative","Very Conservative")
pol.ideology <- factor(c(rep(ideology, rpi), 
                         rep(ideology, dpi)), levels = ideology)
dat <- data.frame(party,pol.ideology)

y = factor(dat$pol.ideology, levels = unique(dat$pol.ideology), labels = c(0:(length(unique(dat$pol.ideology))-1)), ordered = T)
X = as.matrix(as.numeric(factor(dat$party, levels = unique(dat$party), labels = c(1:(length(unique(dat$party)))), ordered = F)))


#parameters
a_vec = seq(-2, 2, length.out = length(unique(y)) - 1)
beta = rep(-0.5, ncol(X))

logistic = function(t){
  logis = 1 / (1 + exp(-t))
  return(logis)
}

# loglikelihood function
loglik.pom = function(y, X, param){
  
  a_vec = param[1:(length(unique(y)) - 1)]
  beta = param[-(1:length(unique(y)) - 1)]
  
  loglikesum = 0
  catgry = length(a_vec)
  
  #indicator matrix. One column for each response category.
  #use this to subset data matrix by response category values
  
  # y needs to start at zero for this to work!
  
  Z = matrix(NA, nrow = nrow(X), ncol = length(unique(y)))
  for (j in 1: ncol(Z)){
    Z[,j] = as.numeric(I(y) == (j - 1))
  }
  X1 = as.matrix(X[(diag(Z[,1]) %*% X != 0),])
  XJ = as.matrix(X[(diag(Z[,ncol(Z)]) %*% X) != 0,])
  
  #likelihood for the first and last threshold values
  # diag(Z[,i]) %*% X leaves all X row values zero except for those rows with y == i
  value1 = sum(log(logistic(a_vec[1] - X1 %*% beta))) + 
    sum(log(1 - logistic(a_vec[catgry] - XJ %*% beta)))
  
  #likelihood for the middle threshold values
  value2 = 0 
  for (i in 2: length(a_vec) ){
    
    X.mid = as.matrix(X[(diag(Z[,i]) %*% X) != 0,])
    subtrac = logistic(a_vec[i] - X.mid %*% beta) - 
                    logistic(a_vec[i - 1] - X.mid %*% beta)
    subtrac = ifelse(subtrac < 0, 0, subtrac)
    mid.ll = sum(log(subtrac)) # same set of X rows for two cutoff values
    value2 = value2 + mid.ll
    
  }
    
  loglikesum = value1 + value2
  return(loglikesum)
}


# lets write a function for the first derivative
gradient.pom = function(param, y, X){
  #columne vector of p*1, p = # of beta + # of a
  a_vec = param[1:(length(unique(y)) - 1)]
  beta = param[-(1:length(unique(y)) - 1)]
  
  k = length(beta)
  catgry = length(a_vec)
  d1.beta = rep(NA, k)
  d1.a = rep(NA, catgry)
  
  Z = matrix(NA, nrow = nrow(X), ncol = length(unique(y)))
  for (j in 1: ncol(Z)){
    Z[,j] = as.numeric(I(y) == (j - 1) )
  }
  
  #beta
  #loop through betas and each predictor, result needs to be column summed
  for (kk in 1:k){
    
    #d1 for the first and last threshold values
    # diag(Z[,i]) %*% X leaves all X row values zero except for those rows with y == i
    # (diag(Z[,i]) %*% X[,kk]) %*% beta[kk] should mean using X values associated with response value i and the kk_th beta parameter
    X1 = as.matrix(X[(diag(Z[,1]) %*% X != 0),])
    XJ = as.matrix(X[(diag(Z[,ncol(Z)]) %*% X) != 0,])
    
    value1 = 0
    value1 =  sum(- X1[,kk] * exp(X1 %*% beta - a_vec[1]) / (1 + exp(X1 %*% beta - a_vec[1]))) +
       sum(XJ[,kk] / (1 + exp(XJ %*% beta - a_vec[catgry])))
      
    #d1 for the middle threshold values
    value2 = 0 
    for (i in 2: length(a_vec) ){
      
      X.mid = as.matrix(X[(diag(Z[,i]) %*% X) != 0,])
      mid.d1 = sum( 1 / (logistic(a_vec[i] - X.mid %*% beta ) - logistic(a_vec[i-1] - X.mid %*% beta) ) * 
        ( X.mid[,kk] * exp(X.mid %*% beta - a_vec[i-1]) / (1 + exp(X.mid %*% beta - a_vec[i-1]))^2 - 
            X.mid[,kk] * exp(X.mid %*% beta - a_vec[i]) / (1 + exp(X.mid %*% beta - a_vec[i]))^2 ) )
        
      value2 = value2 + mid.d1
    }
    d1.beta[kk] = value1 + value2
  }
  
  #alpha
  #d1 for the first and last threshold values
  # diag(Z[,i]) %*% X leaves all X row values zero except for those rows with y == i
  # (diag(Z[,i]) %*% X[,kk]) %*% beta[kk] should mean using X values associated with response value i and the kk_th beta parameter
  X1.a = as.matrix(X[(diag(Z[,1]) %*% X != 0),])
  X2.a = as.matrix(X[(diag(Z[,2]) %*% X != 0),])
  XJm1.a = as.matrix(X[(diag(Z[,ncol(Z)-1]) %*% X) != 0,])
  XJ.a = as.matrix(X[(diag(Z[,ncol(Z)]) %*% X) != 0,])
  
  value1 = 0
  value1 = sum( exp(X1.a %*% beta - a_vec[1]) / (1 + exp(X1.a %*% beta - a_vec[1]))) - 
                sum( logistic(a_vec[1] - X2.a %*% beta) * (1 - logistic(a_vec[1] - X2.a %*% beta)) / 
                  ( logistic(a_vec[2] - X2.a %*% beta) - logistic(a_vec[1] - X2.a %*% beta) ) )
  valuej = 0
  valuej = sum( logistic(a_vec[catgry] - XJm1.a %*% beta)*(1-logistic(a_vec[catgry] - XJm1.a %*% beta) ) / 
                  (logistic(a_vec[catgry] - XJm1.a %*% beta) - logistic(a_vec[catgry-1] - XJm1.a %*% beta)) ) - 
                  sum( 1 / (1 + exp( XJ.a %*% beta - a_vec[catgry])) )
  
  #d1 for the middle threshold values
  valuem = rep(0, catgry - 2)
  for (i in 2: (catgry -1) ){
    
    Xmid.a = as.matrix(X[(diag(Z[,i]) %*% X) != 0,])
    Xmid.a.p1 = as.matrix(X[(diag(Z[,i+1]) %*% X) != 0,])
    valuem[i-1] = sum( 1 / (logistic(a_vec[i] - Xmid.a %*% beta) - logistic(a_vec[i-1] - Xmid.a %*% beta) ) * 
                         ( exp(Xmid.a - a_vec[i]) / (1 + exp(Xmid.a %*% beta - a_vec[i]))^2 ) ) - 
      sum( logistic(a_vec[i] - Xmid.a.p1 %*% beta) * (1 - logistic(a_vec[i] - Xmid.a.p1 %*% beta)) / 
        (logistic(a_vec[i+1] - Xmid.a.p1 %*% beta) - logistic(a_vec[i] - Xmid.a.p1 %*% beta) ) )
  }

  d1.a = c(value1, valuem, valuej)
  
  return(c(d1.a, d1.beta))
}

####trying out##########

#initial values
init.a_vec = seq(-2, 2, length.out = length(unique(y)) - 1) #should be half negative and half positive
init.beta = rep(-0.5, ncol(X))
tol = 10^-4

library(optimx)
fit = optimx(
  par = c(init.a_vec, init.beta), # initial values for the parameters. 
  fn = function(x, X, y){loglik.pom(param = x, y=y, X=X)}, # log likelihood
#  gr = NULL, # use this to debug fn and gr separately
  gr = function(x, X, y){gradient.pom(param = x, y=y, X=X)}, # gradient/1st derivative
  method = "BFGS",
  y = y,
  X = X,
  #itmax = maxit, # max number of iterations
  control = list(
    trace = 100, # higher number print more detailed output
    maximize = T, # default is to minimize
    abstol= tol
    # parscale = vector of scaling values to correct for scale differencs in parameters.  Should be the same length as the num of parameters
  )
)

grchk(par, function(x, X, y){loglik.pom(param = x, y=y, X=X)} , 
      function(x, X, y){gradient.pom(param = x, y=y, X=X)}, trace=100, y = y,X = X)

print(fit) 


optimHess(par, fn, gr = NULL, ..., control = list())


###########Testing#############

# fit proportional odds model
library(MASS)
pom <- polr(pol.ideology ~ party, data=dat)
summary(pom)

loglik.pom(y, X, par)
gradient.pom(par, y, X) 

library(numDeriv)
numgrad <- grad(loglik.pom, X, y=y, param=par)

