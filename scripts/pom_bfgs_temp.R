#POM likelihood and gradient, reference Woodridge (2002, p656)

#yi = 0, 1, ..., J (J+1 response categories)
#a = a1, ..., aj (J cutoff parameters)

# y needs to start at zero for this code to work


logistic = function(t){
  logis = 1 / (1 + exp(-t))
  return(logis)
}

##Y needs to start at zero.  
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
    
    X.mid = as.matrix(X[y == unique(y)[i],])
    subtrac = logistic(a_vec[i] - X.mid %*% beta) - 
                    logistic(a_vec[i - 1] - X.mid %*% beta)
    subtrac = ifelse(subtrac < 0, 0, subtrac) # bound the lilkelihood to avoid NaN
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
      
      X.mid = as.matrix(X[y == unique(y)[i],])
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
  X2.a = as.matrix(X[y == unique(y)[2],])
  XJ.a = as.matrix(X[y == max(y),])
  XJm1.a = as.matrix(X[y == unique(y)[length(unique(y))-1],])
  
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
    
    Xmid.a = as.matrix(X[y == unique(y)[i],])
    Xmid.a.p1 = as.matrix(X[y == unique(y)[i+1],])
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

####trying out on an example##########

#Example data: dat$pol.ideology. IV: dat$party
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

#initial values
init.a_vec = seq(-2, 2, length.out = length(unique(y)) - 1) #should be half negative and half positive
init.beta = rep(-0.5, ncol(X))
par = c(init.a_vec, init.beta)
tol = 10^-4


#check gradient first
grchk(par, function(x, X, y){loglik.pom(param = x, y=y, X=X)} , 
      function(x, X, y){gradient.pom(param = x, y=y, X=X)}, trace=100, y = y,X = X)

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
  )
)

print(fit) 

#optimHess(par, fn, gr = NULL, ..., control = list())

# fit proportional odds model
library(MASS)
pom <- polr(pol.ideology ~ party, data=dat)
summary(pom)

############Fitting the actual training data set#########
library(MASS)
library(data.table)
trn_data = fread("~/Github/BIOS735-Group3/data/NC_Accidents_trn.csv")
tst_data = fread("~/Github/BIOS735-Group3/data/NC_Accidents_tst.csv")

trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)

#need factors that starts at zero, and recode string and logicals in the data
y = factor(trn_data$Severity, levels = c(1,2,3), labels = c(0:(length(unique(trn_data$Severity))-1)), ordered = T)
X = trn_data[, c("Source", "Side", "Temperature(F)", "Humidity(%)", "Pressure(in)", 
                 "Visibility(mi)","Wind_Speed(mph)", "Crossing", "Traffic_Signal",
                 "Sunrise_Sunset", "weekday", "interstate")]
strTmp = c("Source", "Side","Sunrise_Sunset")

ind <- match(strTmp, names(X))
for (i in seq_along(ind)) {
  set(X, NULL, ind[i], as.numeric(as.factor(X[[ind[i]]]))) # need to check coding scheme for characters
}
strTmp = c("Crossing", "Traffic_Signal","weekday", "interstate")
ind <- match(strTmp, names(X))
for (i in seq_along(ind)) {
  set(X, NULL, ind[i], as.numeric(X[[ind[i]]]))
}


init.a_vec = seq(-0.5, 0.5, length.out = length(unique(y)) - 1) #should be half negative and half positive
init.beta = rep(-0.05, ncol(X))
tol = 10^-4

#how to embed library(optimx) in package?
library(optimx)

par = c(init.a_vec, init.beta)
#checking the gradient on the intial parameters
grchk(par, function(x, X, y){loglik.pom(param = x, y=y, X=X)} , 
      function(x, X, y){gradient.pom(param = x, y=y, X=X)}, trace=100, y = y,X = X)

nc_fit = optimx(
  par = c(init.a_vec, init.beta), # initial values for the parameters. 
  fn = function(x, X, y){loglik.pom(param = x, y=y, X=X)}, # log likelihood
  #  gr = NULL, # use this to debug fn and gr separately
  gr = function(x, X, y){gradient.pom(param = x, y=y, X=X)}, # gradient/1st derivative
  method = "BFGS",
  y = y,
  X = X,
  hessian = F,
  #itmax = maxit, # max number of iterations
  control = list(
    trace = 100, # higher number print more detailed output
    maximize = T, # default is to minimize
    abstol= tol
  )
)
print(nc_fit) # polr is dummy variable coding for categorical output. Tis is not yet.


#fitting with polr
set.seed(11)
start = Sys.time()
nc_trn = polr(Severity_ord ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
                `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
                Sunrise_Sunset + weekday + interstate, data = trn_data)
end = Sys.time()
print(end - start)
summary(nc_trn)

