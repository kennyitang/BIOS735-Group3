#This scripts tests the functions for proportional odds model estimation
#before adding them into the R package. It produces the confusion matrix and plots
#estimates against those from MASS:polr()


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


recode.factor = function(formula, data){
  # dependent variable needs to be factor
  A = model.matrix(formula, data = as.data.frame(data))
  X = A[,-1] # intecept already in the likelihood
  return(X) 
}

############Fitting the actual training data set#########
library(MASS)
library(data.table)
trn_data = fread("~/Github/BIOS735-Group3/data/NC_Accidents_trn.csv")
tst_data = fread("~/Github/BIOS735-Group3/data/NC_Accidents_tst.csv")

# Recode y into factor - Training data set
trn_data$Severity_c = as.factor(trn_data$Severity)
tst_data$Severity_c = as.factor(tst_data$Severity)
y = factor(trn_data$Severity, levels = c(1,2,3), 
           labels = c(1:length(unique(trn_data$Severity))), ordered = T)

# Build model matrix (with intercept left out)
X = as.matrix(recode.factor(Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
                    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
                    Sunrise_Sunset + weekday + interstate, data = trn_data))
X[,4:8] = scale(X[,4:8])

# Initial values
init.a_vec = seq(-1, 0.5, length.out = length(unique(y)) - 1) #possibly not the best starting values
init.beta = rep(-0.05, ncol(X))
tol = 10^-6

#polr() uses glm.fit to get starting values, still trying to do the same here
#glm = glm.fit(cbind(1,X), y, family = binomial("logit")) # how to specify offset??
# glm$coefficients

# Fitting
set.seed(11)
start = Sys.time()
nc_fit = optimx::optimx(
  #par = c(a = coef.polr[14:15,1], b = coef.polr[1:13,1]), #polr() use fitted values.
  par = c(a = init.a_vec, b = init.beta), 
  fn = function(x, X, y){loglik.pom(param = x, y=y, X=X)}, # log likelihood
  gr = function(x, X, y){gradient.pom(param = x, y=y, X=X)}, # gradient/1st derivative
  method = "BFGS",
  y = y,
  X = X,
  hessian = T,
  #itmax = maxit, # max number of iterations
  control = list(
    trace = 1000, # higher number print more detailed output
    maximize = T, # default is to minimize
    abstol= tol,
    kkt = T
  )
)
end = Sys.time() 
print(end - start) # 1.5 mins with Hessian
print(nc_fit) 
#hessian still negative
optim.hess = attr(nc_fit, "details")[,"nhatend"][[1]]
optim.se = sqrt(diag(solve(-optim.hess)))

###Training set prediction
ahat <-  as.numeric(nc_fit[1:2])
bhat <- as.numeric(nc_fit[3:15])   
logit1 <- ahat[1] - X %*% bhat
logit2 <- ahat[2] - X %*% bhat
pLeq1  <- 1 / (1 + exp(-logit1))   # p(Y <= 1)
pLeq2  <- 1 / (1 + exp(-logit2))   # p(Y <= 2)
pMat   <- cbind(p1=pLeq1, p2=pLeq2-pLeq1, p3=1-pLeq2)  # matrix p(Y = g)
categHat <- levels(y)[max.col(pMat)]
#Training error
sum(categHat != y) / length(y) #0.07407066

###Test set prediction
X2 = recode.factor(Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
                    `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
                    Sunrise_Sunset + weekday + interstate, data = tst_data)
X2[,4:8] = scale(X2[,4:8])
y2 = factor(tst_data$Severity, levels = c(1,2,3), 
           labels = c(1:length(unique(tst_data$Severity))), ordered = T)

ahat <-  as.numeric(nc_fit[1:2])
bhat <- as.numeric(nc_fit[3:15])   
logit1 <- ahat[1] - X2 %*% bhat
logit2 <- ahat[2] - X2 %*% bhat
pLeq1  <- 1 / (1 + exp(-logit1))   # p(Y <= 1)
pLeq2  <- 1 / (1 + exp(-logit2))   # p(Y <= 2)
pMat   <- cbind(p1=pLeq1, p2=pLeq2-pLeq1, p3=1-pLeq2)  # matrix p(Y = g)
categHat2 <- as.numeric(levels(y2)[max.col(pMat)])
#Test error
sum(categHat2 != y2) / length(y2) #0.09732882
1- sum(categHat2 != y2) / length(y2) # failed to predict level 3 accidents. All 1 and 2 in the data.

#Kappa and confusion matrix
library(caret)
categHat2 = factor(categHat2, levels = c(1,2,3), labels = c(1,2,3), ordered = T)
confusionMatrix(data=categHat2, reference=y2)


###########compare with MASS:polr and plot######
#fitting with polr
set.seed(11)
start = Sys.time()
nc_trn = polr(Severity_c ~ Source + Side + `Temperature(F)` + `Humidity(%)` + `Pressure(in)` + 
                `Visibility(mi)` + `Wind_Speed(mph)` + Crossing + Traffic_Signal +
                Sunrise_Sunset + weekday + interstate , data = trn_data, Hess = T)
end = Sys.time()
print(end - start)
sqrt(diag(solve(nc_trn$Hessian)))

coef.polr = summary(nc_trn)$coefficients

nc_yx = polr(y ~ X)
coef.polrs = summary(nc_yx)$coefficients 
#Phat <- predict(nc_trn, type="probs") 
#categHat.pol <- levels(y)[max.col(Phat)] 
#sum(categHat.pol != y) / length(y)

#Plotting coefficients
est.mat = coef.polr[,1:2]
est.mat[,1:2] = coef.polrs[,1:2]
est.mat = as.data.frame(cbind(est.mat, 0, 0) ) 
colnames(est.mat) = c("MASS", "MASS.se", "OptimFunction", "Optim.se")
est.mat[1:length(beta),3] = as.numeric(nc_fit[(length(a_vec)+1):length(c(a_vec,beta))])
est.mat[(length(beta)+1):length(c(a_vec,beta)),3] = as.numeric(nc_fit[1:length(a_vec)])
est.mat[1:length(beta),4] = optim.se[(length(a_vec)+1):length(c(a_vec,beta))]
est.mat[(length(beta)+1):length(c(a_vec,beta)),4] = optim.se[1:length(a_vec)]
est.mat$var = rownames(est.mat)
est.mat = est.mat[1:length(beta),]

library(reshape2)
est.melt <- melt(est.mat, id.vars = c("var"), measure.vars = c("MASS", "OptimFunction"),
                 variable.name = "Function", value.name = "Estimates" )
est.melt$SE <- c(est.mat$MASS.se, est.mat$Optim.se)

library(ggplot2)
g <- ggplot(est.melt, aes(x = reorder(as.factor(var), -abs(Estimates)), y = Estimates, fill = Function)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge())+ 
  geom_errorbar(aes(ymin=Estimates-SE, ymax=Estimates+SE), width = .2,
                position = position_dodge(0.9)) + xlab("Predictors") + theme_bw()+
  theme(text = element_text(size=20),axis.text.x = element_text(size = 18,angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(limits = c(-5,5)) + scale_fill_manual(values=c('#ED7D31','#4472C4'))+
  ggtitle("Prop. Odds Model Coefficient Estimates")
plot(g)
