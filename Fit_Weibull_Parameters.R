#load Packages
library(fitdistrplus)

#Parameters from fitting weibull from linear regression
fit_Wp_LR <- function(df){
  model <- lm(y ~ x, data =df)
  swp <- summary(model)
  .intercept <- swp$coefficients[1] #intercept of linear regression
  .slope <- swp$coefficients[2] #slope of linear regression
  
  .beta =.slope	                      #shape
  .eta <- exp(-(.intercept/.slope))		#scale
  .gamma <- 0                         #location
  .r2 <- swp$r.squared       #R-squared
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma, "R2"=.r2)
  return(result)
}

#Parameters from fitting weibull maximum likelihood estimation
fit_Wp_MLE <- function(datavec){
  swp <- fitdist(datavec, distr="weibull", method="mle")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  .AIC <- swp$aic                             #Akaike information criterion
  .BIC <- swp$bic                             #Bayesian information criterion
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma, "AIC"=.AIC, "BIC"=.BIC)
  return(result)
}

#Parameters from fitting weibull maximum goodness-of-fit estimation
fit_Wp_MGE <- function(datavec){
  swp <- fitdist(datavec, distr="weibull", method="mge")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  .AIC <- swp$aic                             #Akaike information criterion
  .BIC <- swp$bic                             #Bayesian information criterion
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma, "AIC"=.AIC, "BIC"=.BIC)
  return(result)
}

#Parameters from fitting weibull maximum spacing estimation
fit_Wp_MSE <- function(datavec){
  swp <- fitdist(datavec, distr="weibull", method="mse")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  .AIC <- swp$aic                             #Akaike information criterion
  .BIC <- swp$bic                             #Bayesian information criterion
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma, "AIC"=.AIC, "BIC"=.BIC)
  return(result)
}

#Parameters from fitting weibull quantile matching estimation
fit_Wp_QME <- function(datavec){
  swp <- fitdist(datavec, distr="weibull", method="qme")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma)
  return(result)
}

#Parameters from fitting weibull moment matching estimation
fit_Wp_MME <- function(datavec){
  swp <- fitdist(datavec, distr="weibull", method="mme")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma)
  return(result)
}
