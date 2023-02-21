## All Weibull Functions are described by a list called "wp", this list contains the weibull parameters shape, scale, location.

#load Packages
library(fitdistrplus)

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

# Weibull Reliability Function
WR <- function(wp,t){exp(-(((t-wp$location)/wp$scale)^(wp$shape)))}

inv_WR <- function(wp,r){(wp$scale*((-log(r))^(1/wp$shape)))+wp$location}

# Weibull Cumulative Distribution Function
Wcdf <- function(wp,t){1-(exp(-(((t-wp$location)/wp$scale)^(wp$shape))))}

inv_Wcdf <- function(wp,nr){(wp$scale*((-log(1-nr))^(1/wp$shape)))+wp$location}

# Weibull Hazard Function
WH <- function(wp,t){(wp$shape/wp$scale)*(((t-wp$location)/wp$scale)^(wp$shape-1))}

# Weibull Mean
WMean <- function(wp){wp$location+(wp$scale*gamma(1+(1/wp$shape)))}

# Weibull Median
WMedian <- function(wp){wp$location+wp$scale*((log(2)^(1/wp$shape)))}

# Weibull Mode
WMode <- function(wp){wp$location+(wp$scale*((wp$shape-1)/wp$shape)^(1/wp$shape))}

# Weibull Variance
Wvar <- function(wp){((wp$scale)^2)*((gamma(1+(2/wp$shape)))-((gamma(1+(1/wp$shape)))^2))}

# Weibull Standard Deviation
Wsd <- function(wp){(((wp$scale)^2)*((gamma(1+(2/wp$shape)))-((gamma(1+(1/wp$shape)))^2)))^(1/2)}

# Weibull Skewness
Wskewness <- function(wp){(((wp$scale^3)*(gamma(1+(3/wp$shape))))-(3*WMean(wp)*(Wsd(wp)^3))-(WMean(wp)^3))/(Wsd(wp)^3)}

# Weibull Shannon Entropy
Wentropy <- function(wp){(-digamma(1))*(1-(1/wp$shape))+log(wp$scale/wp$shape)+1}
