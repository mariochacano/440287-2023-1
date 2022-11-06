#Load Packages
library(readxl)
library(ggplot2)
library(MASS)
library(survival)
library(fitdistrplus)
library(tidyr)
library(dplyr)

# Data for example
Dataset <- c(110,120,220,220,220,330)

#Parameters from fitting weibull MLE
fwp <- function(x){
  swp <- fitdist(x, dist="weibull")
  .beta <- as.numeric(swp$estimate["shape"])	#shape
  .eta <- as.numeric(swp$estimate["scale"])		#scale
  .gamma <- 0                                 #location
  print(paste("shape =",.beta))
  print(paste("scale =",.eta))
  print(paste("location =",.gamma))
  result <- list("shape"=.beta, "scale"=.eta, "location"=.gamma)
  return(result)
}

# Weibull Distribution Functions
R <- function(t,wp){exp(-(((t-wp$location)/wp$scale)^(wp$shape)))}
F <- function(t,wp){1-R(t,wp)}
H <- function(t,wp){(wp$shape/wp$scale)*((t/wp$scale)^(wp$shape-1))}
WeibullMean <- function(wp){wp$location+wp$scale*gamma(1+(1/wp$shape))}

# Some Results
wp <- fwp(Dataset)
WeibullMean(wp)
R(0,wp)
