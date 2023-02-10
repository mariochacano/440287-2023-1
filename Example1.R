#Load Packages
library(tidyverse)
library(fitdistrplus)

# Load Weibull Functions
source(paste(getwd(),"/WeibullFunctions.R", sep=""))

# Parameters from fitting weibull MLE
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

# Example 1
Dataset <- c(110,120,220,220,220,330)
wp <- fwp(Dataset)
(MTBF <- WMean(wp))
