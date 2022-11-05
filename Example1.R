#Load Packages
library(MASS)
library(survival)
library(fitdistrplus)
library(tidyr)

# Data
Dataset <- c(110,120,220,220,220,330)

#Parameters from fitting
swp <- fitdist(Dataset, dist="weibull")
.beta <- as.numeric(swp$estimate["shape"])	#shape
.eta <- as.numeric(swp$estimate["scale"])		#scale
.gamma <- 0                                 #location


# FUNCTIONS
R <- function(t){exp(-(((t-.gamma)/.eta)^(.beta)))}
F <- function(t){1-R(t)}
H <- function(t){(.beta/.eta)*((t/.eta)^(.beta-1))}
WeibullMean <- function(.beta,.eta,.gamma){.gamma+.eta*gamma(1+(1/.beta))}
