## All Weibull Functions are described by a list called "wp", this list contains the weibull parameters shape, scale, location.

# Weibull Reliability Function
WR <- function(t,wp){exp(-(((t-wp$location)/wp$scale)^(wp$shape)))}

inv_WR <- function(r,wp){(wp$scale*((-log(r))^(1/wp$shape)))+wp$location}

# Weibull Cumulative Distribution Function
Wcdf <- function(t,wp){1-(exp(-(((t-wp$location)/wp$scale)^(wp$shape))))}

# Weibull Hazard Function
WH <- function(t,wp){(wp$shape/wp$scale)*(((t-wp$location)/wp$scale)^(wp$shape-1))}

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
