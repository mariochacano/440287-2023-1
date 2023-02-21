#Load Packages
library(tidyverse)

# Load Weibull Functions
source(paste(getwd(),"/MaintenanceFunctions.R", sep=""))
source(paste(getwd(),"/WeibullFunctions.R", sep=""))
source(paste(getwd(),"/Fit_Weibull_Parameters.R", sep=""))

# Example 1
df2 <- data.frame(tbfs=c(110,330,120,220,220,220))
print(df2$tbfs %>% fit_Wp_MLE())
WMTBF <- df2$tbfs %>% fit_Wp_MLE() %>% WMean()
