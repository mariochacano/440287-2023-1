#Load Packages
library(tidyverse)

# Load Weibull Functions
source(paste(getwd(),"/MaintenanceFunctions.R", sep=""))
source(paste(getwd(),"/WeibullFunctions.R", sep=""))
source(paste(getwd(),"/Fit_Weibull_Parameters.R", sep=""))
# Example data
df2 <- data.frame(tbfs=c(110,330,120,220,220,220))

# Example 1
df2 %>% arrange(.[1]) %>% bernard_MR() %>% LSR() %>% fit_Wp_LR()
df2 %>% arrange(.[1]) %>% bernard_MR() %>% LSR() %>% fit_Wp_LR() %>% WMean()
  
# Example 2
df2$tbfs %>% fit_Wp_MLE()
df2$tbfs %>% fit_Wp_MLE() %>% WMean()
