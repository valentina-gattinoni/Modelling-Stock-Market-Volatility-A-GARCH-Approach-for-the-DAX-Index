
rm(list = ls())                                                                         # Clear workspace
graphics.off()                                                                          # Clear plots
cat("\014")                                                                             # Clear command window                                                                          # Setting the seed
# setwd("/Users/macbook/Desktop/ingegneria matematica/MAGISTRALE/SECONDO ANNO /Secondo semestre/Econometrics/Final_Project/Code")       # Set directory

# install.packages("nortest") # To use Anderson-Darling test
# install.packages("tseries") # To use Augmented Dickey-Fuller test
# install.packages("rugarch") # Used in modelling
# install.packages("fGarch") # Used in modelling
# install.packages("FinTS") # Used in ARCH test
# install.packages("dynlm") # To add lags in the model
# install.packages("vars") # To use VAR
# install.packages("nlWaldTest") # To use non-linear Wald test
# install.packages("lmtest") # Used in BP test
# install.packages("broom")
# install.packages("car")
# install.packages("sandwich")
# install.packages("knitr")
# install.packages("forecast")
# install.packages("tsbox")
# install.packages("stats")
# install.packages("zoo")
# install.packages("vrtest")
# install.packages("strucchange")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("changepoint")
# install.packages("stats")
# install.packages("moments")

library(readr)                                                                          # To read CSV files
library(moments)                                                                        # To use statistical functions
library(nortest)                                                                        # To use Anderson-Darling test
library(ggplot2)                                                                        # To use ggplot
library(tseries)                                                                        # To use Augmented Dickey-Fuller test
library(rugarch)                                                                        # Used in modelling
library(fGarch)                                                                         # Used in modelling
library(FinTS)                                                                          # Used in ARCH test
library(dynlm)                                                                          # To add lags in the model
library(vars)                                                                           # To use VAR
library(nlWaldTest)                                                                     # To use non-linear Wald test
library(lmtest)                                                                         # Used in BP test
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(FinTS)
library(strucchange)
library(tidyverse)
library(lubridate)
library(changepoint)                                                                    # Used for structural breaks detection
library(stats)

# RUNNING TIME ~ 50 seconds

##############################
### Analysis last 15 years ###
##############################

data   <- read_csv("DAX.csv")                                                           # Import the CSV file (Daily data)
data   <- data[data[,2]!="null",]                                                       # Remove null rows
Dates  <- as.Date(data$Date);                                                           # Saving dates
Prices <- as.numeric(data$`Adj Close`);                                                 # Saving prices

N <- length(Prices);                                                                    # Number of observations
X <- log(Prices[2:N]/Prices[1:N-1])                                                     # Log-returns computation

# PLOTS
plot(Dates, Prices, type = "l", xlab = "Date", ylab = "Price")                          # Prices plot
plot(Dates[2:N], X, type = "l", xlab = "Date", ylab = "Log-Returns")                    # Log-returns plot
# It is not stationary because it has non-constant variance 
# See Bartlett test below

X_test     <- X[Dates[2:length(Dates)]>="2022-12-01"]                                   # Consider the test dataset as the last month of 2022
Dates_test <- Dates[Dates>="2022-12-01"]                                                # Consider the test dataset as the last month of 2022

X     <- X[Dates[2:length(Dates)]<"2022-12-01"]                                         # Consider log-returns before 01-12-2022
Dates <- Dates[Dates<"2022-12-01"]                                                      # Consider dates before 01-12-2022
Dates <- Dates[2:length(Dates)]                                                         # Ignore first date ( length(X) = length(Prices)-1 )

# Descriptive statistics
Summary  <- summary(X);                                                                 # Summary of the data
Std_Dev  <- sd(X);                                                                      # Standard deviation
Skewness <- skewness(X);                                                                # Skewness (Gaussian = 0)
Kurtosis <- kurtosis(X);                                                                # Kurtosis (Gaussian = 3)

# Gaussianity tests
shapiro_test <- shapiro.test(X)                                                         # Shapiro-Wilk test
ad_test      <- ad.test(X)                                                              # Anderson-Darling test
ks_test      <- ks.test(X, "pnorm", mean = mean(X), sd = sd(X))                         # Kolmogorov-Smirnov test
jb_test      <- jarque.bera.test(X)                                                     # Jarque-Bera test

# Display
cat("Shapiro-Wilk test:",       shapiro_test$p.value, "\n")                             # p-value of Shapiro-Wilk test
cat("Anderson-Darling test:",   ad_test$p.value,      "\n")                             # p-value of Anderson-Darling test
cat("Kolmogorov-Smirnov test:", ks_test$p.value,      "\n")                             # p-value of Kolmogorov-Smirnov test
cat("Jarque Berà test:",        jb_test$p.value,      "\n")                             # p-value of Jarque-Bera test

# QQ-plot for x
qqnorm(X)                                                                               # QQ-plot of log-returns
qqline(X)                                                                               # QQ-line which represents gaussianity

font_name <- "Times"
font_size <- 20

# Plot versus a gaussian with same mean and standard deviation
df <- data.frame(x = X)                                                                 # Create a data frame for the density plot
ggplot(df, aes(x = x)) +                                                                # Create a plot object
  geom_density(fill = "blue", alpha = 0.2) +                                            # Plot the density of X
  stat_function(fun = dnorm, args = list(mean = mean(X), 
                                         sd = sd(X)), color = "red") +                  # Theoretical normal distribution
  labs(title = "Log-Return Distrubution", x = "", y = "", 
       color = "Distribution") +                                                        # Add a legend
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical"))    +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors



# Stationarity (unit root perspective)
adf_test <- adf.test(X)                                                                 # Augmented Dickey-Fuller test
cat("Augmented Dickey-Fuller test:", adf_test$p.value,     "\n")                        # Displaying results
# We reject the null hypothesis => Stationary (from a unit root prospective)

# Non-null average test
zm_test <- t.test(X, mu = 0);                                                           # t-test for zero mean
cat("Zero mean test:",zm_test$p.value,"\n")                                             # p-value = 0.4942 => Zero mean


# Structural breaks detection
# https://kevin-kotze.gitlab.io/tsm/ts-2-tut/
v_pelt <- cpt.var(X, method = "PELT")      

# Breaks detection using PELT method                          
plot(Dates, X, type = "l", xlab = "Date", ylab = "Log-Returns")                         # Log-returns plot
plot(v_pelt, type = "l", cpt.col = "red", cpt.width = 2, ylab = "Log-Returns")          # Structural breaks plot


# Bartlett test (Heteroskedaticity)
# https://www.statology.org/bartletts-test-in-r/
Indexes   <- c(0,cpts(v_pelt),length(X))                                                # Expand the indexes for computational purposes
Groups    <- c()                                                                        # Initialization of the empty vector of groups
for (idx in 2:length(Indexes)) {                                                        # Looping the index of the last element of a group
  Groups    <- c( Groups, rep( letters[idx-1], 
                               times = Indexes[idx]-Indexes[idx-1]) )                   # Add the proper number of identical letters to identify the group
}
df <- data.frame(groups = Groups, log_pr = X)                                           # Dataframe composition
bartlett.test(log_pr ~ groups, data = df)                                               # Bartlett test
# We reject the constant variance hypothesis

# ACF and PACF
acf.X  <- acf( X, main = "ACF ", lag.max = 30)                                          # ACF plot of the log-returns
pacf.X <- pacf(X, main = "PACF", lag.max = 30)                                          # Partial ACF plot of the log-returns

# ACF and PACF
acf.X  <- acf( X^2, main = "ACF", lag.max = 30)                                         # ACF plot of the squared log-returns
pacf.X <- pacf(X^2, main = "PACF", lag.max = 30)                                        # Partial ACF plot of the squared log-returns

# ARCH effect test
archTest <- ArchTest(X, lags = 1, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Reject H0 --> There is ARCH effect

archTest <- ArchTest(X, lags = 2, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Reject H0 --> There is ARCH effect

archTest <- ArchTest(X, lags = 3, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Reject H0 --> There is ARCH effect


font_name <- "Times"
font_size <- 16


#############################################
### Model fitting & residuals diagnostics ###
#############################################

### GARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # GARCH(1,1) specifications
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),   
                        distribution.model = 'norm');
sgarch.fit = ugarchfit(data = as.array(X), spec = spec);                                # GARCH(1,1) fitting 
sgarch.fit

# Diagnostic 
resid <- residuals(sgarch.fit)                                                          # Extract the residuals
std_resid <-  resid / sigma(sgarch.fit)                                                 # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
                       theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
                             axis.title = element_text(family = font_name, size = font_size),
                             panel.background = element_rect(fill = "white"),
                             panel.grid.major = element_line(color = "lightgrey", size = 0.20),
                             panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
                             axis.line = element_line(color = "lightgrey"))                        # Set colors
  

# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 


### GARCH-M(1,1) ### (GARCH in Mean, to find the presence of a risk premium)
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0),archm=TRUE,archpow=1),     # GARCH-M(1,1) specifications
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
sgarchM.fit = ugarchfit(data = as.array(X), spec = spec)                                # GARCH-M(1,1) fitting
sgarchM.fit

# Diagnostic 
resid     <- residuals(sgarchM.fit)                                                     # Extract the residuals
std_resid <-  resid / sigma(sgarchM.fit)                                                # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH-M(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
                       theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
                             axis.title = element_text(family = font_name, size = font_size),
                             panel.background = element_rect(fill = "white"),
                             panel.grid.major = element_line(color = "lightgrey", size = 0.20),
                             panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
                             axis.line = element_line(color = "lightgrey"))                        # Set colors
  

# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

### eGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # eGARCH(1,1) specifications
                        variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
egarch.fit = ugarchfit(data = as.array(X), spec = spec)                                 # eGARCH(1,1) fitting
egarch.fit

# Diagnostic 
resid     <- residuals(egarch.fit)                                                       # Extract the residuals
std_resid <-  resid / sigma(egarch.fit)                                                  # Standardized 
mean(std_resid);                                                                         # Mean
var(std_resid);                                                                          # Variance 
skewness(std_resid);                                                                     # Skewness
kurtosis(std_resid);                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "eGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical"))  +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors


# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

### gjrGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # gjrGARCH(1,1) specifications
                        variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
gjrgarch.fit = ugarchfit(data = as.array(X), spec = spec)                               # gjrGARCH(1,1) fitting
gjrgarch.fit

# Diagnostic 
resid <- residuals(gjrgarch.fit)                                                        # Extract the residuals
std_resid <-  resid / sigma(gjrgarch.fit)                                               # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "gjrGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical")) +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors


# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 


###########################
### 1-month forecasting ###
###########################

X_sGARCH   <- vector(mode = "numeric", length = length(X_test));                        # Log-returns initialization 
X_sGARCHM  <- vector(mode = "numeric", length = length(X_test));                        # Log-returns initialization 
X_eGARCH   <- vector(mode = "numeric", length = length(X_test));                        # Log-returns initialization 
X_gjrGARCH <- vector(mode = "numeric", length = length(X_test));                        # Log-returns initialization 

Volatility_sGARCH   <- vector(mode = "numeric", length = length(X_test));               # Volatility initialization 
Volatility_sGARCHM  <- vector(mode = "numeric", length = length(X_test));               # Volatility initialization 
Volatility_eGARCH   <- vector(mode = "numeric", length = length(X_test));               # Volatility initialization 
Volatility_gjrGARCH <- vector(mode = "numeric", length = length(X_test));               # Volatility initialization 

for (day in 1:length(X_test)) {
  
  spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),       
                          variance.model = list( model = 'sGARCH', 
                                                 garchOrder = c(1,1) ),
                          distribution.model = 'norm');                                 # Setting GARCH specification
  sgarch.fit = ugarchfit(data = as.array(X), spec = spec);                              # GARCH fitting
  
  spec       = ugarchspec(mean.model = list( armaOrder = c(0,0), 
                                             include.mean=TRUE, 
                                             archm=TRUE, archpow=1 ), 
                          variance.model = list( model = 'sGARCH', 
                                                 garchOrder = c(1,1) ),
                          distribution.model = 'norm');                                 # Setting GARCH-M specification
  sgarchM.fit = ugarchfit(data = as.array(X), spec = spec);                             # GARCH in Mean fitting
  # This is the GARCH in Mean, used to find a risk premium
  
  spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list( model = 'eGARCH', 
                                                 garchOrder = c(1,1) ),
                          distribution.model = 'norm');                                 # Setting eGARCH specification
  egarch.fit = ugarchfit(data = as.array(X), spec = spec);                              # eGARCH fitting
  
  spec         = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                            variance.model = list( model = 'gjrGARCH', 
                                                   garchOrder = c(1,1) ),
                            distribution.model = 'norm');                               # Setting gjrGARCH specification
  gjrgarch.fit = ugarchfit(data = as.array(X), spec = spec);                            # gjrGARCH fitting
  
  
  if (day < length(X_test))
    X <- c(X[(day+1):length(X)],X_test[1:day]);                                           # Update historical data
  
  Forecast_sGARCH   <- ugarchforecast(fitORspec = sgarch.fit,   n.ahead = 1);           # Forecasting with GARCH model (ugarchfit)
  Forecast_sGARCHM  <- ugarchforecast(fitORspec = sgarchM.fit,  n.ahead = 1);           # Forecasting with GARCH-M model (ugarchfit)
  Forecast_eGARCH   <- ugarchforecast(fitORspec = egarch.fit,   n.ahead = 1);           # Forecasting with eGARCH model
  Forecast_gjrGARCH <- ugarchforecast(fitORspec = gjrgarch.fit, n.ahead = 1);           # Forecasting with gjrGARCH model
  
  X_sGARCH[day]   <- Forecast_sGARCH@forecast$seriesFor;                                # Saving the expected log-return with GARCH model
  X_sGARCHM[day]  <- Forecast_sGARCHM@forecast$seriesFor;                               # Saving the expected log-return with GARCH-M model
  X_eGARCH[day]   <- Forecast_eGARCH@forecast$seriesFor;                                # Saving the expected log-return with eGARCH model
  X_gjrGARCH[day] <- Forecast_gjrGARCH@forecast$seriesFor;                              # Saving the expected log-return with gjrGARCH model
  
  Volatility_sGARCH[day]   <- Forecast_sGARCH@forecast$sigmaFor;                        # Saving the forecasted volatility with GARCH model
  Volatility_sGARCHM[day]  <- Forecast_sGARCHM@forecast$sigmaFor;                       # Saving the forecasted volatility with GARCH-M model
  Volatility_eGARCH[day]   <- Forecast_eGARCH@forecast$sigmaFor;                        # Saving the forecasted volatility with eGARCH model
  Volatility_gjrGARCH[day] <- Forecast_gjrGARCH@forecast$sigmaFor;                      # Saving the forecasted volatility with gjrGARCH model

}



Error_GARCH    <- sum(abs(abs(X_test) - Volatility_sGARCH))                             # GARCH forecast absolute error
Error_GARCHM   <- sum(abs(abs(X_test) - Volatility_sGARCHM))                            # GARCH in Mean forecast absolute error
Error_eGARCH   <- sum(abs(abs(X_test) - Volatility_eGARCH))                             # Exponential GARCH forecast absolute error
Error_gjrGARCH <- sum(abs(abs(X_test) - Volatility_gjrGARCH))                           # GJR-GARCH forecast absolute error


########################
### Plotting results ###
########################

font_name <- "Times"
font_size <- 20

df <- data.frame(Dates_test, X_test, X_sGARCH, Volatility_sGARCH);                      # Plotting GARCH model forecasts
ggplot(df, aes(x = Dates_test)) +
  geom_segment(aes(x = Dates_test, xend = Dates_test, y = X_test, yend = X_test), colour = "red") +
  geom_ribbon(aes(ymin = X_sGARCH - 1*Volatility_sGARCH, ymax = X_sGARCH + 1*Volatility_sGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_sGARCH - 2*Volatility_sGARCH, ymax = X_sGARCH + 2*Volatility_sGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_sGARCH - 3*Volatility_sGARCH, ymax = X_sGARCH + 3*Volatility_sGARCH), fill = "red", alpha = 0.2) +
  geom_line(aes(y = X_test), colour = "black") +
  labs(title = "GARCH", x = "Dates", y = "Daily Log-returns") +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
        axis.line = element_line(color = "lightgrey"))

df <- data.frame(Dates_test, X_test, X_sGARCHM, Volatility_sGARCHM);                    # Plotting GARCH-M model forecasts
ggplot(df, aes(x = Dates_test)) +
  geom_segment(aes(x = Dates_test, xend = Dates_test, y = X_test, yend = X_test), colour = "red") +
  geom_ribbon(aes(ymin = X_sGARCHM - 1*Volatility_sGARCHM, ymax = X_sGARCHM + 1*Volatility_sGARCHM), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_sGARCHM - 2*Volatility_sGARCHM, ymax = X_sGARCHM + 2*Volatility_sGARCHM), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_sGARCHM - 3*Volatility_sGARCHM, ymax = X_sGARCHM + 3*Volatility_sGARCHM), fill = "red", alpha = 0.2) +
  geom_line(aes(y = X_test), colour = "black") +
  labs(title = "GARCH in Mean", x = "Dates", y = "Daily Log-returns") +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
        axis.line = element_line(color = "lightgrey"))

df <- data.frame(Dates_test, X_test, X_eGARCH, Volatility_eGARCH);                      # Plotting eGARCH model forecasts
ggplot(df, aes(x = Dates_test)) +
  geom_segment(aes(x = Dates_test, xend = Dates_test, y = X_test, yend = X_test), colour = "red") +
  geom_ribbon(aes(ymin = X_eGARCH - 1*Volatility_eGARCH, ymax = X_eGARCH + 1*Volatility_eGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_eGARCH - 2*Volatility_eGARCH, ymax = X_eGARCH + 2*Volatility_eGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_eGARCH - 3*Volatility_eGARCH, ymax = X_eGARCH + 3*Volatility_eGARCH), fill = "red", alpha = 0.2) +
  geom_line(aes(y = X_test), colour = "black") +
  labs(title = "Exponential GARCH", x = "Dates", y = "Daily Log-returns") +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
        axis.line = element_line(color = "lightgrey"))

df <- data.frame(Dates_test, X_test, X_gjrGARCH, Volatility_gjrGARCH);                  # Plotting gjrGARCH model forecasts
ggplot(df, aes(x = Dates_test)) +
  geom_segment(aes(x = Dates_test, xend = Dates_test, y = X_test, yend = X_test), colour = "red") +
  geom_ribbon(aes(ymin = X_gjrGARCH - 1*Volatility_gjrGARCH, ymax = X_gjrGARCH + 1*Volatility_gjrGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_gjrGARCH - 2*Volatility_gjrGARCH, ymax = X_gjrGARCH + 2*Volatility_gjrGARCH), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = X_gjrGARCH - 3*Volatility_gjrGARCH, ymax = X_gjrGARCH + 3*Volatility_gjrGARCH), fill = "red", alpha = 0.2) +
  geom_line(aes(y = X_test), colour = "black") +
  labs(title = "GJR-GARCH", x = "Dates", y = "Daily Log-returns") +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.25),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
        axis.line = element_line(color = "lightgrey"))
