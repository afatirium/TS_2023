

## Loading packages
#install.packages("rvest")
#install.packages("lubridate")
#install.packages("BatchGetSymbols")
#install.packages("tictoc")

library(dplyr)
library(tidyverse)
library(xts)
library(zoo)
library(fBasics)  # basicStats()
library(tseries)  # jarque.bera.test()
library(FinTS)    # ArchTest()
library(car)      # durbinWatsonTest()
library(rmgarch)
library(fGarch) # e.g. garchFit()
library(quantmod)
library(rvest)
library(lubridate)
library(BatchGetSymbols)
library(graphics)
library(tictoc)

setwd("C:\\Users\\Afat\\Documents\\GitHub\\TS_2023")
getwd()

# Let's load additional functions which help  easily compare ICs for 
# GARCH models:

source("functions/compare_ICs_GARCH.R")
source("functions/compare_ICs_ugarchfit.R")

# Set Alpha Vantage API key
api_key <- "1HPP3767XJS4QL7L"

# Prepraing the data
start.date <- "2000-01-01"
stop.date  <- "2023-06-07"


# Fetch data for the S&P 500
SP500 <- getSymbols("^GSPC", src = "yahoo", 
                    from = start.date, to = stop.date, 
                    auto.assign = FALSE)

# Fetch data for DAX from Yahoo Finance
DAX <- getSymbols("^GDAXI", 
                  from = start.date, to = stop.date, 
                  auto.assign = FALSE)

# Read the CSV files for Wig20, kospi200, nikkei225
wig20 <- read.csv("wig20_d.csv",
                     header = TRUE,
                     sep = ",",
                     dec = ".",
                     stringsAsFactors = F)

kospi200 <- read.csv("^kospi_d.csv",
                     header = TRUE,
                     sep = ",",
                     dec = ".",
                     stringsAsFactors = F)
nikkei225 <- read.csv("^nkx_d.csv",
                      header = TRUE,
                      sep = ",",
                      dec = ".",
                      stringsAsFactors = F)

## Print the data frames for checking
head(wig20)
head(kospi200)
head(nikkei225)
head(SP500)
head(DAX)

str(wig20)
str(SP500)
str(DAX)

# Data Preparation

## change date as row index
wig20 <- xts(wig20[, -1], 
             order.by = as.Date(wig20$Date))
kospi200 <- xts(kospi200[, -1],
                order.by = as.Date(kospi200$Date))
nikkei225 <- xts(nikkei225[, -1], 
                 order.by = as.Date(nikkei225$Date))

head(wig20)

## select only close values for all indexes
SP500 <- SP500[, 4]
names(SP500) <- "SP500"

wig20 <- wig20[, 4]
names(wig20) <- c("WIG20")

kospi200 <- kospi200[, 4]
names(kospi200) <- c("KOSPI200")

nikkei225 <- nikkei225[, 4]
names(nikkei225) <- c("NKK225")

DAX <- DAX[, 4]
names(DAX) <- c("DAX")

##Create portfolio 

portfolio <- SP500
portfolio$DAX <-DAX$DAX
portfolio$WIG20 <-wig20$WIG20
portfolio$KOSPI200 <- kospi200$KOSPI200
portfolio$NKK225<- nikkei225$NKK225

head(portfolio)

##Handling missing values
any(is.na(portfolio))

## I have missing values, because my data start is different for different variables
## that's why limit start time from 2018

portfolio <- portfolio["2018/", ]
tail (portfolio)
head(portfolio)

## all index start from 2018 and I have missing values still...
## I assume that these missing values exist because of non-work days, that's why fill missing values with the previous available value.

portfolio <- na.locf(portfolio)
##Check
any(is.na(portfolio))

#I have missing values, Again..

dweek_ <- wday(portfolio)
table(dweek_)

#According to the result of wday function, there isn't weekend in dataset

##Still, I have NA values at the begining of the dataset, so I decided to omit these
portfolio <- na.omit(portfolio)
##Check
any(is.na(portfolio))

head(portfolio)
tail(portfolio)

#
##All Missing values are cleaned. Let's continue ^.^

#Adding log return of index seperatedly and applying weight

portfolio$SP500_r <- 0.2 * diff.xts(log(portfolio$SP500))

portfolio$DAX_r <- 0.2 * diff.xts(log(portfolio$DAX))

portfolio$WIG20_r <- 0.2 * diff.xts(log(portfolio$WIG20))

portfolio$KOSPI_r <- 0.2 * diff.xts(log(portfolio$KOSPI200))

portfolio$NKK225_r <- 0.2 * diff.xts(log(portfolio$NKK225))


head(portfolio)

#Creating of all indexes log returns all together
portfolio$PORTFOLIO_r <- rowSums(portfolio[ , c("SP500_r", "DAX_r", "KOSPI_r", "WIG20_r", "NKK225_r")])

head(portfolio)
## I have missing values in the first row because of the diff function, let's omit the first row
portfolio <- na.omit(portfolio)
head(portfolio)
any(is.na(portfolio))

#Volatility Modelling - - stylized facts

## Plot of portfolio returns

plot(portfolio$PORTFOLIO_r,
     col = "blue",
     main = "Portfolio returns")


# This in turn indicates some autoregressive relations among **squared** returns which can be used to build a (G)ARCH model.

# Checking skewness and kurtosis
basicStats(portfolio$PORTFOLIO_r)

# Plotting Histogram of Log-returns
tibble(r = as.numeric(portfolio$PORTFOLIO_r)) %>%
  ggplot(aes(r)) +
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "light green") +
  stat_function(fun = dnorm,colour="blue",
                args = list(mean = mean(portfolio$PORTFOLIO_r), 
                            sd = sd(portfolio$PORTFOLIO_r))) +
  theme_dark() + 
  labs(
    title = paste("Density of the Portfolio log-returns"), 
    y = "", x = "",
    caption = "source: own calculations"
  )

# Jarque-Bera statistic:
jarque.bera.test(portfolio$PORTFOLIO_r)

##Conclusion for this part:
##1. Skewness is negative: -0.825809
##2. Kurtosis is 13.232398, it means strong excess kurtosis
##3. The plot is leptokurtosis,  "heavy tails" and higher peak in mean
##4. According to the JB test, we reject the null hypothesis. The null hypothesis is a joint hypothesis of the skewness being zero and the excess kurtosis being zero.

#ARCH Test

## Let's plot the ACF function of log-returns:

acf(portfolio$PORTFOLIO_r, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(-0.1,0.1), # we rescale the vertical axis
    col = "darkgreen", 
    lwd = 7, 
    main = "ACF of log-returns of Portfolio")

# As seen, values of the ACF function indicate some autoregressive/MA relations among returns which can be  used to build an ARIMA model.

# Now, let's also see the ACF values for for the **squared** log-returns:

acf(portfolio$PORTFOLIO_r ^ 2, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(0,0.5), # we rescale the vertical axis
    col = "blue", 
    lwd = 7, 
    main = "ACF of SQUARED log-returns of Portfolio")

# Let's verify existence of ARCH effects among log-returns. 
# The ARCH test is based on the autocorrelation of **squared** returns.

ArchTest(portfolio$PORTFOLIO_r,  # here we use a vector of returns as input
         lags = 5) # and maximum order of ARCH effect

# The null hypothesis about lack of ARCH effects strongly rejected!
# Note: I check this test for different lags, for example, 5,10,15. All the lags indicate same result.

durbinWatsonTest(lm(portfolio$PORTFOLIO_r^2 ~ 1),
                 max.lag = 5)

#The null hypothesis for durbinWatsonTest about no autocorrelation in squared log returns is rejected.


# Modelling

# Now, let's try to find the most attractive GARCH(q, p) model. 

## ARCH vs. GARCH models

### ARCH(1)

k.arch1 <- garchFit(formula = ~ garch(1, 0), # GARCH(q, p)
                    # formula describing the mean and variance equation 
                    # of the ARMA-GARCH - we assume that returns
                    # follow an ARCH(1) process
                    data = portfolio$PORTFOLIO_r,
                    # conditional distribution of errors
                    cond.dist = "norm", 
                    # if we don't want to see the history of iterations
                    trace = FALSE) 

# Summary of results and some diagnostic tests:

summary(k.arch1)
plot(k.arch1, which = 10)
plot(k.arch1, which = 11)
plot(k.arch1, which = 2)

# The parameters are denoted by the following symbols:

# * mu - constant term in the mean equation
# * omega - constant term in the variance equation
# * alpha1 - arch1 parameter in the variance equation

# If the intercept in the mean equation of the model above is not significant at 5% level, we can drop it.

### ARCH(1) - without mu
k.arch1a <- garchFit(~garch(1, 0), 
                     data = portfolio$PORTFOLIO_r,
                     # remove intercept term in the mean equation
                     include.mean = FALSE,
                     # conditional distribution of errors
                     cond.dist = "norm",
                     trace = FALSE)
summary(k.arch1a)
plot(k.arch1a, which = 10)
plot(k.arch1a, which = 11)
plot(k.arch1a, which = 2)

#Significant lags are observed for square residuals for lags 2-12, 14-15. 
#And the Ljung-Box test for R^2 shows there is autocorrelation at 5% level. 
#Lets try if ARCH(5).


### ARCH(5)

k.arch5 <- garchFit(~garch(5, 0),
                    data = portfolio$PORTFOLIO_r,
                    include.mean = TRUE,
                    cond.dist = "norm", 
                    trace = FALSE) 
summary(k.arch5)

plot(k.arch5, which = 10)
plot(k.arch5, which = 11)

# All parameters are significant. The Ljung-box tests for R and R^2 show there is still autocorrelation.
# The lags 5,6,9,in ACF for squared residuals seem to be still significant.

### ARCH(10)

k.arch10 <- garchFit(~garch(10, 0),
                    data = portfolio$PORTFOLIO_r,
                    include.mean = TRUE,
                    cond.dist = "norm", 
                    trace = FALSE) 
summary(k.arch10)

plot(k.arch10, which = 10)
plot(k.arch10, which = 11)

# - Again most of parameters are significant at 5% level - excluding 4,5,7,10. 
# - The Ljung-box test for R^2 shows there is no more autocorrelation between the current and past standardized squared residuals.
# - It looks like no further extension of the **conditional variance** equation is needed.

### GARCH (1,1)

k.garch11 <- garchFit(~garch(1, 1),
                      data = portfolio$PORTFOLIO_r,
                      include.mean = TRUE,
                      cond.dist = "norm", 
                      trace = FALSE) 
summary(k.garch11)

plot(k.garch11, which = 10)
plot(k.garch11, which = 11)

# - All parameters are significant. 
# - The Ljung-Box test for R^2 does not show autocorrelation at 5% level between the current and past standardized squared residuals (variance).
# - The **conditional variance** equation seems to be complete, however the ACF plot of residuals indicates that we may still improve the model by adding AR components in the **mean** equation.

# - mu is less significant than other parameters, let's try without mu: (just for fun)

### GARCH(1,1) - without mu

k.garch11a <- garchFit(~garch(1, 1),
                      data = portfolio$PORTFOLIO_r,
                      include.mean = FALSE,
                      cond.dist = "norm", 
                      trace = FALSE) 
summary(k.garch11a)

plot(k.garch11a, which = 10)
plot(k.garch11a, which = 11)

# - All parameters are significant.
# - The Ljung-Box test for R^2 does not show autocorrelation at 5% level between the current and past standardized squared residuals (variance).
# - ACF for R^2 looks much better.

### AR(5)-GARCH(1,1)
k.ar5garch11 <- garchFit(~arma(5, 0) + garch(1, 1),
                         data = portfolio$PORTFOLIO_r,
                         include.mean = TRUE,
                         cond.dist = "norm",
                         trace = FALSE)
summary(k.ar5garch11)
plot(k.ar5garch11, which = 10)
plot(k.ar5garch11, which = 11)

# - Except AR2,AR3,AR4,AR5, other parameters are significant.
# - The Ljung-box test for R^2 shows there is no more autocorrelation between the current and past standardized squared residuals. Similarly, there is no autocorrelation among returns after adding AR part now.
# - Both ACF figures show no significant lags.

# Since moast of AR parameters are insignificant, let's try simplier version:

### AR(1)-GARCH(1,1)
k.ar1garch11 <- garchFit(~arma(1, 0) + garch(1, 1),
                         data = portfolio$PORTFOLIO_r,
                         include.mean = TRUE,
                         cond.dist = "norm",
                         trace = FALSE)
summary(k.ar1garch11)
plot(k.ar1garch11, which = 10)
plot(k.ar1garch11, which = 11)

# - All parameters are significant.
# - The Ljung-box test for R^2 shows there is no more autocorrelation between the current and past standardized squared residuals. Similarly, there is no autocorrelation among returns after adding AR part now.
# - Both ACF figures show no significant lags.

### AR(1)-ARCH(10)
tic()
k.ar1arch10 <- garchFit(~arma(1, 0) + garch(10, 0),
                        data = portfolio$PORTFOLIO_r,
                        include.mean = TRUE,
                        cond.dist = "norm",
                        trace = FALSE)
toc()
summary(k.ar1arch10)
plot(k.ar1arch10, which = 10)
plot(k.ar1arch10, which = 11)

# - All parameters are significant.
# - The Ljung-box test for R^2 shows there is no more autocorrelation between the current and past standardized squared residuals. Similarly, there is no autocorrelation among returns after adding AR part now.
# - Both ACF figures show no significant lags

### Result: ARCH vs GARCH

# Let's compare the models using above mentioned fucntion:

compare_ICs_GARCH(c("k.arch1", 
                    "k.arch1a", 
                    "k.arch5", 
                    "k.arch10", 
                    "k.garch11", 
                    "k.garch11a",
                    "k.ar5garch11",
                    "k.ar1garch11", 
                    "k.ar1arch10"))

# The comparison shows that 8th - **AR(1)-GARCH(1,1)** is the best model among others. 
# I will use this model for other GARCH extensions. 
# Let's turn the page for new chapter! ^.^


# GARCH extensions

## EGARCH model
# Let's define a model specification:

### AR(1)-EGARCH(1,1)

spec <- ugarchspec(# variance equation
    variance.model = list(model = "eGARCH", 
                          garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0), 
                      include.mean = TRUE), 
    # assumed distribution of errors
    distribution.model = "norm")

# and estimate the model:

k.ar1egarch11 <- ugarchfit(spec = spec, 
                           data = portfolio$PORTFOLIO_r)

#Check the results:

k.ar1egarch11

plot(k.ar1egarch11, which = 3)
plot(k.ar1egarch11, which = 11)
plot(k.ar1egarch11, which = 12)

### AR(1)-EGARCH(1,1) without mu

speca <- ugarchspec(# variance equation
  variance.model = list(model = "eGARCH", 
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), 
                    include.mean = FALSE), 
  # assumed distribution of errors
  distribution.model = "norm")

# and estimate the model:

k.ar1egarch11a <- ugarchfit(spec = spec, 
                           data = portfolio$PORTFOLIO_r)

#Result:
k.ar1egarch11a

plot(k.ar1egarch11a, which = 3)
plot(k.ar1egarch11a, which = 11)
plot(k.ar1egarch11a, which = 12)

## The GARCH-st model 
### AR(1) - GARCH-st(1,1)
# Let's first define a model specification:

spec <- ugarchspec(# variance equation
  variance.model = list(model = "sGARCH", 
                        garchOrder = c(1, 1)),
  # mean equation
  mean.model = list(armaOrder = c(1, 0), 
                    include.mean = TRUE), 
  # assumed distribution of errors
  distribution.model = "std") # std = t-Student


# Then, estimate the model:

k.ar1garcht11 <- ugarchfit(spec = spec, 
                           data = portfolio$PORTFOLIO_r)
# Result:
k.ar1garcht11

plot(k.ar1garcht11, which = 3)
plot(k.ar1garcht11, which = 11)
plot(k.ar1garcht11, which = 12)

## The GARCH-in-Mean model

### AR(1)-GARCH-m(1,1)
# Let's first define a model specification:

spec <- ugarchspec(# variance equation
  variance.model = list(model = "sGARCH", 
                        # sGARCH = standard GARCH
                        garchOrder = c(1, 1)),
  # mean equation - lets turn on the intercept term
  mean.model = list(armaOrder = c(1, 0), 
                    include.mean = TRUE,
                    # we add an element to the mean equation,
                    # which can be either stdev (archpow 1)
                    # or var (archpow=2)
                    archm = TRUE, archpow = 1), 
  # assumed distribution of errors
  distribution.model = "norm")

# Then, we can estimate the model:

k.ar1garchm11 <- ugarchfit(spec = spec, 
                           data = portfolio$PORTFOLIO_r)

# Let's examine the results:

k.ar1garchm11

plot(k.ar1garchm11, which = 3)
plot(k.ar1garchm11, which = 11)
plot(k.ar1garchm11, which = 12)

### AR(1)-GARCH-m(1,1) without mu

spec <- ugarchspec(# variance equation
  variance.model = list(model = "sGARCH", 
                        # sGARCH = standard GARCH
                        garchOrder = c(1, 1)),
  # mean equation - lets turn on the intercept term
  mean.model = list(armaOrder = c(1, 0), 
                    include.mean = FALSE,
                    # we add an element to the mean equation,
                    # which can be either stdev (archpow 1)
                    # or var (archpow=2)
                    archm = TRUE, archpow = 1), 
  # assumed distribution of errors
  distribution.model = "norm")

# Model estimation:

k.ar1garchm11a <- ugarchfit(spec = spec, 
                           data = portfolio$PORTFOLIO_r)

# Model summary:

k.ar1garchm11a

plot(k.ar1garchm11a, which = 3)
plot(k.ar1garchm11a, which = 11)
plot(k.ar1garchm11a, which = 12)


## The EGARCH in mean-t model

### AR(1)-EGARCH-m-st(1,1)
# Let's first define the model specification:

spec <- ugarchspec(# variance equation
  variance.model = list(model = "eGARCH", 
                        garchOrder = c(1, 1)),
  # mean equation
  mean.model = list(armaOrder = c(1, 0), 
                    include.mean = TRUE,
                    archm = TRUE, archpow = 1), 
  # assumed distribution of errors
  distribution.model = "std") # std = t-Student

# Now, let's estimate the model:

k.ar1egarchmt11 <- ugarchfit(spec = spec, 
                             data = portfolio$PORTFOLIO_r)

# The model summary:

k.ar1egarchmt11

plot(k.ar1egarchmt11, which = 3)
plot(k.ar1egarchmt11, which = 11)
plot(k.ar1egarchmt11, which = 12)


# Comparison

compare_ICs_ugarchfit(c("k.ar1egarch11",
                        "k.ar1egarch11a",
                        "k.ar1garcht11",
                        "k.ar1garchm11", 
                        "k.ar1garchm11a",
                        "k.ar1egarchmt11"))

# According to the comparison, last model, AR(1)-EGARCH-m-st(1,1) is the best model
# Let's turn page to the new chapter ^.^ 

# VaR (Value at risk)
