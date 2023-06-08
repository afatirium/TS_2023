

## Loading packages
#install.packages("rvest")
#install.packages("lubridate")
#install.packages("BatchGetSymbols")

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
library(ccgarch)
library(ccgarch2) # not available for R 4.2.0!
library(quantmod)
library(rvest)
library(lubridate)
library(BatchGetSymbols)

setwd("C:\\Users\\Afat\\Documents\\GitHub\\TS_2023")
getwd()

# Set Alpha Vantage API key
api_key <- "1HPP3767XJS4QL7L"

# Prepraing the data
start.date <- "2000-01-01"
stop.date  <- "2023-01-01"


# Fetch data for the S&P 500
SP500 <- getSymbols("^GSPC", src = "yahoo", 
                    from = start.date, to = stop.date, 
                    auto.assign = FALSE)

# Fetch data for DAX from Yahoo Finance
DAX <- getSymbols("^GDAXI", 
                  from = start.date, to = stop.date, 
                  auto.assign = FALSE)

# Fill missing values in DAX with the previous available value
DAX <- na.locf(DAX)


# Read the CSV files for Wig20, kospi200, nikkei225
wig20 <- read.csv("wig20_d.csv",
                     header = TRUE,
                     sep = ",",
                     dec = ".",
                     stringsAsFactors = F)
wig20 <- na.locf(wig20)


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

## Print the data frames
head(wig20)
head(kospi200)
head(nikkei225)
head(SP500)
head(DAX)

## change date as row index
wig20 <- xts(wig20[, -1], 
                order.by = as.Date(wig20$Date))
kospi200 <- xts(kospi200[, -1],
                order.by = as.Date(kospi200$Date))
nikkei225 <- xts(nikkei225[, -1], 
                 order.by = as.Date(nikkei225$Date))

str(wig20)
str(SP500)
str(DAX)


# It is pratice part with SP500
# I will include only the close price 


SP500 <- SP500[, 4]
names(SP500) <- "SP500"

# add log-returns to the data

SP500$SP500_r <- diff.xts(log(SP500$SP500))

# Finally, limit the data to days since the beginning of 2008:

SP500 <- SP500["2008/",] 

# Now, let's plot the close price 

plot(SP500$SP500,
     col = "blue",
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "Daily close price of SP500")

# and it's log-returns

plot(SP500$r, 
     col = "red",
     major.ticks = "years", 
     grid.ticks.on = "years",
     main = "Log-returns of SP500")

#Plot the ACF function of log-returns:

acf(SP500$r, 
    lag.max = 36, 
    na.action = na.pass,
    ylim = c(-0.1,0.1), # we rescale the vertical axis
    col = "darkblue", 
    lwd = 7, 
    main = "ACF of log-returns of SP500")

# Data Preparation
## change date as row index
wig20 <- xts(wig20[, -1], 
             order.by = as.Date(wig20$Date))
kospi200 <- xts(kospi200[, -1],
                order.by = as.Date(kospi200$Date))
nikkei225 <- xts(nikkei225[, -1], 
                 order.by = as.Date(nikkei225$Date))

## select only close values for all indexes
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
portfolio$wig20 <-wig20$WIG20
portfolio$KOSPI200 <- kospi200$KOSPI200
portfolio$NKK225<- nikkei225$NKK225

head(portfolio)

##Handling missing values
any(is.na(portfolio))

## 1) I have missing values, because my data start from different date for different variables
## that's why limit start time from 2018

portfolio <- portfolio["2018", ]

## all index start from 2018 and I have missing values still...
## I assume that these missing values exist because of non-work days, that's why fill values wth previous ones.

portfolio <- na.locf(portfolio)
##Check
any(is.na(portfolio))

##Still, I have NA values at the begining of the dataset, so I decided to omit these two days
portfolio <- na.omit(portfolio)
##Check
any(is.na(portfolio))

#All Missing values are cleaned.




