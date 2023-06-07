

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

# Set Alpha Vantage API key)
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

# Print the data frames
head(wig20)
head(kospi200)
head(nikkei225)
head(SP500)
head(DAX)

## change date as row index
wig20 <- xts(wig20[, -1], # data columns (without the first column with date)
                order.by = as.Date(wig20$Date))
kospi200 <- xts(kospi200[, -1], # data columns (without the first column with date)
                order.by = as.Date(kospi200$Date))
nikkei225 <- xts(nikkei225[, -1], # data columns (without the first column with date)
                 order.by = as.Date(nikkei225$Date))
