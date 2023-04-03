
library(dynamac)          # For ARDL estimation
library(forecast)         # For ARIMA model estimation
library(tseries)          # for time series models and diagnostic checks
library(nlme)             # to estimate ARIMA models
library(pdfetch)          # to fetch data directly from online data bases
library(zoo)              # for the zoo function for daily time series
library(urca)             # for unit root tests
library(vars)             # for Granger tests and VAR estimation
library(car)              # for regression diagnostics and hypothesis testing
library(dynlm)            # for Vector Error Correction Model(VECM)
library(tsDyn)            # for linear and non-linear VAR and VECM models
library(gets)             # for Isat function: step and impsulse indicator saturation
library(readxl)           # to read Excel files and load the data from Excel files
library(aod)              # for Wald tests
library(egcm)             # Engle-Granger cointegration test
library(aTSA)             # Engle-Granger cointegration test
library(tidyverse)

# Clear the data buffer:

rm(list=ls())

# Inflation data:

CPI = pdfetch_FRED("CPIAUCSL")
names(CPI) = "CPI"

Inflation = diff(log(CPI), lag = 12) * 100                               # Percent change from year ago
names(Inflation) = "Inflation"

Inflation = ts(Inflation, start=c(1947, 1), frequency=12)
Inflation = na.omit(Inflation)                                                 # inflation series begins in January 1948
# Capacity utilization data:

TCU = pdfetch_FRED("TCU")
names(TCU) = "TCU"
TCU = ts(TCU, start=c(1967, 1), frequency=12)

# Plotting Data

# Plot TCU in levels
ggplot() +
  geom_line(data = TCU, aes(x = time(TCU), y = TCU), color = 'blue') +
  labs(title = "Capacity Utilisation Rate (TCU)") +
  ylab("TCU") +
  xlab("Year")
## Question Two
#The data consists of two time series: the Consumer Price Index (CPI) and the Capacity Utilization Rate (CUR).
#The CPI measures the average change in prices over time of a fixed basket of goods and services consumed by households. It is calculated by the Bureau of Labor Statistics (BLS) in the United States and is widely used as a measure of inflation. The CPI in this dataset is measured in levels and represents the monthly average price of the fixed basket of goods and services in the United States.
#The Capacity Utilization Rate (CUR) is a measure of the extent to which a firm is using its installed productive capacity. It is calculated as the ratio of actual output to potential output, where potential output is the maximum level of output that a firm can produce with its installed productive capacity. In this dataset, the CUR is measured in levels and represents the percentage of productive
#capacity that is being utilized in the manufacturing, mining, and electric and gas utilities industries in the United States.
# Plot CPIAUCSL in levels
ggplot() +
  geom_line(data = CPI, aes(x = time(CPI), y = CPI), color = 'red') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL)") +
  ylab("CPIAUCSL") +
  xlab("Year")

# Plot TCU in first differences
ggplot() +
  geom_line(data = diff(TCU), aes(x = time(TCU)[2:length(time(TCU))], y = diff(TCU)), color = 'blue') +
  labs(title = "Capacity Utilisation Rate (TCU) - First Differences") +
  ylab("TCU Diff") +
  xlab("Year")

# Plot CPIAUCSL in first differences
ggplot() +
  geom_line(data = CPI, aes(x = time(CPI), y = CPI), color = 'blue') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL)") +
  ylab("CPIAUCSL") +
  xlab("Year")

ggplot() +
  geom_line(data = diff(CPI), aes(x = time(CPI), y = diff(Inflation)), color = 'red') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL) - First Differences") +
  ylab("CPIAUCSL Diff") +
  xlab("Year")





