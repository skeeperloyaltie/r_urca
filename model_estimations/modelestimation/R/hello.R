library(dynamac)
library(forecast)
library(tseries)
library(nlme)
library(pdfetch)
library(zoo)
library(urca)
library(vars)
library(car)
library(dynlm)
library(tsDyn)
library(gets)
library(readxl)
library(aod)
library(egcm)
library(aTSA)
library(tidyverse)

rm(list=ls())

CPI = pdfetch_FRED("CPIAUCSL")
names(CPI) = "CPI"

Inflation = diff(log(CPI), lag = 12) * 100
names(Inflation) = "Inflation"
Inflation = ts(Inflation, start=c(1947, 1), frequency=12)
Inflation = na.omit(Inflation)
head(CPI)
TCU = pdfetch_FRED("TCU")
names(TCU) = "TCU"
TCU = ts(TCU, start=c(1967, 1), frequency=12)
head(TCU)

# Plot TCU in levels
ggplot() +
  geom_line(data = TCU, aes(x = time(TCU), y = TCU), color = 'blue') +
  labs(title = "Capacity Utilisation Rate (TCU)") +
  ylab("TCU") +
  xlab("Year")


# Load required libraries
library(lattice)
library(gridExtra)

# Compute lag correlations
lag_cor_levels <- acf(TCU, lag.max = 24, plot = FALSE)$acf
lag_cor_diff <- acf(diff(TCU), lag.max = 24, plot = FALSE)$acf
lag_cor_levels_pacf <- pacf(TCU, lag.max = 24, plot = FALSE)
lag_cor_levpac <- pacf(diff(TCU), lag.max = 24, plot= FALSE)


# Plot the lag correlations
par(mfrow = c(2,1))
plot(lag_cor_levels, type = "h", main = "ACF Lag Correlation of TCU in Levels")
abline(h = 0, lty = 2)
plot(lag_cor_diff, type = "h", main = "ACF Lag Correlation of First Difference of TCU")
abline(h = 0, lty = 2)
plot(lag_cor_levels_pacf, main = "PACF Lag Correlation of TCU in Levels")
abline(h = 0, lty = 2)

plot(lag_cor_levpac, main = "PACF Lag Correlation of First Difference of TCU")
abline(h = 0, lty = 2)

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

ggplot() +
  geom_line(data = CPI, aes(x = time(CPI), y = CPI), color = 'blue') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL)") +
  ylab("CPIAUCSL") +
  xlab("Year")

# ADF Tests for inflation in levels
ur.df(Inflation, type = "drift", lags = 12, selectlags = "AIC")
summary(ur.df(Inflation, type = "drift", lags = 12, selectlags = "AIC"))
ur.pp(Inflation, type = "Z-alpha", lags = NULL)
summary(ur.pp(Inflation, type = "Z-alpha", lags = NULL))

# KPSS test for inflation in levels
summary(ur.kpss(Inflation, type = "tau", lags = "short"))
# ADF test for inflation in first differences
ur.df(diff(Inflation), type = "drift", lags = 12, selectlags = "AIC")
summary(ur.df(diff(Inflation), type = "drift", lags = 12, selectlags = "AIC"))

# PP test for inflation in first differences
ur.pp(diff(Inflation), type = "Z-alpha", lags = NULL)
summary(ur.pp(diff(Inflation), type = "Z-alpha", lags = NULL))
# KPSS test for inflation in first differences
summary(ur.kpss(diff(Inflation), type = "tau", lags = "short"))

# ADF Tests for TCu in levels
ur.df(TCU, type = "drift", lags = 12, selectlags = "AIC")
summary(ur.df(TCU, type = "drift", lags = 12, selectlags = "AIC"))
# PP tets for TCu in levels
ur.pp(TCU, type = "Z-alpha", lags = NULL)
summary(ur.pp(TCU, type = "Z-alpha", lags = NULL))
# KPSS test for TCU in levels

summary(ur.kpss(TCU, type = "tau", lags = NULL))

# Combine the two time series into a matrix
data <- cbind(TCU, Inflation)

# Engle-Granger cointegration test
eg_test <- ca.jo(data, type = "trace", K = 2)
summary(eg_test)
# Johansen cointegration test
j_test <- ca.jo(data, type = "eigen", K = 2)
summary(j_test)

# VAR Model
library(vars)

data <- cbind(TCU, Inflation)
data <- na.omit(data)
model <- VAR(data, p = 2, type = "both", season = NULL)
summary(model)

# Create the first-difference series of the data
diff_data <- diff(data)

# Estimate the VAR model
model_diff <- VAR(diff_data, p = 2, type = "both", season = NULL)

# Test for Granger causality in both directions
causality_test1 <- causality(model_diff, cause = "TCU")
causality_test2 <- causality(model_diff, cause = "Inflation")

# Print the results
print(causality_test1)
print(causality_test2)

data <- cbind(TCU, Inflation)
data_diff <- diff(data)

# Run Johansen test for cointegration
j <- ca.jo(data, type="trace", K=2)
summary(j)

# ARDL Modelling - Impulse Responce Evaluation

m <- merge(Inflation, TCU)
head(m)
ardl.model <- dynardl(x ~ TCU, data = m, lags = list("TCU" = 1, "x" = 1), shockvar = 'TCU',
                      diffs = c("x", "TCU"), simulate = TRUE)
summary(ardl.model)
jarque.bera.test(residuals(ardl.model))

# Estimate VEC model
vecm <- ca.jo(data, type="eigen", K=2, spec="longrun")
summary(vecm)

# Perform Cholesky decomposition to make the structural errors orthogonal
my_irf <- irf(model, impulse = "Inflation", response = "TCU", n.ahead = 10, ortho = TRUE)

# Plot the impulse-response functions
plot(my_irf)

library(forecast)
# we can first use auto.arima to find the best ARIMA model
fit <- auto.arima(Inflation)

# view the summary of the ARIMA model
summary(fit)

forecast_ <- estimate(Inflation)

forecast_
