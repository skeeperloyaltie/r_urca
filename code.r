library(tidyverse)
library(lubridate)
library(tseries)
library(urca)
library(dynlm)
library(vars)
library(dynamac)
library(forecast)

# Importing and Cleaning Data
tcu <- read.csv("https://fred.stlouisfed.org/data/TCU.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= ymd('1995-01-01'))

cpi <- read.csv("https://fred.stlouisfed.org/data/CPIAUCSL.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= ymd('1995-01-01')) %>%
  mutate(CPIAUCSL = ((CPIAUCSL / lag(CPIAUCSL, 12)) - 1) * 100) %>%
  select(Date, CPIAUCSL)

# Plotting Data
ggplot() +
  geom_line(data = tcu, aes(x = Date, y = TCU), color = 'blue') +
  labs(title = "Capacity Utilisation Rate (TCU)") +
  ylab("TCU") +
  xlab("Year")

ggplot() +
  geom_line(data = cpi, aes(x = Date, y = CPIAUCSL), color = 'red') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL)") +
  ylab("CPIAUCSL") +
  xlab("Year")

ggplot() +
  geom_line(data = tcu %>% mutate(TCU_diff = diff(TCU)), aes(x = Date, y = TCU_diff), color = 'blue') +
  labs(title = "Capacity Utilisation Rate (TCU) - First Differences") +
  ylab("TCU Diff") +
  xlab("Year")

ggplot() +
  geom_line(data = cpi %>% mutate(CPIAUCSL_diff = diff(CPIAUCSL)), aes(x = Date, y = CPIAUCSL_diff), color = 'red') +
  labs(title = "Consumer Price Index Inflation Rate (CPIAUCSL) - First Differences") +
  ylab("CPIAUCSL Diff") +
  xlab("Year")


# Set the date column as the index
rownames(tcu) <- tcu$date
rownames(cpi) <- cpi$date

# Extract the columns with the data we need
tcu <- tcu[, "TCU"]
cpi <- cpi[, "CPIAUCSL"]
