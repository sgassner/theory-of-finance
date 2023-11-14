#------------------------------------------------------------------------------#
# Theory of Finance, 2022
# Problem Set 5
# Deadline: 30.12.2022
# Authors:
# - Sandro Gassner
#------------------------------------------------------------------------------#

# set working directory
setwd("~/Documents/R/TheoryOfFinance/assignment5")

# load packages
# install.packages("rootSolve")
library(tidyverse)
library(lubridate)
library(rootSolve)
library(knitr)

# load data
data <- read.csv("data_ps5.csv", sep = ";")

# reformat date column
data$date <- as.Date(format(as.Date(data$date, "%d.%m.%Y"), "%Y-%m-%d"))

# define index price and risk free rate
s <- 4766.18
y <- 0.531 / 100

################################################################################
### Problem 1
################################################################################

## 1a) ------------------------------------------------------------------------#

# define up and down factor
u <- 1.1
d <- 1 / u

# define maturity
h <- (3 / 12) / 3

# calculate risk neutral probability (LN 20.8)
p <- (exp(y * h) - d) / (u - d)

## 1b) ------------------------------------------------------------------------#

# price of european call option with m = 3 and strike = 5000

# define strike value
k = 5000

# calculate prices of underlying s
s_uuu <- s * u^3
s_uud <- s * u^2 * d
s_ddu <- s * d^2 * u
s_ddd <- s * d^3

# calculate prices of derivative f
f_uuu <- max(s_uuu - k, 0)
f_uud <- max(s_uud - k, 0)
f_ddu <- max(s_ddu - k, 0)
f_ddd <- max(s_ddd - k, 0)

# calculate option price (LN 20.21)
option_price_b <- exp(-(y*h)) * ((p^3*f_uuu) + (3*p^2*(1-p)*f_uud) + 
                                   (3*p*(1-p)^2*f_ddu) + ((1-p)^3*f_ddd))

## 1c) ------------------------------------------------------------------------#

# define strike value
kc <- 5200

# define all values for different paths
s_u <- s * u
s_d <- s * d
s_uu <- s * u^2
s_ud <- s * u * d
s_du <- s * d * u
s_dd <- s * d^2
s_uuu <- s * u^3
s_uud <- s * u^2 * d
s_udd <- s * u * d^2
s_ddd <- s * d^3
s_ddu <- s * d^2 * u
s_duu <- s * d * u^2
s_dud <- s * d * u * d
s_udu <- s * u * d * u

# calculate prices of derivative f (highest price of the path)
fc_uuu <- max(s_u - kc, s_uu - kc, s_uuu - kc, 0)
fc_uud <- max(s_u - kc, s_uu - kc, s_uud - kc, 0)
fc_udd <- max(s_u - kc, s_ud - kc, s_udd - kc, 0)
fc_ddd <- max(s_d - kc, s_dd - kc, s_ddd - kc, 0)
fc_ddu <- max(s_d - kc, s_dd - kc, s_ddu - kc, 0)
fc_duu <- max(s_d - kc, s_du - kc, s_duu - kc, 0)
fc_dud <- max(s_d - kc, s_du - kc, s_dud - kc, 0)
fc_udu <- max(s_u - kc, s_ud - kc, s_udu - kc, 0)

# calculate exotic option price
option_price_c <- exp(-(y*h)) * ((p^3*fc_uuu) +
                                   (p^2*(1-p)*fc_uud) +
                                   (p*(1-p)^2*fc_udd) +
                                   ((1-p)^3*fc_ddd) +
                                   ((1-p)^2*p*fc_ddu) +
                                   ((1-p)*p^2*fc_duu) +
                                   ((1-p)*p*(1-p)*fc_dud) +
                                   (p*(1-p)*p*fc_udu))
option_price_c

################################################################################
### Problem 2
################################################################################

## 2a) ------------------------------------------------------------------------#

# define maturity and dividend yield
m <- 1 / 12
div <- 1.256 / 100

# fair price for forward contract (15.7)
forward_price <- s * exp(m * (y - div))

## 2b) ------------------------------------------------------------------------#

# rename option_price to call_price
data <- data %>% rename("call_price" = "option_price")

# calculate put prices according to put-call parity (LN 19.5)
data$put_price <- data$call_price - (exp(-m*y) * (forward_price - data$strike))

# prepare data frame for plot
data_long <- data %>% select(c("strike", "call_price", "put_price"))
data_long <- data_long %>% pivot_longer(cols = c("call_price", "put_price"), 
               names_to = "type", values_to = "price")

# create plot
ggplot(data_long, aes(x = strike, y = price, group = type)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type)) +
  geom_vline(xintercept = forward_price, linetype = "dashed") +
  labs(title = "Call & Put Prices for Different Strike Levels", 
       x = "Strike", 
       y = "Option Price") +
  theme(legend.position = "bottom")

## 2c) ------------------------------------------------------------------------#

# define volatility
volatility <- 13.43 / 100

# create column to store black-scholes call prices 
data$call_price_theoretical <- NA

# write black-scholes call price function (LN 21.7 & 21.8)
fun_bs_call_price <- function(pice_underlying, maturity, strike, interest_rate, 
                              dividend, volatility) {
  # simplify parameters
  s <- pice_underlying
  m <- maturity
  k <- strike
  y <- interest_rate
  d <- dividend
  sd <- volatility
  # calculate d1 and d2 for BS formula
  d_1 <- (log(s / k) + (y - d + sd^2 / 2) * m) / (sd * sqrt(m))
  d_2 <- d_1 - (sd * sqrt(m))
  # calculate probability of x <= d for x ~ N(0,1)
  p_d_1 <- pnorm(d_1, mean = 0, sd = 1)
  p_d_2 <- pnorm(d_2, mean = 0, sd = 1)
  # calculate theoretical call price
  return((exp(-d * m) * s * p_d_1) - (exp(-y * m) * k * p_d_2))
}

# calculate theoretical call option prices for all strike levels
for (k in data$strike) {
  data$call_price_theoretical[data$strike == k] <- 
    fun_bs_call_price(s, m, k, y, div, volatility)
}

# prepare data frame for plot
df_bs <- data %>% select(c("strike", "call_price", "call_price_theoretical"))
df_bs <- df_bs %>% 
  pivot_longer(cols = c("call_price", "call_price_theoretical"),
               names_to = "type", values_to = "price")

# create plot
ggplot(df_bs, aes(x = strike, y = price, group = type)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type)) +
  geom_vline(xintercept = forward_price, linetype = "dashed") +
  labs(title = "Empirical vs. Theoretical Call Prices", 
       x = "Strike", 
       y = "Call Price") +
  theme(legend.position = "bottom")

## 2d) ------------------------------------------------------------------------#

# create column to store implied volotilities
data$implied_volatility <- NA

# calculate Black-Scholes implied volatilities for all strikes (LN 21.3)
for (k in data$strike) {
  fun_bs_implied_sd <- function(volatility) {
    fun_bs_call_price(s, m, k, y, div, volatility) - 
      data$call_price[data$strike == k]
  }
  data$implied_volatility[data$strike == k] <- 
    uniroot(fun_bs_implied_sd, c(-100, 100))$root
}

# create plot
ggplot(data, aes(x = strike, y = implied_volatility)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = volatility, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = forward_price, linetype = "dashed", colour = "blue") +
  labs(title = "Black-Scholes Implied Volatilities", 
       x = "Strike", 
       y = "Implied Volatility")

################################################################################
### Voluntary Extra Exercise
################################################################################

## Va) ------------------------------------------------------------------------#

# store strike^2
data$strike_sq <- data$strike^2

# run quadratic regression
ols <- lm(implied_volatility ~ strike + strike_sq, data = data)

# create data frame to store new strike levels
df_sd <- data.frame(strike = seq(4000, 5500, by = 10), volatility = NA)

# calculate volatilities for all strike levels
for (k in df_sd$strike){
  df_sd$volatility[df_sd$strike == k] <- ols[["coefficients"]][["(Intercept)"]] +
    ols[["coefficients"]][["strike"]] * k +
    ols[["coefficients"]][["strike_sq"]] * k^2
}

# create plot
ggplot(df_sd, aes(x = strike, y = volatility)) +
  geom_line() +
  labs(title = "New Implied Volatility Surface", 
       x = "Strike (K')", 
       y = "Estimated Volatility")

## Vb) ------------------------------------------------------------------------#

# calculate Black-Scholes call prices for every strike k
df_sd$call_price <- NA
for (k in df_sd$strike) {
  df_sd$call_price[df_sd$strike == k] <-
    fun_bs_call_price(s, m, k, y, div, df_sd$volatility[df_sd$strike == k])
}

# compute density distribution for all strikes k
df_sd$density_distribution <- NA
for (i in 2:150){
  df_sd$density_distribution[i] <- 
    ((df_sd$call_price[i+1] - 2 * df_sd$call_price[i] + df_sd$call_price[i-1]) /
       (10^2)) * exp((y - div) * m)
}

# calculate underlying return
df_sd$underlying_return <- (df_sd$strike / s) - 1

# create plot
ggplot(df_sd, aes(x = underlying_return, y = density_distribution)) +
  geom_line() +
  labs(title = "Risk-Neutral Density Distribution", 
       x = "Underlying Return", 
       y = "Density Distribution")
