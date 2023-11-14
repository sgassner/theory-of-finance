#------------------------------------------------------------------------------#
# Theory of Finance, 2022
# Problem Set 2
# Authors:
# - Sandro Gassner
#------------------------------------------------------------------------------#

# set working directory
setwd("~/Documents/R/TheoryOfFinance/assignment2")

# load packages
# install.packages("rootSolve")
# install.packages("PerformanceAnalytics")
library(tidyverse)
library(lubridate)
library(rootSolve)
library(PerformanceAnalytics)

# load data
data <- read.csv("data_ps2.csv")

# look at data set structure
str(data)
summary(data)

# reformat date column
data$Date <- as_date(data$Date)

# sort by date
data <- data %>% arrange(ymd(data$Date))

#------------------------------------------------------------------------------#
# Problem 1
#------------------------------------------------------------------------------#

## 1a)

# convert from wide to long format
data_long <- data %>% gather("Stock", "Price", -Date)

# compute monthly net returns for each stock
data_long <- data_long %>% group_by(Stock) %>% 
  mutate(r_month = (Price - lag(Price)) / lag(Price))

# convert back to wide format
returns_wide <- data_long %>% select(-Price)
returns_wide <- returns_wide %>% spread("Stock", "r_month")

# the risk-free rate is at 0% -> excess return = monthly return
# we use SMI as market benchmark

# calculate mean excess return
stock_returns <- data_long %>% group_by(Stock) %>% 
  summarize(r_excess = mean(r_month, na.rm = TRUE))

# calculate annualized mean excess return
stock_returns$r_excess_annual <- stock_returns$r_excess * 12

# add columns to store alphas and betas
stock_returns$alphas <- NA
stock_returns$betas <- NA
stock_returns$alpha_tvalue <- NA
stock_returns$alpha_pvalue <- NA

# define stock names
stock_names <- c("Nestle", "UBS", "CS", "Swiss.Re", "Roche")

# calculate alphas and betas
for (i in stock_names) {
  # run linear model
  ols <- lm(formula = paste0(i, " ~ SMI"), data = returns_wide)
  # store alpha cefficients (intercept)
  stock_returns$alphas[stock_returns$Stock == i] <- 
    as.numeric(ols$coefficients[1])
  # store beta coefficients
  stock_returns$betas[stock_returns$Stock == i] <- 
    as.numeric(ols$coefficients[2])
  # store t values
  stock_returns$alpha_tvalue[stock_returns$Stock == i] <- 
    summary(ols)$coefficients[1,3]
  # store p values
  stock_returns$alpha_pvalue[stock_returns$Stock == i] <- 
    summary(ols)$coefficients[1,4]
}

# make predictions with CAPM
returns_wide_CAPM <- returns_wide

for (i in stock_names) {
  returns_wide_CAPM[i] <- stock_returns$alphas[stock_returns$Stock == i] + 
    (stock_returns$betas[stock_returns$Stock == i] * returns_wide_CAPM$SMI)
}

# store prediction of mean return by CAPM
stock_returns$r_excess_CAPM <- NA
for (i in stock_names) {
  preds_i <- as.data.frame(returns_wide_CAPM[,i])
  stock_returns$r_excess_CAPM[stock_returns$Stock == i] <- 
    mean(preds_i[,1], na.rm = TRUE)
}

# prediction of mean return by CAPM
stock_returns$r_excess_annual_CAPM <- stock_returns$r_excess_CAPM * 12

# remove SMI from the data frame as it is the market benchmark
# stock_returns <- stock_returns %>% filter(Stock != "SMI")

# plot 1: y = mean excess annualized returns, x = betas
ggplot(na.omit(stock_returns), 
       aes(x = betas, y = r_excess_annual, group = Stock)) +
  geom_point(aes(color = Stock), size = 3) +
  xlab("Betas") +
  ylab("Mean Excess Annualized Returns")

# plot 2: y = mean excess annualized returns, x = prediction CAPM
ggplot(na.omit(stock_returns), 
       aes(x = r_excess_annual_CAPM, y = r_excess_annual, group = Stock)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_point(aes(color = Stock), size = 3) +
  xlab("Annualized Excess Return Predicted by CAPM") +
  ylab("Mean Excess Annualized Returns")
  

## 1b)

# convert actual and predicted returns to long format
returns_long <- returns_wide %>% select(-SMI)
returns_long <- returns_long %>% gather("Stock", "r_month", -Date)
returns_long_CAPM <- returns_wide_CAPM %>% select(-SMI)
returns_long_CAPM <- returns_long_CAPM %>% gather("Stock", "r_month", -Date)

# t-test of actual and predicted returns
t.test(returns_long$r_month, returns_long_CAPM$r_month)

# t-test for null-hypothesis: alpha = 0
t.test(na.omit(stock_returns[["alphas"]]), rep(0, 5))

# show t- and p-values for null-hypothesis: alpha = 0 for each stock
t_statistics <- na.omit(stock_returns %>% 
                          select("Stock", "alpha_tvalue", "alpha_pvalue"))
t_statistics[,2:3] <- round(t_statistics[,2:3], 4)
t_statistics

## 1c)

# beta i
beta_i <- 1.1

# variance of the index (SMI)
var_smi <-  var(returns_wide$SMI, na.rm = TRUE)

# variance of asset i (formula 4.8)
var_i <- (beta_i^2) * var_smi

# standard deviation of asset i
sd_i <- sqrt(var_i)
sd_i

# create data frame to store correlations
corr_ij <- data.frame(stock_j = stock_names, corr_ij = NA)

# covariance of asset i with the other stocks j (formula 4.9)
for (j in stock_names) {
  cov_ij <- beta_i * stock_returns$betas[stock_returns$Stock == j] * var_smi
  sd_j <- sd(na.omit(returns_wide[[j]]))
  corr_ij$corr_ij[corr_ij$stock_j == j] <- cov_ij / (sd_i * sd_j)
}

# round and show results
corr_ij$corr_ij <- round(corr_ij$corr_ij, 4)
corr_ij

## 1d)

# calculate stock sd's
stock_sd <- data_long %>% group_by(Stock) %>% 
  summarize(sd_excess = sd(r_month, na.rm = TRUE))

# calculate annualized stock sd's
stock_sd$sd_excess_annual <- stock_sd$sd_excess * sqrt(12)

# calculate residuals of different stocks
residuals_stocks <- returns_wide
residuals_stocks[,2:7] <- returns_wide[,2:7] - returns_wide_CAPM[,2:7]

# calculate sd's of the residuals
stock_sd$sd_residual <- NA
for (i in c(stock_names, "SMI")) {
  stock_sd$sd_residual[stock_sd$Stock == i] <- 
    sd(residuals_stocks[[i]], na.rm = TRUE)
}

# calculate annualized sd's of the residuals
stock_sd$sd_residual_annual <- stock_sd$sd_residual * sqrt(12)

# create empty data frame to store performance measures
performance_measures <- data.frame(Stock = stock_names,
                                   jensens_alpha = NA,
                                   sharpe_ratio = NA,
                                   m2 = NA,
                                   appraisal_ratio = NA,
                                   information_ratio = NA,
                                   treynors_ratio = NA,
                                   t2 = NA)

# calculate performance measures
for (i in stock_names) {
  # calculate jensens_alpha
  performance_measures$jensens_alpha[performance_measures$Stock == i] <- 
    stock_returns$r_excess_annual[stock_returns$Stock == i] - 
    (stock_returns$r_excess_annual[stock_returns$Stock == "SMI"] * 
       stock_returns$betas[stock_returns$Stock == i])
  # calculate sharpe_ratio
  performance_measures$sharpe_ratio[performance_measures$Stock == i] <- 
    stock_returns$r_excess_annual[stock_returns$Stock == i] /
    stock_sd$sd_excess_annual[stock_sd$Stock == i]
  # calculate m2
  performance_measures$m2[performance_measures$Stock == i] <- 
    ((stock_returns$r_excess_annual[stock_returns$Stock == i] /
       stock_sd$sd_excess_annual[stock_sd$Stock == i]) - 
       (stock_returns$r_excess_annual[stock_returns$Stock == "SMI"] /
          stock_sd$sd_excess_annual[stock_sd$Stock == "SMI"])) *
    stock_sd$sd_excess_annual[stock_sd$Stock == "SMI"]
  # calculate appraisal_ratio
  performance_measures$appraisal_ratio[performance_measures$Stock == i] <- 
    stock_returns$alphas[stock_returns$Stock == i] * 12 /
    stock_sd$sd_residual_annual[stock_sd$Stock == i]
  # calculate information_ratio
  performance_measures$information_ratio[performance_measures$Stock == i] <- 
    (stock_returns$r_excess_annual[stock_returns$Stock == i] - 
       stock_returns$r_excess_annual[stock_returns$Stock == "SMI"]) /
      (sd(na.omit(returns_wide[[i]]) - na.omit(returns_wide[["SMI"]])) * sqrt(12))
  # calculate treynors_ratio
  performance_measures$treynors_ratio[performance_measures$Stock == i] <- 
    stock_returns$r_excess_annual[stock_returns$Stock == i] /
    stock_returns$betas[stock_returns$Stock == i]
  # calculate t2
  performance_measures$t2[performance_measures$Stock == i] <- 
    (stock_returns$r_excess_annual[stock_returns$Stock == i] / 
    stock_returns$betas[stock_returns$Stock == i]) - 
    stock_returns$r_excess_annual[stock_returns$Stock == "SMI"]
}

# round and show results
performance_measures[,2:8] <- round(performance_measures[,2:8], 4)
performance_measures

#------------------------------------------------------------------------------#
# Problem 2
#------------------------------------------------------------------------------#

## 2a)

# create empty data frame to store risk measures
risk_measures <- data.frame(Stock = stock_names,
                            mean_absolute_deviation = NA,
                            semi_std_deviation = NA,
                            empirical_VaR = NA,
                            empirical_exp_shotfall = NA)

# write mean absolute deviation function
MeanAbsDev <- function(returnVector, digits = 4) 
{
  returnVector <- as.vector(na.omit(returnVector))
  returns_mean <- mean(returnVector)
  absolute_deviation <- abs(returnVector - returns_mean)
  ans <- mean(absolute_deviation)
  return(round(ans, digits = digits))
}

# write emprirical VaR function
VaRempirical <- function(returnVector, prob = 0.05, notional = 1, digits = 4) 
{
  returnVector <- as.vector(na.omit(returnVector))
  if(prob > .5) prob <- 1 - prob
  ans <- -quantile(returnVector, prob) * notional
  return(round(ans, digits = digits))
}

# write expected shortfall function
ESempirical <- function(returnVector, prob = 0.05, notional = 1, digits = 4) 
{
  returnVector <- as.vector(na.omit(returnVector))
  if(prob > .5) prob <- 1 - prob
  v <- quantile(returnVector, prob)
  ans <- -mean(returnVector[returnVector <= v]) * notional
  return(round(ans, digits = digits))
}

# calculate risk measures for all stocks
for (i in stock_names) {
  
  # mean absolute deviation
  risk_measures$mean_absolute_deviation[risk_measures$Stock == i] <- 
    MeanAbsDev(returns_wide[[i]])
  
  # semi-standard deviation
  risk_measures$semi_std_deviation[risk_measures$Stock == i] <-
    round(DownsideDeviation(returns_wide[[i]], 
                      MAR = mean(na.omit(returns_wide[[i]]))), 4)
  
  # empirical value at risk
  risk_measures$empirical_VaR[risk_measures$Stock == i] <-
    VaRempirical(returns_wide[[i]])
  
  # empirical expected shortfall
  risk_measures$empirical_exp_shotfall[risk_measures$Stock == i] <-
    ESempirical(returns_wide[[i]])
}

# show risk measures
risk_measures

## 2c)

# calculate mean and sd of UBS return history
mean_return_UBS <- mean(returns_wide$UBS, na.rm = TRUE)
sd_UBS <- sd(returns_wide$UBS, na.rm = TRUE)

# create histogram with normal density distribution
ggplot(data = returns_wide) + 
  geom_histogram(mapping = aes(x = UBS, y =..density..), fill="blue", 
                 color = "white", bins = 100) +
  labs(title="Monthly Returns of UBS", x = "Monthly Return", y = "Count") +
  stat_function(fun = dnorm, args = list(mean = mean_return_UBS, sd = sd_UBS),
                color = "red")

# create Q-Q-plot
ggplot(returns_wide, aes(sample = UBS)) + 
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title="Q-Q Plot of UBS Monthly Returns", 
       x = "Theoretical Quantiles (Standard Normal Distribution)", 
       y = "Sample Quantiles")

#------------------------------------------------------------------------------#
# Problem 3
#------------------------------------------------------------------------------#

## 3a)

# define parameters
target_VaR <- 0.08
confidence <- 0.01
rf <- 0.001

# filter data for UBS
data_UBS <- na.omit(data_long) %>% filter(Stock == "UBS")

# store risk free rate in a column
data_UBS$r_riskfree <- rf

# create columns to store expected return and sd
data_UBS$r_UBS_expected <- NA
data_UBS$sd_UBS_expected <- NA
data_UBS$VaR_UBS <- NA
data_UBS$VaR_rf <- NA

# calculate the expected returns and sd's and VaR's
for (i in 61:350) {
  data_UBS$r_UBS_expected[i] <- mean(data_UBS$r_month[(i-60):(i-1)])
  data_UBS$sd_UBS_expected[i] <- sd(data_UBS$r_month[(i-60):(i-1)])
  data_UBS$VaR_UBS[i] <- VaR(data_UBS$r_month[(i-60):(i-1)], p = 0.99, 
                             method = "gaussian") * (-1)
  data_UBS$VaR_rf[i] <- -0.001
}

# delete observations before 1998-06-29
data_UBS <- na.omit(data_UBS)

# create column for weights of UBS
data_UBS$weight_UBS <- NA

# covariance matrix of UBS and rf
cov_ubs_rf <- cov(as.matrix(data.frame(data_UBS[["VaR_UBS"]], 
                                       data_UBS[["VaR_rf"]])))

# calculate weight of UBS
for (i in 1:290) {
  weight_function = function(x) {
    if (data_UBS$r_UBS_expected[i] >= data_UBS$r_riskfree[i]) {
      -((target_VaR + data_UBS$r_riskfree[i])/
          (data_UBS$r_UBS_expected[i] + (-2.325) * 
             data_UBS$sd_UBS_expected[i] - data_UBS$r_riskfree[i])) - x
    } else {
      -((target_VaR + data_UBS$r_riskfree[i])/
          (data_UBS$r_UBS_expected[i] - (-2.325) * 
             data_UBS$sd_UBS_expected[i] - data_UBS$r_riskfree[i])) - x
    }
  }
  data_UBS$weight_UBS[i] <- uniroot.all(weight_function, lower = -1, upper = 1)
}

# calculate weights for risk free asset
data_UBS$weight_riskfree <- 1 - data_UBS$weight_UBS

# plot weights of UBS
ggplot(data_UBS, aes(x = Date, y = weight_UBS)) +
  geom_line() +
  labs(title="Weights of UBS (8%-VaR-Target-Portfolio)", 
       x = "Date", y = "Weight of UBS") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# calculate portfolio price for the first month
data_UBS$portfolio_price_UBS <- NA
data_UBS$portfolio_price_riskfree <- NA

data_UBS$portfolio_price_UBS[1] <- 1 * data_UBS$weight_UBS[1] * 
  (1 + data_UBS$r_month[1])
data_UBS$portfolio_price_riskfree[1] <- 1 * data_UBS$weight_riskfree[1] * 
  (1 + data_UBS$r_riskfree[1])
data_UBS$portfolio_price[1] <- data_UBS$portfolio_price_UBS[1] + 
  data_UBS$portfolio_price_riskfree[1]

# calculate portfolio prices for all remaining months
for (i in 2:290) {
  data_UBS$portfolio_price_UBS[i] <- data_UBS$portfolio_price[(i-1)] * 
    data_UBS$weight_UBS[i] * (1 + data_UBS$r_month[i])
  data_UBS$portfolio_price_riskfree[i] <- data_UBS$portfolio_price[(i-1)] * 
    data_UBS$weight_riskfree[i] * (1 + data_UBS$r_riskfree[i])
  data_UBS$portfolio_price[i] <- data_UBS$portfolio_price_UBS[i] + 
    data_UBS$portfolio_price_riskfree[i]
}

# theoretical price development of a portfolio of the risk free asset only
data_UBS$riskfree_only <- NA
data_UBS$riskfree_only[1] <- 1 + data_UBS$r_riskfree[1]
for (i in 2:290) {
  data_UBS$riskfree_only[i] <- data_UBS$riskfree_only[(i-1)] *
    (1 + data_UBS$r_riskfree[i])
}

# prepare data frame for plot
df_plot <- data_UBS %>% select(Date, portfolio_price, riskfree_only) %>% 
  pivot_longer(., cols = c(portfolio_price, riskfree_only), 
               names_to = "Portfolio", values_to = "Price")

# plot portfolio price development
ggplot(df_plot, aes(x = Date, y = Price, group = Portfolio)) +
  geom_line(aes(color = Portfolio)) +
  scale_y_continuous(breaks = seq(0,2, by = 0.2)) +
  labs(title="Price Development (8%-VaR-Target-Portfolio)", 
       x = "Date", 
       y = "Price (Initial Investment = 1)") +
  theme(legend.position="bottom")

# portfolio returns
data_UBS <- data_UBS %>% 
  mutate(r_portfolio = (portfolio_price - lag(portfolio_price)) / 
           lag(portfolio_price))
data_UBS$r_portfolio[1] <- data_UBS$portfolio_price[1] - 1

# portfolio mean annual return and sd
mean(data_UBS$r_portfolio) * 12
sd(data_UBS$r_portfolio) * sqrt(12)

## 3b)

# actual portfolio VaR (theoretical)
data_UBS$VaR_portfolio <- NA
for (i in 61:290) {
  data_UBS$VaR_portfolio[i] <- VaR(data_UBS$r_portfolio[(i-60):(i-1)], 
                                   p = 0.99, method = "gaussian") * (-1)
}

# plot actual portfolio VaR (theoretical)
ggplot(data_UBS, aes(x = Date, y = VaR_portfolio)) +
  geom_line() +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "red") +
  labs(title="Actual Portfolio-VaR (8%-VaR-Target-Portfolio)", 
       x = "Date", 
       y = "VaR")

# count and print months where portfolio VaR exceeded 0.08
higher_VaR <- as.numeric(colSums(na.omit(data_UBS["VaR_portfolio"]) > 0.08))
n_VaR <- as.numeric(sum(!is.na(data_UBS$VaR_portfolio)))
cat("From ", n_VaR, 
    " months observed, the Portfolio-VaR exceeded the target of 8% in ",
    higher_VaR, " months.")
