#------------------------------------------------------------------------------#
# Theory of Finance 2022
# Problem Set 1
# Authors:
# - Sandro Gassner
#------------------------------------------------------------------------------#

# set working directory
setwd("~/Documents/R/TheoryOfFinance/assignment1")

# install packages
# install.packages(c("xts", "fPortfolio", "PerformanceAnalytics", "ggrepel", 
#                    "quadprog"))
# install.packages("IntroCompFinR", repos = "http://R-Forge.R-project.org", 
#                  type = "source")

# load packages
library(tidyverse)
library(lubridate)
library(IntroCompFinR)

# load data
data <- read.csv("data_ps1.csv")

# look at data set structure
str(data)
summary(data)

# reformat date column
data$Date <- as_date(data$Date)

# sort by date
data <- data %>% arrange(ymd(data$Date))

# convert from wide to long format
data_long <- data %>% gather("Stock", "Price", -Date)

#------------------------------------------------------------------------------#
# Problem 1
#------------------------------------------------------------------------------#

## 1a)

# compute monthly net returns for each stock
data_long <- data_long %>% group_by(Stock) %>% 
  mutate(r_month = (Price - lag(Price)) / lag(Price))

# compute net log returns
data_long <- data_long %>% group_by(Stock) %>% 
  mutate(r_log = log((Price / lag(Price))))

# compute mean return for each stock
stock_returns <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(r_month), list(mean_return = mean), na.rm = TRUE)

# compute mean log return for each stock
stock_returns_log <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(r_log), list(mean_return_log = mean), na.rm = TRUE)

# compute sd's for each stock
stock_sd <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(r_month), list(sd = sd), na.rm = TRUE)

# compute log sd's for each stock
stock_sd_log <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(r_log), list(sd_log = sd), na.rm = TRUE)

# annualize mean return and sd's
stock_returns$annual_mean_return <- stock_returns$mean_return * 12
stock_sd$annual_sd <- stock_sd$sd * sqrt(12)

stock_returns_log$annual_mean_return_log <- stock_returns_log$mean_return_log * 12
stock_sd_log$annual_sd_log <- stock_sd_log$sd_log * sqrt(12)

# join stock_returns and stock_sd and round results
stock_descriptives <- stock_returns %>% 
  inner_join(stock_sd, by = c("Stock" = "Stock")) %>% 
  mutate_if(is.numeric, round, digits = 4)

stock_descriptives <- stock_descriptives %>% 
  inner_join(stock_returns_log, by = c("Stock" = "Stock")) %>% 
  mutate_if(is.numeric, round, digits = 4)

stock_descriptives <- stock_descriptives %>% 
  inner_join(stock_sd_log, by = c("Stock" = "Stock")) %>% 
  mutate_if(is.numeric, round, digits = 4)

# show stock descriptives
stock_descriptives

write

## 1b)

# filter data for VOLKSWAGEN
data_long_volkswagen <- data_long %>% filter(Stock == "VOLKSWAGEN")

# create dot plot 
ggplot(data_long_volkswagen, aes(x = r_month, y = r_log)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlim(-0.4, 0.4) +
  ylim(-0.4, 0.4) +
  ggtitle("Returns for Volkswagen") +
  xlab("non-logarithmic returns") +
  ylab("logarithmic returns")

## 1c)

# define weights
w <- c(rep(1/7, 7))

# compute annualized mean returns for equally weighted portfolio
round(mean(stock_descriptives$annual_mean_return), 4)

# compute covariance matrix of non-logarithmic returns
returns <- data_long %>% select(-c(Price, r_log)) %>% spread("Stock", "r_month")
covariance_matrix <- cov(returns[2:114,2:8])

# compute standard deviation for equally weighted portfolio
sd_portfolio <- as.numeric(sqrt(t(w) %*% covariance_matrix %*% w))

# annualize standard deviation for equally weighted portfolio
sd_portfolio_ann <- sd_portfolio * sqrt(12)

# show rounded annualized standard deviation
round(sd_portfolio_ann, 4)

#------------------------------------------------------------------------------#
# Problem 2
#------------------------------------------------------------------------------#

## 2a)

# define risk free rates
rf_1 <- 0.001
rf_2 <- 0.01

# calculate excess returns
data_long$excess_return_1 <- data_long$r_month - rf_1
data_long$excess_return_2 <- data_long$r_month - rf_2

excess_returns_1 <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(excess_return_1), list(excess_return_1 = mean), na.rm = TRUE)

excess_returns_2 <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(excess_return_2), list(excess_return_2 = mean), na.rm = TRUE)

# compute sd's for excess returns
excess_sd_1 <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(excess_return_1), list(sd_1 = sd), na.rm = TRUE)

excess_sd_2 <- data_long %>% group_by(Stock) %>% 
  summarise_at(vars(excess_return_2), list(sd_2 = sd), na.rm = TRUE)

# join data for sharp ratios
sharp_ratios <- excess_returns_1 %>% 
  inner_join(excess_returns_2, by = c("Stock" = "Stock")) %>%
  inner_join(excess_sd_1, by = c("Stock" = "Stock")) %>%
  inner_join(excess_sd_2, by = c("Stock" = "Stock"))

# calculate sharp ratios
sharp_ratios$sharp_ratio_1 <- sharp_ratios$excess_return_1 / sharp_ratios$sd_1
sharp_ratios$sharp_ratio_2 <- sharp_ratios$excess_return_2 / sharp_ratios$sd_2

# show rounded results
sharp_ratios %>% mutate_if(is.numeric, round, digits = 4)

## 2b) & 2c)

# define investment, transaction fees and years
investment <- 100000
fee_1 <- 2000
fee_2 <- 3000
t <- 2

# define mean annual returns
r_ann_RWE <- as.numeric(stock_descriptives[5,3])
r_ann_BMW <- as.numeric(stock_descriptives[2,3])
r_ann_BAYER <- as.numeric(stock_descriptives[1,3])

# create data frame to store results
portfolio_roi <- data.frame(transaction_fee = c(fee_1, fee_2),
                            portfolio_1 = NA, portfolio_2 = NA, 
                            portfolio_3 = NA)

# portfolio 1 - only RWE
portfolio_roi[1,2] <- (investment * (1 + r_ann_RWE)^t - 
                         investment - fee_1) / investment
portfolio_roi[2,2] <- (investment * (1 + r_ann_RWE)^t - 
                         investment - fee_2) / investment

# portfolio 2 - 1/2 * RWE + 1/2 * BMW
portfolio_roi[1,3] <- (investment * (1 + (r_ann_RWE + r_ann_BMW)/2)^t - 
                         investment - 2*fee_1) / investment
portfolio_roi[2,3] <- (investment * (1 + (r_ann_RWE + r_ann_BMW)/2)^t - 
                         investment - 2*fee_2) / investment

# portfolio 3 - 1/3 * RWE + 1/3 * BMW + 1/3 * BAYER
portfolio_roi[1,4] <- (investment * 
                         (1 + (r_ann_RWE + r_ann_BMW + r_ann_BAYER)/3)^t - 
                         investment - 3*fee_1) / investment
portfolio_roi[2,4] <- (investment * 
                         (1 + (r_ann_RWE + r_ann_BMW + r_ann_BAYER)/3)^t - 
                         investment - 3*fee_2) / investment

# show portfolio returns
round(portfolio_roi, 4)

## 2d)

# define weight of SAP
w <- c(0.8, 0.2)
v <- 0.8

# calculate expected return
r_SAP <- as.numeric(stock_descriptives[6,2])
expected_return <- v * r_SAP + (1-v) * rf_1

# calculate excess return
excess_return <- r_SAP - rf_1

# compute portfolio variance
returns <- data_long %>% 
  select(-c(Price, r_log, excess_return_1, excess_return_2)) %>% 
  spread("Stock", "r_month")
returns_SAP <- na.omit(returns$SAP)
returns_rf <- c(rep(rf_1, length(returns_SAP)))
returns_2d <- data.frame(returns_SAP, returns_rf)
covariance_matrix_2d <- cov(returns_2d)
var_2d <- t(w) %*% covariance_matrix_2d %*% w

# solve FOC (formula 2.9) for k
k <- round(as.numeric((1/v)*(excess_return/covariance_matrix_2d[1,1])), 4)

# check optimal solution (formula 2.10)
k_check <- round(as.numeric((expected_return - rf_1) / var_2d), 4)

#------------------------------------------------------------------------------#
# Problem 3
#------------------------------------------------------------------------------#

## 3a)

# minimum variance portfolio for BMW and VW
er_BMW_VW <- c(as.numeric(stock_descriptives[2,2]), 
               as.numeric(stock_descriptives[7,2]))
names(er_BMW_VW) = c("BMW", "VOLKSWAGEN")
returns_BMW <- na.omit(returns$BMW)
returns_VW <- na.omit(returns$VOLKSWAGEN)
cov_BMW_VW <- as.matrix(cov(data.frame(returns_BMW, returns_VW)))
gm_BMW_VW <- globalMin.portfolio(er_BMW_VW, cov_BMW_VW, shorts = TRUE)

gm_BMW_VW_annual <- gm_BMW_VW
gm_BMW_VW_annual$er <- gm_BMW_VW_annual$er * 12
gm_BMW_VW_annual$sd <- gm_BMW_VW_annual$sd * sqrt(12)
gm_BMW_VW_annual

# minimum variance portfolio for RWE and VW
er_RWE_VW <- c(as.numeric(stock_descriptives[5,2]), 
               as.numeric(stock_descriptives[7,2]))
names(er_RWE_VW) = c("RWE", "VOLKSWAGEN")
returns_RWE <- na.omit(returns$RWE)
cov_RWE_VW <- as.matrix(cov(data.frame(returns_RWE, returns_VW)))
gm_RWE_VW <- globalMin.portfolio(er_RWE_VW, cov_RWE_VW, shorts = TRUE)

gm_RWE_VW_annual <- gm_RWE_VW
gm_RWE_VW_annual$er <- gm_RWE_VW_annual$er * 12
gm_RWE_VW_annual$sd <- gm_RWE_VW_annual$sd * sqrt(12)
gm_RWE_VW_annual

## 3b)

# minimum variance portfolio for all 7 stocks
er_all <- as.vector(stock_descriptives$mean_return)
names(er_all) <- c("BAYER", "BMW", "DTE", "EON", "RWE", "SAP", "VW")
gm_all <- globalMin.portfolio(er_all, as.matrix(covariance_matrix), 
                              shorts = TRUE)

gm_all_annual <- gm_all
gm_all_annual$er <- gm_all_annual$er * 12
gm_all_annual$sd <- gm_all_annual$sd * sqrt(12)
gm_all_annual

# tangency portfolio for all 7 stocks
tp_all <- tangency.portfolio(er_all, as.matrix(covariance_matrix), rf_1, 
                             shorts = TRUE)

tp_all_annual <- tp_all
tp_all_annual$er <- tp_all_annual$er * 12
tp_all_annual$sd <- tp_all_annual$sd * sqrt(12)
tp_all_annual

## 3c) Theoretical Question

## 3d) & 3e)

# compute the mean-variance efficient frontier
ef <- efficient.frontier(er_all, as.matrix(covariance_matrix), nport = 20, 
                         alpha.min = -5, alpha.max = 5, shorts = TRUE)

# plot mean-variance efficient frontier
plot(ef)
plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gm_all$sd, gm_all$er, col="green", pch=16, cex=2)
points(tp_all$sd, tp_all$er, col="red", pch=16, cex=2)
text(gm_all$sd, gm_all$er, labels="GLOBAL MIN", pos=2, col="green")
text(tp_all$sd, tp_all$er, labels="TANGENCY", pos=2, col="red")
sr.tan = (tp_all$er - rf_1)/tp_all$sd

# add capital market line
abline(a=rf_1, b=sr.tan, col="red", lwd=2)
