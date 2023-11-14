#------------------------------------------------------------------------------#
# Theory of Finance, 2022
# Problem Set 3
# Authors:
# - Sandro Gassner
#------------------------------------------------------------------------------#

# set working directory
setwd("~/Documents/R/TheoryOfFinance/assignment3")

# load packages
# install.packages("rootSolve")
# install.packages("PerformanceAnalytics")
library(tidyverse)
library(lubridate)
library(rootSolve)
library(PerformanceAnalytics)
library(knitr)
library(corrplot)

# load data
data_all_themes_ew <- read.csv("ToF_2022_data_PS-3_all_themes_ew.csv")
data_all_themes_vw <- read.csv("ToF_2022_data_PS-3_all_themes_vw.csv")
data_market <- read.csv("ToF_2022_data_PS-3_market.csv")

# reformat date column
data_all_themes_ew$date <- as_date(data_all_themes_ew$date)
data_all_themes_vw$date <- as_date(data_all_themes_vw$date)
data_market$date <- as_date(data_market$date)

# sort by date
data_all_themes_ew <- data_all_themes_ew %>% arrange(ymd(data_all_themes_ew$date))
data_all_themes_vw <- data_all_themes_vw %>% arrange(ymd(data_all_themes_vw$date))
data_market <- data_market %>% arrange(ymd(data_market$date))

#------------------------------------------------------------------------------#
# Problem 1
#------------------------------------------------------------------------------#

## 1a)

# define utility function with k = 1.5
utility_function <- function(w){((-w^(1.2-1.5))/(1-1.5))^(-1)}

# create data frame for ggplot plot axis
df_axis <- data.frame(x = c(0.1, 3))

# plot function
ggplot(df_axis, aes(x = x)) + 
  stat_function(fun = utility_function) +
  geom_point(aes(x = 1, y = utility_function(1)), colour = "red", size = 2) +
  geom_segment(x = 1, y = utility_function(1), xend = 1, yend = 0,
               colour = "red", linetype = "dashed") +
  geom_segment(x = 1, y = utility_function(1), xend = 0, 
               yend = utility_function(1), colour = "red", 
               linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  labs(title = "Utility Curve of the Decision-Maker", 
       x = "W", 
       y = "U(W)")

## 1b)

# define lottery function to calculate expected wealth gain
lottery_function <- function(p){
  -0.5 + (p * 1.3) + ((1 - p) * 0.1)
}

# define lottery function to calculate expected utility
lottery_function_utility <- function(p){
  p * utility_function(1 - 0.5 + 1.3) + (1-p) * utility_function(1 - 0.5 + 0.1)
}

# create data frame to store utility levels for different p's
utility_p <- data.frame(p = seq(0, 1, by = 0.01),
                        w_expected = NA,
                        u_expected = NA,
                        expected_u = NA)

# calculate utility for different p's
for (p in seq(0, 1, by = 0.01)){
  w_expected <- 1 + lottery_function(p)
  utility_p$w_expected[utility_p$p == p] <- w_expected
  utility_p$u_expected[utility_p$p == p] <- utility_function(w_expected)
  utility_p$expected_u[utility_p$p == p] <- lottery_function_utility(p)
}

# add utility levels to the plot
ggplot(data = NULL, aes(x = x)) + 
  stat_function(data = df_axis, fun = utility_function) +
  geom_line(data = utility_p, aes(x = w_expected, y = expected_u), 
            color = "green", size = 1) +
  geom_point(data = df_axis, aes(x = 1, y = utility_function(1)), 
             colour = "red", size = 2) +
  geom_segment(data = df_axis, x = 1, y = utility_function(1), xend = 1, 
               yend = 0, colour = "red", linetype = "dashed") +
  geom_segment(data = df_axis, x = 1, y = utility_function(1), xend = 0, 
               yend = utility_function(1), colour = "red", 
               linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  labs(title = "Utility Curve of the Decision-Maker", 
       x = "W", 
       y = "U(W)")

## 1c)

# set up the indifference function
indifference_function = function(p) {
  # utility of investing = utility of outside-option
  lottery_function_utility(p) - utility_function(1)
} 

# solve the indifference function and show rounded result
p_indifferent <- uniroot.all(indifference_function, lower = 0, upper = 1)
round(p_indifferent, 4)

## 1d)

# expected value of wealth with the calculated p
w_expected <- 1 + lottery_function(p_indifferent)
round(w_expected, 4)

# add expected value of wealth to the plot
ggplot(data = NULL, aes(x = x)) + 
  stat_function(data = df_axis, fun = utility_function) +
  geom_line(data = utility_p, aes(x = w_expected, y = expected_u), 
            color = "green", size = 1) +
  geom_point(data = df_axis, aes(x = 1, y = utility_function(1)), 
             colour = "red", size = 2) +
  geom_segment(data = df_axis, x = 1, y = utility_function(1), xend = 1, 
               yend = 0, colour = "red", linetype = "dashed") +
  geom_segment(data = df_axis, x = 1, y = utility_function(1), xend = 0, 
               yend = utility_function(1), colour = "red", 
               linetype = "dashed") +
  geom_point(data = df_axis, aes(x = w_expected, y = utility_function(1)), 
             colour = "blue", size = 2) +
  geom_segment(data = df_axis, x = w_expected, y = utility_function(1), 
               xend = w_expected, yend = 0, colour = "blue", 
               linetype = "dashed") +
  geom_segment(data = df_axis, x = w_expected, y = utility_function(1), xend = 0, 
               yend = utility_function(1), colour = "blue", 
               linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  labs(title = "Utility Curve of the Decision-Maker", 
       x = "W", 
       y = "U(W)")

## 1e)

# define lottery function to calculate expected wealth gain
lottery_function_w <- function(w){
  1 - w + 0.6 * (1.4 * w) + 0.4 * (0.6 * w)
}

# define lottery function to calculate expected utility
lottery_function_utility_w <- function(w){
  0.6 * utility_function(1 - w + 1.4 * w) + 
    0.4 * utility_function(1 - w + 0.6 * w)
}

# create data frame to store utility levels for different w's
utility_w <- data.frame(w = seq(0, 1, by = 0.0001),
                        w_expected = NA,
                        u_expected = NA,
                        expected_u = NA)

# calculate utility for different p's
for (w in seq(0, 1, by = 0.0001)){
  w_expected <- lottery_function_w(w)
  utility_w$w_expected[utility_w$w == w] <- w_expected
  utility_w$u_expected[utility_w$w == w] <- utility_function(w_expected)
  utility_w$expected_u[utility_w$w == w] <- lottery_function_utility_w(w)
}

# plot y = expected utility, x = weight
ggplot(utility_w, aes(x = w, y = expected_u)) +
  geom_line() +
  labs(title="Expected Utility by Weight of Investment", 
       x = "Weight", 
       y = "Expected Utility")

## 1f)

# show weight that maximizes expected utility
w_optimum <- utility_w$w[which.max(utility_w$expected_u)]
u_optimum <- utility_w$expected_u[which.max(utility_w$expected_u)]
round(w_optimum, 4)
round(u_optimum, 4)

# highlight optimal weight in plot
ggplot(utility_w, aes(x = w, y = expected_u)) +
  geom_line() +
  geom_point(x = w_optimum, y = u_optimum, colour = "red", size = 2) +
  geom_segment(x = w_optimum, y = u_optimum, xend = w_optimum, 
               yend = 0, colour = "red", linetype = "dashed") +
  geom_segment(x = w_optimum, y = u_optimum, xend = 0, 
               yend = u_optimum, colour = "red", linetype = "dashed") +
  labs(title="Expected Utility by Weight of Investment", 
       x = "Weight", 
       y = "Expected Utility")

#------------------------------------------------------------------------------#
# Problem 2, Part I
#------------------------------------------------------------------------------#

## 2a)

# add market excess returns to ew and vw data frame
data_all_themes_ew <- cbind(data_all_themes_ew, market = data_market$Mkt.RF)
data_all_themes_vw <- cbind(data_all_themes_vw, market = data_market$Mkt.RF)

# define factors
factors <- c("investment", "low_risk", "momentum", "profitability", 
             "short_term_reversal", "size", "value")

# create data frames to store results
statistics_ew <- data.frame(factor = factors, annual_return = NA, 
                            annual_sd = NA, sharpe_ratio = NA)
statistics_vw <- data.frame(factor = factors, annual_return = NA, 
                            annual_sd = NA, sharpe_ratio = NA)

# calculate annual excess returns and sd's for all factor portfolios
for (i in factors) {
  statistics_ew$annual_return[statistics_ew$factor == i] <- mean(data_all_themes_ew[[i]]) * 12
  statistics_vw$annual_return[statistics_vw$factor == i] <- mean(data_all_themes_vw[[i]]) * 12
  statistics_ew$annual_sd[statistics_ew$factor == i] <- sd(data_all_themes_ew[[i]]) * sqrt(12)
  statistics_vw$annual_sd[statistics_vw$factor == i] <- sd(data_all_themes_vw[[i]]) * sqrt(12)
}

# calculate sharpe ratio for all factor portfolios
statistics_ew$sharpe_ratio <- statistics_ew$annual_return / statistics_ew$annual_sd *12
statistics_vw$sharpe_ratio <- statistics_vw$annual_return / statistics_vw$annual_sd

# show tables
kable(statistics_ew, digits = 4, caption = "Equal-Weighted Portfolios", 
      format = "simple")
kable(statistics_vw, digits = 4, caption = "Value-Weighted Portfolios", 
      format = "simple")

## 2b)

# extend tables from 2a) with alpha, beta and p-value
statistics_ew$alpha <- NA
statistics_ew$p_value_alpha <- NA
statistics_ew$beta <- NA
statistics_ew$p_value_beta <- NA
statistics_vw$alpha <- NA
statistics_vw$p_value_alpha <- NA
statistics_vw$beta <- NA
statistics_vw$p_value_beta <- NA

# run CAPM for each factor portfolio and store coefficients
for (i in factors) {
  CAPM_ew <- lm(formula = paste0(i, " ~ market"), data = data_all_themes_ew)
  CAPM_vw <- lm(formula = paste0(i, " ~ market"), data = data_all_themes_vw)
  statistics_ew$alpha[statistics_ew$factor == i] <- CAPM_ew[["coefficients"]][["(Intercept)"]] * 12
  statistics_vw$alpha[statistics_vw$factor == i] <- CAPM_vw[["coefficients"]][["(Intercept)"]] * 12
  statistics_ew$beta[statistics_ew$factor == i] <- CAPM_ew[["coefficients"]][["market"]]
  statistics_vw$beta[statistics_vw$factor == i] <- CAPM_vw[["coefficients"]][["market"]]
  statistics_ew$p_value_alpha[statistics_ew$factor == i] <- summary(CAPM_ew)$coefficients[1,4]
  statistics_vw$p_value_alpha[statistics_vw$factor == i] <- summary(CAPM_vw)$coefficients[1,4]
  statistics_ew$p_value_beta[statistics_ew$factor == i] <- summary(CAPM_ew)$coefficients[2,4]
  statistics_vw$p_value_beta[statistics_vw$factor == i] <- summary(CAPM_vw)$coefficients[2,4]
}

# show extended tables
kable(statistics_ew, digits = 4, caption = "Equal-Weighted Portfolios", 
      format = "simple")
kable(statistics_vw, digits = 4, caption = "Value-Weighted Portfolios",
      format = "simple")

## 2c) interpretation of results

## 2d)

# prepare data frame to plot excess returns
excess_factor_returns <- data.frame(factor = factors,
                                    alpha_ew = statistics_ew[["alpha"]],
                                    p_value_alpha_ew = statistics_ew[["p_value_alpha"]],
                                    alpha_vw = statistics_vw[["alpha"]],
                                    p_value_alpha_vw = statistics_vw[["p_value_alpha"]])

# remove results below 95% significance
excess_factor_returns$alpha_ew[excess_factor_returns$p_value_alpha_ew > 0.05] <- NA
excess_factor_returns$alpha_vw[excess_factor_returns$p_value_alpha_vw > 0.05] <- NA

# remove p_value and convert to long format
excess_factor_returns <- excess_factor_returns %>% 
  select(c("factor", "alpha_ew", "alpha_vw"))
excess_factor_returns_long <- excess_factor_returns %>% 
  pivot_longer(cols = c("alpha_ew", "alpha_vw"), 
               names_to = "weighting", values_to = "alpha")

# plot CAPM excess returns of factor models
ggplot(excess_factor_returns_long, 
       aes(fill = weighting, y = alpha, x = factor)) + 
  geom_bar(position="dodge", stat="identity", width = 0.5) +
  labs(title="CAPM Excess Return of Factor Portfolios", 
       x = NA, 
       y = "Alpha (annualized)",
       fill= "Weighting") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank())

## 2e)

# create data frames to store coefficients
size_coef_ew <- data.frame(factor = factors, alpha = NA, 
                           beta_m = NA, beta_size = NA, p_value_size = NA)
size_coef_vw <- data.frame(factor = factors, alpha = NA, 
                           beta_m = NA, beta_size = NA, p_value_size = NA)

# add equal equal weighted size factor to value weighted data
data_all_themes_vw$size_ew <- data_all_themes_ew$size

# run CAPM with equal weighted size factor and store coefficients
for (i in factors[factors !="size"]) {
  CAPM_ew <- lm(formula = paste0(i, " ~ market + size"), data = data_all_themes_ew)
  CAPM_vw <- lm(formula = paste0(i, " ~ market + size_ew"), data = data_all_themes_vw)
  size_coef_ew$alpha[size_coef_ew$factor == i] <- CAPM_ew[["coefficients"]][["(Intercept)"]] * 12
  size_coef_vw$alpha[size_coef_vw$factor == i] <- CAPM_vw[["coefficients"]][["(Intercept)"]] * 12
  size_coef_ew$beta_m[size_coef_ew$factor == i] <- CAPM_ew[["coefficients"]][["market"]]
  size_coef_vw$beta_m[size_coef_vw$factor == i] <- CAPM_vw[["coefficients"]][["market"]]
  size_coef_ew$beta_size[size_coef_ew$factor == i] <- CAPM_ew[["coefficients"]][["size"]]
  size_coef_vw$beta_size[size_coef_vw$factor == i] <- CAPM_vw[["coefficients"]][["size_ew"]]
  size_coef_ew$p_value_size[size_coef_ew$factor == i] <- summary(CAPM_ew)$coefficients[3,4]
  size_coef_vw$p_value_size[size_coef_vw$factor == i] <- summary(CAPM_vw)$coefficients[3,4]
}

# add coefficients for size_ew to size_coef_vw
CAPM_size_vw <- lm(size ~ market + size_ew, data = data_all_themes_vw)
size_coef_vw$alpha[size_coef_vw$factor == "size"] <- CAPM_size_vw[["coefficients"]][["(Intercept)"]] * 12
size_coef_vw$beta_m[size_coef_vw$factor == "size"] <- CAPM_size_vw[["coefficients"]][["market"]]
size_coef_vw$beta_size[size_coef_vw$factor == "size"] <- CAPM_size_vw[["coefficients"]][["size_ew"]]
size_coef_vw$p_value_size[size_coef_vw$factor == "size"] <- summary(CAPM_size_vw)$coefficients[3,4]

# prepare data frame to plot excess returns
df_size_coef <- data.frame(factor = factors,
                           beta_size_ew = size_coef_ew[["beta_size"]],
                           p_value_size_ew = size_coef_ew[["p_value_size"]],
                           beta_size_vw = size_coef_vw[["beta_size"]],
                           p_value_size_vw = size_coef_vw[["p_value_size"]])

# remove results below 95% significance
df_size_coef$beta_size_ew[df_size_coef$p_value_size_ew > 0.05] <- NA
df_size_coef$beta_size_vw[df_size_coef$p_value_size_vw > 0.05] <- NA

# remove p_value and convert to long format
df_size_coef <- df_size_coef %>% 
  select(-c("p_value_size_ew", "p_value_size_vw"))
df_size_coef_long <- df_size_coef %>% 
  pivot_longer(cols = c("beta_size_ew", "beta_size_vw"), 
               names_to = "weighting", values_to = "beta_size")

# plot CAPM excess returns of factor models
ggplot(df_size_coef_long, 
       aes(fill = weighting, y = beta_size, x = factor)) + 
  geom_bar(position="dodge", stat="identity", width = 0.5) +
  labs(title="CAPM Betas for Size Factor", 
       x = NA, 
       y = "beta_size",
       fill= "Weighting") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank())

#------------------------------------------------------------------------------#
# Problem 2, Part II
#------------------------------------------------------------------------------#

## 2f)

# compute correlations of value weighted portfolios
cor <- cor(data_all_themes_vw[2:9])

# create correlation plot
corrplot(cor, title = "Correlation of Value-Weighted Portfolios",
         mar = c(2, 2, 2, 2))

## 2g)

# subset data_all_themes_vw for the time-horizon asked for
data_rolling <- data_all_themes_vw[c("date", "low_risk", "value", "size", 
                                     "profitability")][39:842,]
# reset row indexes
rownames(data_rolling) <- NULL

# create data frame to store rolling coefficients
data_rolling$alpha <- NA
data_rolling$beta_value <- NA
data_rolling$beta_size <- NA
data_rolling$beta_profitability <- NA
data_rolling$p_value_alpha <- NA

# run rolling window regression 
for (i in 1:744) {
  roll_reg <- lm(low_risk ~ value + size + profitability, 
                 data = data_rolling[i:(i+59),])
  data_rolling$alpha[i+60] <- roll_reg[["coefficients"]][["(Intercept)"]]
  data_rolling$beta_value[i+60] <- roll_reg[["coefficients"]][["value"]]
  data_rolling$beta_size[i+60] <- roll_reg[["coefficients"]][["size"]]
  data_rolling$beta_profitability[i+60] <- roll_reg[["coefficients"]][["profitability"]]
  data_rolling$p_value_alpha[i+60] <- summary(roll_reg)$coefficients[1,4]
}

# remove rows with NA's
data_rolling <- na.omit(data_rolling)

# head coefficients
kable(head(data_rolling[,6:10], 5), digits = 4, format = "simple", 
      caption = "Illustration: Rolling-Window Regression Coefficients of the first 5 Months")

# report mean alpha and mean annual alpha
round(mean(data_rolling$alpha), 6)
round(mean(data_rolling$alpha) * 12, 4)

# count how many times alpha is statistically significant at 95%
length(data_rolling$p_value_alpha[data_rolling$p_value_alpha <= 0.05])

## 2h)

# create column for portfolio price 
data_rolling$price_portfolio <- 1

# calculate prices for t+n
for (i in 1:743){
    data_rolling$price_portfolio[i+1] <- data_rolling$price_portfolio[i] * 
      (1 + (data_rolling$beta_value[i] * data_rolling$value[i] +
              data_rolling$beta_size[i] * data_rolling$size[i] + 
              data_rolling$beta_profitability[i] * data_rolling$profitability[i]))
}

# calculate returns of portfolio
data_rolling <- data_rolling %>% 
  mutate(r_portfolio = (price_portfolio - lag(price_portfolio)) / 
           lag(price_portfolio))

# calculate mean annualized return and sd of portfolio
portfolio_statistics <- data.frame(portfolio = c("value_size_profitability", "low_risk"),
                                   annual_return = NA,
                                   annual_sd = NA)
portfolio_statistics$annual_return[1] <- mean(data_rolling$r_portfolio, na.rm = TRUE) * 12
portfolio_statistics$annual_return[2] <- mean(data_rolling$low_risk, na.rm = TRUE) * 12
portfolio_statistics$annual_sd[1] <- sd(data_rolling$r_portfolio, na.rm = TRUE) * sqrt(12)
portfolio_statistics$annual_sd[2] <- sd(data_rolling$low_risk, na.rm = TRUE) * sqrt(12)

# show portfolio statistics
kable(portfolio_statistics, digits = 4, format = "simple", 
      caption = "Portfolio Statistics")

# report correlation of the two return series
cor(data_rolling$low_risk[1:743], data_rolling$r_portfolio[2:744])

# calculate price development of the low risk portfolio
data_rolling$price_low_risk <- 1
for (i in 1:743){
  data_rolling$price_low_risk[i+1] <- data_rolling$price_low_risk[i] * 
    (1 + data_rolling$low_risk[i])
}

# prepare data frame for portfolio prices plot
portfolio_prices <- data_rolling[c("date", "price_portfolio", "price_low_risk")]
colnames(portfolio_prices) <- c('date','value_size_profitability','low_risk')
portfolio_prices <- portfolio_prices %>% 
  pivot_longer(cols = c("value_size_profitability", "low_risk"), 
               names_to = "portfolio", values_to = "price")

# plot portfolio price development
ggplot(portfolio_prices, aes(x = date, y = price, group = portfolio)) +
  geom_line(aes(color = portfolio)) +
  labs(title="Portfolio Price Developements", 
       x = "Date", 
       y = "Portfolio Price")+
  theme(legend.position = "bottom")

