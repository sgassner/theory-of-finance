#------------------------------------------------------------------------------#
# Theory of Finance, 2022
# Problem Set 4
# Authors:
# - Sandro Gassner
#------------------------------------------------------------------------------#

# set working directory
setwd("~/Documents/R/TheoryOfFinance/assignment4")

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
data_1 <- read.csv("data_ps4-1.csv")
data_2 <- read.csv("data_ps4-2.csv", sep = ";")

# rename DATE column in data_2
data_2 <- data_2 %>% rename("Date" = "DATE")

# reformat date column
data_1$Date <- as.Date(format(as.Date(data_1$Date, "%m/%d/%Y"), "%Y-%m-%d"))
data_2$Date <- as.Date(format(as.Date(data_2$Date, "%d.%m.%Y"), "%Y-%m-%d"))

# sort by date
data_1 <- data_1 %>% arrange(ymd(data_1$Date))
data_2 <- data_2 %>% arrange(ymd(data_2$Date))

################################################################################
### Problem 1
################################################################################

# filter for yield curve per 2022-01-03 
yield_curve <- data_1 %>% filter(Date == "2022-01-03")

# divide yields by 100 to get decimal values
yield_curve[,2:13] <- yield_curve[,2:13] / 100

## 1a) ------------------------------------------------------------------------#

# calculate price of 3y zero-coupon bond with face value $100M
fv_bond_a <- 100000000
pv_bond_a <- fv_bond_a / (1 + yield_curve$X3.Yr)^3
pv_bond_a

## 1b) ------------------------------------------------------------------------#

# calculate FV of coupon paying bond ($5M yearly) with PV = p_1a
pv_bond_b <- pv_bond_a
c_bond_b <- 5000000
fv_bond_b <- (pv_bond_b - 
                (c_bond_b / (1 + yield_curve$X1.Yr)^1) -
                (c_bond_b / (1 + yield_curve$X2.Yr)^2) -
                (c_bond_b / (1 + yield_curve$X3.Yr)^3)) * (1 + yield_curve$X3.Yr)^3
fv_bond_b

## 1c) ------------------------------------------------------------------------#

# create cash flow vectors for the two bonds
cf_bond_a <- c(0, 0, fv_bond_a)
cf_bond_b <- c(c_bond_b, c_bond_b, (c_bond_b + fv_bond_b))

# create bond valuation function
bval_fun <- function(i, cf, t = seq(along = cf)) {
  sum(cf / (1 + i)^t)
} 

# set bond valuation function equal to 0 to solve for ytm with uniroot
bval_0_fun <- function(i, cf, pv) {
  bval_fun(i = i, cf = cf) - pv
} 

# create ytm() function using uniroot
ytm_fun <- function(cf, pv) {
  uniroot(bval_0_fun, c(0, 1), cf = cf, pv = pv)$root
}

# use ytm() function to find yields
ytm_bond_a <- ytm_fun(cf_bond_a, pv_bond_a)
ytm_bond_b <- ytm_fun(cf_bond_b, pv_bond_b)

# show rounded results (in %)
round(ytm_bond_a * 100, 4)
round(ytm_bond_b * 100, 4)

## 1d) ------------------------------------------------------------------------#

# calculate simple interest rates
ysimple_bond_a <- ((1 + ytm_bond_a)^3 - 1) / 3
ysimple_bond_b <- ((1 + ytm_bond_b)^3 - 1) / 3

# show simple interest rates (in %)
round(ysimple_bond_a * 100, 4)
round(ysimple_bond_b * 100, 4)

# calculate continuously compounded interest rates
ycontin_bond_a <- log(1 + ytm_bond_a)
ycontin_bond_b <- log(1 + ytm_bond_b)

# show continuously compounded interest rates (in %)
round(ycontin_bond_a * 100, 4)
round(ycontin_bond_b * 100, 4)

## 1e) ------------------------------------------------------------------------#

# create dollar duration function (notes 17.2, page 4)
dollar_duration_fun <- function(ytm, cf, t = seq(along = cf)) {
  dollar_duration <- (1 / (1 + ytm)) * sum(t * (cf / (1 + ytm)^t))
  return(dollar_duration)
} 

# calculate and show dollar durations
dollar_duration_bond_a <- dollar_duration_fun(ytm_bond_a, cf_bond_a)
dollar_duration_bond_b <- dollar_duration_fun(ytm_bond_b, cf_bond_b)
round(dollar_duration_bond_a, 4)
round(dollar_duration_bond_b, 4)

# calculate and show adjusted duration
adj_duration_bond_a <- dollar_duration_bond_a / pv_bond_a
adj_duration_bond_b <- dollar_duration_bond_b / pv_bond_b
round(adj_duration_bond_a, 4)
round(adj_duration_bond_b, 4)

# Macaulay duration
macaulay_duration_bond_a <- (dollar_duration_bond_a * 
                               (1 + ytm_bond_a)) / pv_bond_a
macaulay_duration_bond_b <- (dollar_duration_bond_b * 
                               (1 + ytm_bond_b)) / pv_bond_b
round(macaulay_duration_bond_a, 3)
round(macaulay_duration_bond_b, 4)

## NOTE: For Zero-Coupon Bonds the macaulay_duration is equal to the time to maturity!
## This shows, that our calculations are right!

## 1f) ------------------------------------------------------------------------#

# create data frame to store the results
df_1f <- data.frame(ytm = seq(0, 0.05, by = 0.001),
                    pv_bond_a = NA,
                    pv_bond_b = NA)

# run for loop to calculate bond prices for all ytm's
for (i in seq(0, 0.05, by = 0.001)) {
  df_1f$pv_bond_a[df_1f$ytm == i] <- bval_fun(i = i, cf = cf_bond_a)
  df_1f$pv_bond_b[df_1f$ytm == i] <- bval_fun(i = i, cf = cf_bond_b)
}

# convert df_1f to long format for plot
df_1f_long <- df_1f %>% pivot_longer(cols = c("pv_bond_a", "pv_bond_b"), 
               names_to = "bond", values_to = "price")

# create plot
ggplot(df_1f_long, aes(x = ytm, y = (price / 1000000), group = bond)) +
  geom_line(aes(color = bond)) +
  labs(title="Bond Prices for Different YTM", 
       x = "YTM", 
       y = "Bond Price (USD Mio.)")+
  theme(legend.position = "bottom")

## 1g) ------------------------------------------------------------------------#

# filter for prices of the zero-bond
df_1g <- df_1f[1:2]

# add columns for duration approximation according to (notes 7.2)
df_1g$delta_ytm <- NA
df_1g$pv_bond_a_approx <- NA

# calculate delta ytm compared to the current ytm of the zero-bond
for (i in 1:51) {
  df_1g$delta_ytm[i] <- df_1g$ytm[i] - ytm_bond_a
}

# add price estimation based on duration for all ytm's
for (i in 1:51) {
  df_1g$pv_bond_a_approx[i] <- 
    pv_bond_a + ((-1) * dollar_duration_bond_a * df_1g$delta_ytm[i])
}

# convert relevant columns to long format for the plot
df_1g_long <- df_1g %>% select(ytm, pv_bond_a, pv_bond_a_approx)
df_1g_long <- df_1g_long %>% 
  pivot_longer(cols = c("pv_bond_a", "pv_bond_a_approx"), 
               names_to = "calculation", values_to = "price")

# plot results
ggplot(df_1g_long, aes(x = ytm, y = (price / 1000000), group = calculation)) +
  geom_line(aes(color = calculation)) +
  geom_point(aes(x = ytm_bond_a, y = pv_bond_a/1000000), 
             colour="red", size = 2) +
  geom_vline(xintercept = 0.0179, linetype = "dashed", color = "darkgreen") +
  labs(title="Bond Prices: Exact Calculation vs. Approximation (LN 17.2)", 
       x = "YTM", 
       y = "Bond Price (USD Mio.)") +theme(legend.position = "bottom")

# calculate error of approximation for delta ytm +75 bps
true_delta_pv <- bval2_fun(ytm = (ytm_bond_a + 0.0075), cf = cf_bond_a) - 
  bval2_fun(ytm = ytm_bond_a, cf = cf_bond_a)
approx_delta_pv <- ((-1) * dollar_duration_bond_a * 0.0075)
diff_delta_pv <- approx_delta_pv - true_delta_pv
perc_diff_delta_pv <- round((diff_delta_pv / true_delta_pv) * 100, 1)

# print difference
cat("The duration approximation over-estimates the bond price reduction caused 
    by an interest rate hike of 75 bps by ", (-1) * diff_delta_pv, 
    "USD. This is equal to approximately ", perc_diff_delta_pv, " percent.")

################################################################################
### Problem 2
################################################################################

## 2a) ------------------------------------------------------------------------#

# theory question -> see answer sheet

## 2b) ------------------------------------------------------------------------#

# Naive Hedge
# create data frame for hedged portfolio values
df_naive_hedge <- data.frame(date = data_1$Date,
                             delta_date = NA,
                             yield_3y = data_1$X3.Yr / 100,
                             yield_2y = data_1$X2.Yr / 100,
                             m_duration_L = 3,
                             m_duration_H = 2,
                             P_L = NA,
                             P_H = NA,
                             v = NA,
                             vPH = NA,
                             delta_V = NA,
                             V = NA,
                             vPH_PL = NA)

# calculate durations of both instruments
df_naive_hedge$delta_date <- as.numeric(df_naive_hedge$date - 
                                          lag(df_naive_hedge$date))
for (i in 2:184) {
  days <- sum(na.omit(df_naive_hedge$delta_date[2:i]))
  df_naive_hedge$m_duration_L[i] <- 3 - (days / 365)
  df_naive_hedge$m_duration_H[i] <- 2 - (days / 365)
}

# calculate prices of both instruments
for (i in 1:184) {
  df_naive_hedge$P_L[i] <- 1 / 
    (1 + df_naive_hedge$yield_3y[i])^df_naive_hedge$m_duration_L[i]
  df_naive_hedge$P_H[i] <- 1 / 
    (1 + df_naive_hedge$yield_2y[i])^df_naive_hedge$m_duration_H[i]
}

# calculate weight of hedge v
for (i in 1:length(df_naive_hedge$date)) {
  # define function to solve in period i
  hedge_weight_fun <- function (v) {
    ((-1) * df_naive_hedge$P_L[i]) + (v * df_naive_hedge$P_H[i])
  }
  # store weight of hedge in period i
  df_naive_hedge$v[i] <- uniroot(hedge_weight_fun, c(-10, 10))$root
}

# caluclate vPH and vPH/PL
df_naive_hedge$vPH <- df_naive_hedge$v * df_naive_hedge$P_H
df_naive_hedge$vPH_PL <- df_naive_hedge$vPH / df_naive_hedge$P_L

# calculate delta_V
df_naive_hedge$delta_V[1] <- 0
for (i in 1:183) {
  df_naive_hedge$delta_V[i+1] <- 
    ((-1) * (df_naive_hedge$P_L[i+1] - df_naive_hedge$P_L[i])) +
    (df_naive_hedge$v[i] * (df_naive_hedge$P_H[i+1] - df_naive_hedge$P_H[i]))
}

# calculate portfolio value V
df_naive_hedge$V[1] <- 0
for (i in 1:183) {
  df_naive_hedge$V[i+1] <- df_naive_hedge$V[i] + df_naive_hedge$delta_V[i+1]
}

# plot portfolio value V over time
ggplot(df_naive_hedge, aes(x = date, y = V)) +
  geom_line() +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  labs(title = "Naive Hedge (Liability = 3y-Zero-Bond, Hedge = 2y-Zero-Bond)", 
       x = "Days", 
       y = "Portfolio Value (V)") +
  ylim(-0.03, 0.03)

## 2c) ------------------------------------------------------------------------#

# Duration Hedge
# create data frame for hedged portfolio values
df_duration_hedge <- data.frame(date = data_1$Date,
                                yield_3y = data_1$X3.Yr / 100,
                                yield_2y = data_1$X2.Yr / 100,
                                m_duration_L = 3,
                                m_duration_H = 2,
                                P_L = NA,
                                P_H = NA,
                                v = NA,
                                vPH = NA,
                                delta_V = NA,
                                V = NA,
                                vPH_PL = NA)

# calculate durations of both instruments
df_duration_hedge$delta_date <- as.numeric(df_duration_hedge$date - 
                                          lag(df_duration_hedge$date))
for (i in 2:184) {
  days <- sum(na.omit(df_duration_hedge$delta_date[2:i]))
  df_duration_hedge$m_duration_L[i] <- 3 - (days / 365)
  df_duration_hedge$m_duration_H[i] <- 2 - (days / 365)
}

# calculate prices of both instruments
for (i in 1:184) {
  df_duration_hedge$P_L[i] <- 1 / 
    (1 + df_duration_hedge$yield_3y[i])^df_duration_hedge$m_duration_L[i]
  df_duration_hedge$P_H[i] <- 1 / 
    (1 + df_duration_hedge$yield_2y[i])^df_duration_hedge$m_duration_H[i]
}

# calculate weight of the hedge v
for (i in 1:184){
  df_duration_hedge$v[i] <- 
    (df_duration_hedge$m_duration_L[i] / df_duration_hedge$m_duration_H[i]) * 
    (df_duration_hedge$P_L[i] / df_duration_hedge$P_H[i])
}

# caluclate vPH and vPH/PL
df_duration_hedge$vPH <- df_duration_hedge$v * df_duration_hedge$P_H
df_duration_hedge$vPH_PL <- df_duration_hedge$vPH / df_duration_hedge$P_L

# calculate delta_V
df_duration_hedge$delta_V[1] <- 0
for (i in 1:183) {
  df_duration_hedge$delta_V[i+1] <- 
    ((-1) * (df_duration_hedge$P_L[i+1] - df_duration_hedge$P_L[i])) +
    (df_duration_hedge$v[i] * (df_duration_hedge$P_H[i+1] - 
                                 df_duration_hedge$P_H[i]))
}

# calculate portfolio value V
df_duration_hedge$V[1] <- 0
for (i in 1:183) {
  df_duration_hedge$V[i+1] <- df_duration_hedge$V[i] + 
    df_duration_hedge$delta_V[i+1]
}

# plot portfolio value V over time
ggplot(df_duration_hedge, aes(x = date, y = V)) +
  geom_line() +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  labs(title = "Duration Hedge (Liability = 3y-Zero-Bond, Hedge = 2y-Zero-Bond)", 
       x = "Date", 
       y = "Portfolio Value (V)")+
  ylim(-0.03, 0.03)

## 2d) ------------------------------------------------------------------------#

# compute sd's of daily change in V for both hedging methods
round(sd(df_naive_hedge$delta_V), 6)
round(sd(df_duration_hedge$delta_V), 6)

################################################################################
### Problem 3
################################################################################

## 3a) ------------------------------------------------------------------------#

# compute and store level, slope and curvature for the given yield curves
df_empirical <- data.frame(date = data_1$Date,
                           level = data_1$X10.Yr,
                           slope = data_1$X10.Yr - data_1$X3.Mo,
                           curvature = ((data_1$X2.Yr - data_1$X3.Mo) - 
                                          (data_1$X10.Yr - data_1$X2.Yr)))

# convert to long data frame for plot
df_empirical_long <- df_empirical %>% 
  pivot_longer(cols = c("level", "slope", "curvature"), 
               names_to = "property", values_to = "value")

# plot empirical properties
ggplot(df_empirical_long, aes(x = date, y = value, group = property)) +
  geom_line(aes(color = property)) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  labs(title = "Empirical Properties of Yield Curves (2022)", 
       x = "Month", 
       y = "Value  (in %)") +
  theme(legend.position = "bottom")


## 3b) ------------------------------------------------------------------------#

# compute changes in empirical properties
df_empirical$delta_level <- df_empirical$level - lag(df_empirical$level)
df_empirical$delta_slope <- df_empirical$slope - lag(df_empirical$slope)
df_empirical$delta_curvature <- df_empirical$curvature - lag(df_empirical$curvature)

# perform correlation analysis
cor_empirical <- cor(na.omit(df_empirical[5:7]))

# show results
kable(cor_empirical, digits = 4, format = "simple", 
      caption = "Correlations of Changes in Empirical Yield Properties")

## 3c) ------------------------------------------------------------------------#

# fills NA's for IRP's using the previous entries
# data_2 <- data_2 %>% fill(IRP)

# create table for risk premia
risk_premia <- data.frame(date = data_1$Date,
                          term_rp = data_1$X10.Yr,
                          e_inflation_rp = data_2$IRP,
                          default_rp = data_2$corp_BBB_1Y - data_1$X1.Yr)

# perform correlation analysis
cor_risk_premia <- cor(na.omit(risk_premia[2:4]))

# show results
kable(cor_risk_premia, digits = 4, format = "simple", 
      caption = "Correlations of Different Risk Premia")

# NOTE: We only have one observation of the IRP per month! (9 in total)!
