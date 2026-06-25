
# Bayesian Hurdle Model — Lag Comparison (2-weeks vs 1-Month vs 2-Month)

library(brms)
library(emmeans)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom.mixed)

CHAINS     <- 4
ITER       <- 4000
WARMUP     <- 1000
BAYES_SEED <- 1234


# DATASETS

df_2weeks <- read.csv("lag_2_weeks.csv", stringsAsFactors = FALSE)
df_1month <- read.csv("lag_1_month.csv", stringsAsFactors = FALSE)
df_2month <- read.csv("lag_2_months.csv", stringsAsFactors = FALSE)


# 2-WEEK LAG MODEL

fit_2weeks <- brm(
  bf(cong_t ~ news_lag2w + cong_lag2w, #count part
     hu ~ news_lag2w + cong_lag2w),  #zero part
  data    = df_2weeks,
  family  = hurdle_poisson(), #tells the model to combine those two pieces: a logistic model for zeros and a Poisson model for the positive counts.
  chains  = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent  = 2
)

print(summary(fit_2weeks))


# 1-MONTH LAG MODEL

fit_1month <- brm(
  bf(cong_t ~ news_lag1 + cong_lag1, #count part
     hu ~ news_lag1 + cong_lag1), #zero part
  data    = df_1month,
  family  = hurdle_poisson(), #tells the model to combine those two pieces: a logistic model for zeros and a Poisson model for the positive counts.
  chains  = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent  = 2
)

print(summary(fit_1month))

# 2-MONTH LAG MODEL

fit_2month <- brm(
  bf(cong_t ~ news_lag2 + cong_lag2, #count part
     hu ~ news_lag2 + cong_lag2), #zero part
  data    = df_2month,
  family  = hurdle_poisson(), #tells the model to combine those two pieces: a logistic model for zeros and a Poisson model for the positive counts.
  chains  = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent  = 2
)

print(summary(fit_2month))


