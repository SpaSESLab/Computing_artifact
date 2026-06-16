library(pscl)
library(MASS)

# load data
df <- read.csv("listing_lagged_300.csv", stringsAsFactors = FALSE)

#fitting hurdle model
m <- hurdle(cong_t ~ news_lag + cong_lag,
            data = df, 
            dist = "poisson",   # Part 2: zero-truncated Poisson - how many docs, given at least one?
            zero.dist = "binomial")  # Part 1: logistic - does Congress produce any listing doc this month?

summary(m)

# pull coefficients
cf <- coef(m)
#se <- sqrt(diag(vcov(m)))

# Part 1 coefficients (zero_ prefix in pscl)
g0 <- cf["zero_(Intercept)"]; g4 <- cf["zero_news_lag"]; g3 <- cf["zero_cong_lag"]

# Part 2 coefficients (count_ prefix in pscl)
b0 <- cf["count_(Intercept)"]; b4 <- cf["count_news_lag"]; b3 <- cf["count_cong_lag"]
