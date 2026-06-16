library(pscl)
library(MASS)

df <- read.csv("listing_lagged_300.csv", stringsAsFactors = FALSE)

#fitting hurdle model
m <- hurdle(cong_t ~ news_lag + cong_lag,
            data = df, 
            dist = "poisson",   # Part 2: zero-truncated Poisson - how many docs, given at least one?
            zero.dist = "binomial")  # Part 1: logistic - does Congress produce any listing doc this month?

summary(m)

# coefficients
cf <- coef(m)

#Part 1 coefficients
g0 <- cf["zero_(Intercept)"]; g4 <- cf["zero_news_lag"]; g3 <- cf["zero_cong_lag"]

#Part 2 coefficients
b0 <- cf["count_(Intercept)"]; b4 <- cf["count_news_lag"]; b3 <- cf["count_cong_lag"]



# Part 1 threshold probabilities for combinations in my data
combos <- unique(df[, c("news_lag", "cong_lag")])
combos <- combos[order(combos$news_lag, combos$cong_lag), ]

combos$n      <- mapply(function(nl, cl) sum(df$news_lag == nl & df$cong_lag == cl),
                        combos$news_lag, combos$cong_lag)
combos$eta    <- g0 + g4 * combos$news_lag + g3 * combos$cong_lag
combos$p_act  <- plogis(combos$eta)
combos$p_sil  <- 1 - combos$p_act

print(combos)