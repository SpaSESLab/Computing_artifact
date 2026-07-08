
# Bayesian Hurdle Model — News -> Congress direction
# Lag Comparison (2-weeks vs 1-Month vs 2-Month)


library(brms)
library(emmeans)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom.mixed)
library(ggplot2)

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


#gives line graphs -- 10 posterior draws to display
#A density check :the tails of the y_rep behave with the y

#pp_check(fit_2weeks, type = "dens_overlay", ndraws = 10)
#pp_check(fit_1month, type = "dens_overlay", ndraws = 10)
#pp_check(fit_2month, type = "dens_overlay", ndraws = 10)

library(gridExtra)
library(ggplot2)

make_ppc <- function(fit, title_text) {
  pp_check(fit, type = "dens_overlay", ndraws = 10) +
    ggtitle(title_text) +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    coord_cartesian(xlim = c(0, 10)) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      plot.margin = margin(4, 4, 4, 4)
    )
}

p1 <- make_ppc(fit_2weeks, "2-Week Model")
p2 <- make_ppc(fit_1month, "1-Month Model")
p3 <- make_ppc(fit_2month, "2-Month Model")

png("ppc_three_plots.png", width = 1500, height = 320, res = 150)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()



# pp_check(fit_2weeks, type = "bars", ndraws = 10) +
#   ggtitle("Posterior Predictive Bar Plot: 2-Week Model")


# pp_check(fit_1month, type = "bars", ndraws = 10) +
#   ggtitle("Posterior Predictive Bar Plot: 1-Month Model")
  

# pp_check(fit_2month, type = "bars", ndraws = 10) +
#   ggtitle("Posterior Predictive Bar Plot: 2-Months Model")


library(gridExtra)
library(ggplot2)

make_ppc_bars <- function(fit, title_text) {
  pp_check(fit, type = "bars", ndraws = 10) +
    ggtitle(title_text) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      plot.margin = margin(4, 4, 4, 4)
    )
}

p1 <- make_ppc_bars(fit_2weeks, "Posterior Predictive Bar Plot: 2-Week Model")
p2 <- make_ppc_bars(fit_1month, "Posterior Predictive Bar Plot: 1-Month Model")
p3 <- make_ppc_bars(fit_2month, "Posterior Predictive Bar Plot: 2-Month Model")

png("ppc_bars_three.png", width = 1500, height = 320, res = 150)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()


###################

#gives the histograms
# print(plot(fit_2weeks))
# print(plot(fit_1month))
# print(plot(fit_2month))

library(brms)
library(ggplot2)
library(patchwork)

p1 <- plot(fit_2weeks) +
  ggtitle("2-Week Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

p2 <- plot(fit_1month) +
  ggtitle("1-Month Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

p3 <- plot(fit_2month) +
  ggtitle("2-Month Model") +s
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

combined <- p1 | p2 | p3
combined

#checking the simulated 100 draws
# # Simulate replicated outcomes from the fitted model
# yrep_2weeks <- posterior_predict(fit_2weeks, draws = 100)
# yrep_1month <- posterior_predict(fit_1month, draws = 100)
# yrep_2month <- posterior_predict(fit_2month, draws = 100)

# dim(yrep_2weeks)
# head(yrep_2weeks[, 1:10])


# #Compare observed vs replicated data
# #First, save the observed response vectors

# y_obs_2weeks <- df_2weeks$cong_t
# y_obs_1month <- df_1month$cong_t
# y_obs_2month <- df_2month$cong_t

# #Then you can compare observed and replicated summaries.

# #Zero counts

# obs_zeros_2weeks <- sum(y_obs_2weeks == 0)
# obs_zeros_1month  <- sum(y_obs_1month == 0)
# obs_zeros_2month  <- sum(y_obs_2month == 0)

# rep_zeros_2weeks <- apply(yrep_2weeks, 1, function(x) sum(x == 0))
# rep_zeros_1month <- apply(yrep_1month, 1, function(x) sum(x == 0))
# rep_zeros_2month <- apply(yrep_2month, 1, function(x) sum(x == 0))

# mean(rep_zeros_2weeks)
# mean(rep_zeros_1month)
# mean(rep_zeros_2month)


# #Small counts

# obs_small_2weeks <- sum(y_obs_2weeks <= 2)
# obs_small_1month  <- sum(y_obs_1month <= 2)
# obs_small_2month  <- sum(y_obs_2month <= 2)

# rep_small_2weeks <- apply(yrep_2weeks, 1, function(x) sum(x <= 2))
# rep_small_1month <- apply(yrep_1month, 1, function(x) sum(x <= 2))
# rep_small_2month <- apply(yrep_2month, 1, function(x) sum(x <= 2))

# mean(rep_small_2weeks)
# mean(rep_small_1month)
# mean(rep_small_2month)

# #Spread and tail

# obs_sd_2weeks <- sd(y_obs_2weeks)
# obs_sd_1month  <- sd(y_obs_1month)
# obs_sd_2month  <- sd(y_obs_2month)

# rep_sd_2weeks <- apply(yrep_2weeks, 1, sd)
# rep_sd_1month  <- apply(yrep_1month, 1, sd)
# rep_sd_2month  <- apply(yrep_2month, 1, sd)

# mean(rep_sd_2weeks)
# mean(rep_sd_1month)
# mean(rep_sd_2month)

# #For the right tail:

# obs_max_2weeks <- max(y_obs_2weeks)
# obs_max_1month  <- max(y_obs_1month)
# obs_max_2month  <- max(y_obs_2month)

# rep_max_2weeks <- apply(yrep_2weeks, 1, max)
# rep_max_1month  <- apply(yrep_1month, 1, max)
# rep_max_2month  <- apply(yrep_2month, 1, max)

# mean(rep_max_2weeks)
# mean(rep_max_1month)
# mean(rep_max_2month)


# #gives the boxplot
# print(pp_check(fit_2weeks, type = "dens_overlay",ndraws = 100))
# print(pp_check(fit_2weeks, type = "hist",ndraws = 100))
# print(pp_check(fit_2weeks, type = "bars",ndraws = 100))


# #summary(fit_2weeks)
# #summary(fit_1month)
# #summary(fit_2month)


# #gives the histograms
# #print(plot(fit_2weeks))
# #print(plot(fit_1month))
# #print(plot(fit_2month))

# #gives line graphs
# #print(pp_check(fit_2weeks))  
# #print(pp_check(fit_1month))
# #print(pp_check(fit_2month))

# #gives the boxplot
# #print(pp_check(fit_2weeks, type = "dens_overlay",ndraws = 100))
# #print(pp_check(fit_2weeks, type = "hist",ndraws = 100))
# #print(pp_check(fit_2weeks, type = "bars",ndraws = 100))

# #checking zero counts

# zero_summary <- data.frame(
#   model = c("2-week", "1-month", "2-month"),
#   observed = c(obs_zeros_2weeks, obs_zeros_1month, obs_zeros_2month),
#   replicated_mean = c(mean(rep_zeros_2weeks), mean(rep_zeros_1month), mean(rep_zeros_2month))
# )

# p <- ggplot(zero_summary, aes(x = model)) +
#   geom_point(aes(y = observed), size = 3, color = "black") +
#   geom_point(aes(y = replicated_mean), size = 3, color = "blue") +
#   labs(x = "Model", y = "Number of zeros",
#        title = "Observed vs Posterior Predictive Zero Counts") +
#   theme_minimal()

# print(p)
# ggsave("zero_counts_ppc.png", plot = p, width = 8, height = 6, dpi = 300)


# obs_freq_2weeks <- as.data.frame(table(y_obs_2weeks))
# names(obs_freq_2weeks) <- c("count", "observed")
# obs_freq_2weeks$count <- as.numeric(as.character(obs_freq_2weeks$count))

# rep_freq_2weeks <- apply(yrep_2weeks, 1, function(x) table(factor(x, levels = 0:max(y_obs_2weeks))))
# rep_mean_2weeks <- rowMeans(as.matrix(rep_freq_2weeks))

# root_df_2weeks <- data.frame(
#   count = 0:max(y_obs_2weeks),
#   observed = as.numeric(table(factor(y_obs_2weeks, levels = 0:max(y_obs_2weeks)))),
#   expected = rep_mean_2weeks
# )

# f <- ggplot(root_df_2weeks, aes(x = count)) +
#   geom_col(aes(y = observed), fill = "steelblue", alpha = 0.7) +
#   geom_line(aes(y = expected), color = "red", linewidth = 1) +
#   geom_point(aes(y = expected), color = "red", size = 2) +
#   labs(x = "Count", y = "Frequency",
#        title = "Observed vs Posterior Predictive Frequencies") +
#   theme_minimal()

# print(f)
# ggsave("zero_counts_ppc_2.png", plot = f, width = 8, height = 6, dpi = 300)