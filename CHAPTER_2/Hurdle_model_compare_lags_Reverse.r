# Bayesian Hurdle Model  for Congress -> News Direction

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
df_2weeks <- read.csv("reverse_lag_2_weeks.csv", stringsAsFactors = FALSE)
df_1month <- read.csv("reverse_lag_1_month.csv", stringsAsFactors = FALSE)
df_2month <- read.csv("reverse_lag_2_months.csv", stringsAsFactors = FALSE)


# 2-WEEK LAG MODEL
# With the Outcome: News and Predictors: Congress lag + News lag

fit_2weeks <- brm(
  bf(news_t ~ cong_lag2w + news_lag2w,
     hu ~ cong_lag2w + news_lag2w),
  data    = df_2weeks,
  family  = hurdle_poisson(),
  chains  = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent  = 2
)

print(summary(fit_2weeks))


# 1-MONTH LAG MODEL

fit_1month <- brm(
  bf(news_t ~ cong_lag1 + news_lag1,
     hu ~ cong_lag1 + news_lag1),
  data    = df_1month,
  family  = hurdle_poisson(),
  chains  = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent  = 2
)

print(summary(fit_1month))

# 2-MONTH LAG MODEL


fit_2month <- brm(
  bf(news_t ~ cong_lag2 + news_lag2,
     hu ~ cong_lag2 + news_lag2),
  data    = df_2month,
  family  = hurdle_poisson(),
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

png("ppc_three_plots_reversed.png", width = 1500, height = 320, res = 150)
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

png("ppc_bars_three_reversed.png", width = 1500, height = 320, res = 150)
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
  ggtitle("2-Month Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

combined_reversed <- p1 | p2 | p3
combined_reversed

