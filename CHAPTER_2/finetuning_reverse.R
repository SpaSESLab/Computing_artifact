# Bayesian Hurdle Model for Congress -> News Direction
# Random intercept for journal on both parts

library(brms)
library(emmeans)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom.mixed)
library(bayesplot)
library(posterior)
library(gridExtra)
library(grid)
library(patchwork)

CHAINS     <- 4
ITER       <- 4000
WARMUP     <- 1000
BAYES_SEED <- 1234


df_2weeks <- read.csv("reverse_lag_2_weeks_new_check_with_journal.csv", stringsAsFactors = FALSE)
df_1month  <- read.csv("reverse_lag_1_month_new_check_with_journal.csv", stringsAsFactors = FALSE)
df_2month  <- read.csv("reverse_lag_2_months_new_check_with_journal.csv", stringsAsFactors = FALSE)

# journal is treated as a grouping factor
df_2weeks$journal <- as.factor(df_2weeks$journal)
df_1month$journal  <- as.factor(df_1month$journal)
df_2month$journal  <- as.factor(df_2month$journal)

# 2-WEEK LAG MODEL
fit_2weeks <- brm(
  bf(news_t ~ cong_lag2w + news_lag2w + (1 | journal),
     hu ~ cong_lag2w + news_lag2w + (1 | journal)),
  data   = df_2weeks,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

print(summary(fit_2weeks))

# 1-MONTH LAG MODEL
fit_1month <- brm(
  bf(news_t ~ cong_lag1 + news_lag1 + (1 | journal),
     hu ~ cong_lag1 + news_lag1 + (1 | journal)),
  data   = df_1month,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

print(summary(fit_1month))

# 2-MONTH LAG MODEL
fit_2month <- brm(
  bf(news_t ~ cong_lag2 + news_lag2 + (1 | journal),
     hu ~ cong_lag2 + news_lag2 + (1 | journal)),
  data   = df_2month,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

print(summary(fit_2month))

# POSTERIOR DISTRIBUTIONS FOR CONGRESS LAG EFFECTS
bayesplot::color_scheme_set("blue")

make_reverse_plot <- function(fit, par1, par2, label1, label2, title_text) {
  d <- posterior::as_draws_df(fit)
  d[[label1]] <- d[[par1]]
  d[[label2]] <- d[[par2]]

  bayesplot::mcmc_areas(
    d,
    pars = c(label1, label2),
    prob = 0.80,
    prob_outer = 0.95,
    point_est = "median"
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::labs(
      title = title_text,
      x = "Posterior estimate",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13,
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = margin(6, 10, 6, 10)
    )
}

p_2w <- make_reverse_plot(
  fit_2weeks,
  "b_cong_lag2w", "b_hu_cong_lag2w",
  "Congress lag (2 weeks)", "Hurdle: Congress lag (2 weeks)",
  "2-Week Lag"
)

p_1m <- make_reverse_plot(
  fit_1month,
  "b_cong_lag1", "b_hu_cong_lag1",
  "Congress lag (1 month)", "Hurdle: Congress lag (1 month)",
  "1-Month Lag"
)

p_2m <- make_reverse_plot(
  fit_2month,
  "b_cong_lag2", "b_hu_cong_lag2",
  "Congress lag (2 months)", "Hurdle: Congress lag (2 months)",
  "2-Month Lag"
)

png("combined_mcmc_areas_cong_lags_reverse_journal.png", width = 1800, height = 2200, res = 300)
gridExtra::grid.arrange(
  p_2w, p_1m, p_2m,
  ncol = 1,
  top = grid::textGrob(
    "Posterior distributions of Congress lag effects\nacross reverse-direction hurdle Poisson models",
    gp = grid::gpar(fontsize = 14, fontface = "bold"),
    just = "center"
  )
)
dev.off()

# POSTERIOR PREDICTIVE CHECKS: DENSITY OVERLAY
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

png("ppc_three_plots_reversed_new_journal.png", width = 1500, height = 320, res = 150)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

# POSTERIOR PREDICTIVE BAR PLOTS
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

png("ppc_bars_three_reversed_new_journal.png", width = 1500, height = 320, res = 150)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

# DENSITY PLOTS
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