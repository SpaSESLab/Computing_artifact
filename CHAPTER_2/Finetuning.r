# Bayesian Hurdle Model for News -> Congress Direction
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

CHAINS    <- 4
ITER      <- 4000
WARMUP    <- 1000
BAYES_SEED <- 1234

#adding year as a factor
df_2weeks <- read.csv("CHAPTER_2/lag_2_weeks_new_check_with_journal.csv", stringsAsFactors = FALSE)%>%
  mutate(year = factor(format(as.Date(month_t), "%Y")))
df_1month <- read.csv("CHAPTER_2/lag_1_month_new_check_with_journal.csv", stringsAsFactors = FALSE)%>%
  mutate(year = factor(format(as.Date(month_t), "%Y")))
df_2month <- read.csv("CHAPTER_2/lag_2_months_new_check_with_journal.csv", stringsAsFactors = FALSE) %>%
  mutate(year = factor(format(as.Date(month_t), "%Y")))

# journal is treated as a grouping factor
df_2weeks$journal <- as.factor(df_2weeks$journal)
df_1month$journal <- as.factor(df_1month$journal)
df_2month$journal <- as.factor(df_2month$journal)

#adding priors, adding varying slopes, adjusting sampling params to try and avoid diversgences


# 2-WEEK LAG MODEL
fit_2weeks <- brm(
  bf(
    cong_t ~ news_lag2w + (news_lag2w | journal) + cong_lag2w + year,
    hu ~ news_lag2w + (news_lag2w | journal) + cong_lag2w + year
  ),
  data = df_2weeks,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  cores = 4,
  control = list(adapt_delta = 0.99),
  prior = prior(normal(0,1), class = sd) +
    prior(normal(0,1), class = sd, dpar = hu) +
    prior(normal(0,1), class = b) +
    prior(normal(0,1), class = Intercept) +
    prior(lkj(2), class = cor),
  backend = "cmdstanr"
)

print(summary(fit_2weeks))

# 1-MONTH LAG MODEL
fit_1month <- brm(
  bf(
    cong_t ~ news_lag1 +  (news_lag1 | journal) + cong_lag1 + year,
    hu ~ news_lag1 +  (news_lag1 | journal) + cong_lag1
  ),
  data = df_1month,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 13),
  prior = prior(normal(0,1), class = sd) +
    prior(normal(0,1), class = sd, dpar = hu) +
    prior(normal(0,1), class = b) +
    prior(normal(0,1), class = Intercept) +
    prior(lkj(2), class = cor),
  backend = "cmdstanr"
)

print(summary(fit_1month))

# 2-MONTH LAG MODEL
fit_2month <- brm(
  bf(
    cong_t ~ news_lag2 + (cong_lag2 | journal),
    hu ~ news_lag2 + (cong_lag2 | journal)
  ),
  data = df_2month,
  family = hurdle_poisson(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  cores = 4,
  control = list(adapt_delta = 0.99),
  prior = prior(normal(0,1), class = sd) +
    prior(normal(0,1), class = sd, dpar = hu) +
    prior(normal(0,1), class = b) +
    prior(normal(0,1), class = Intercept) +
    prior(lkj(2), class = cor),
  backend = "cmdstanr"
)

print(summary(fit_2month))

# POSTERIOR DISTRIBUTIONS FOR NEWS LAG EFFECTS
bayesplot::color_scheme_set("blue")

make_plot <- function(fit, par1, par2, label1, label2, title_text) {
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

p_2w <- make_plot(
  fit_2weeks,
  "b_news_lag2w", "b_hu_news_lag2w",
  "News lag (2 weeks)", "Hurdle: News lag (2 weeks)",
  "2-Week Lag"
)

p_1m <- make_plot(
  fit_1month,
  "b_news_lag1", "b_hu_news_lag1",
  "News lag (1 month)", "Hurdle: News lag (1 month)",
  "1-Month Lag"
)

p_2m <- make_plot(
  fit_2month,
  "b_news_lag2", "b_hu_news_lag2",
  "News lag (2 months)", "Hurdle: News lag (2 months)",
  "2-Month Lag"
)

png("combined_mcmc_areas_news_lags_with_journal.png", width = 1800, height = 2200, res = 300)
gridExtra::grid.arrange(
  p_2w, p_1m, p_2m,
  ncol = 1,
  top = grid::textGrob(
    "Posterior distributions of news lag effects\nacross news-to-Congress hurdle Poisson models with journal random intercepts",
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

png("ppc_three_plots_news_to_congress_with_journal.png", width = 1500, height = 320, res = 150)
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

p1b <- make_ppc_bars(fit_2weeks, "Posterior Predictive Bar Plot: 2-Week Model")
p2b <- make_ppc_bars(fit_1month, "Posterior Predictive Bar Plot: 1-Month Model")
p3b <- make_ppc_bars(fit_2month, "Posterior Predictive Bar Plot: 2-Month Model")

png("ppc_bars_three_news_to_congress_with_journal.png", width = 1500, height = 320, res = 150)
grid.arrange(p1b, p2b, p3b, nrow = 1)
dev.off()

# DENSITY PLOTS
p_hist1 <- plot(fit_2weeks) +
  ggtitle("2-Week Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

p_hist2 <- plot(fit_1month) +
  ggtitle("1-Month Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

p_hist3 <- plot(fit_2month) +
  ggtitle("2-Month Model") +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )

combined_news_to_cong <- p_hist1 | p_hist2 | p_hist3
combined_news_to_cong