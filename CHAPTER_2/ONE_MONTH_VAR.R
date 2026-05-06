# ============================================================
#  CHAPTER 2 — STEP 1: Fix p=1 + STEP 2: Fit VAR(1)
#  Findings and insights BEFORE Granger causality
#  MONTHLY VERSION — lag order fixed at p = 1 month
#  Dataset: _180 files (news n=180, congress n=180)
#  Overlap window: 1995-01 to 2017-02 (n=266 months)
# ============================================================

library(tidyverse)
library(vars)
library(ggplot2)
library(gridExtra)

select <- dplyr::select

# LOAD DATA 
news <- read.csv("narrative_coded_results_with_baselines_widlife_180.csv", stringsAsFactors = FALSE)
cong <- read.csv("congress_document_outputs_180.csv",stringsAsFactors = FALSE)

news$year_month <- substr(news$Date, 1, 7)
cong$year_month <- sub("CREC-(\\d{4}-\\d{2})-.*", "\\1",
                       cong$document_id)

months <- format(
  seq(as.Date("1995-01-01"),
      as.Date("2017-02-01"), by = "month"), "%Y-%m")

# ESTIMABLE CATEGORIES
# Cooperative Arrangements: 0 congressional docs — excluded
# Habitat / Recovery: 9 news nonzero months — below threshold
# Industry / Economic Conflict: 2 cong nonzero months — too sparse
# These 4 have enough data to fit VAR(1)
estimable_cats <- c(
  "Regulatory / Agency Action",   # news nz=30, cong nz=38 -- PRIMARY
  "Legislative Action",           # news nz=10, cong nz=63 -- sparse news
  "Listing / Delisting",          # news nz=13, cong nz=6  -- sparse cong
  "Litigation / Courts"           # news nz=25, cong nz=7  -- sparse cong
)

LAG_ORDER  <- 1L
MIN_NONZERO <- 10

# MONTHLY COUNT BUILDER 
monthly_counts <- function(df, cat, month_range) {
  df %>%
    filter(event_group == cat) %>%
    count(year_month, name = "n") %>%
    right_join(data.frame(year_month = month_range),
               by = "year_month") %>%
    replace_na(list(n = 0)) %>%
    arrange(year_month) %>%
    pull(n)
}

# ════════════════════════════════════════════════════════════
#  STEP 2: FIT VAR(1) FOR EACH ESTIMABLE CATEGORY
#  Extract and report:
#    - β₁ (news own-lag)
#    - β₂ (congress -> news cross-lag)
#    - β₃ (congress own-lag)
#    - β₄ (news -> congress cross-lag)
# ════════════════════════════════════════════════════════════
cat(" STEP 2: FIT VAR(1) — COEFFICIENT ESTIMATES\n")

cat("VAR(1) SYSTEM FOR EACH CATEGORY:\n")
cat("  news_t    = α₁ + β₁×news_{t-1}  + β₂×cong_{t-1}  + ε₁\n")
cat("  congress_t = α₂ + β₃×cong_{t-1}  + β₄×news_{t-1}  + ε₂\n\n")
cat("  β₄ = media agenda-setting coefficient \n")
cat("  β₂ = congress agenda-setting coefficient\n\n")


all_fits <- list()
results  <- list()

for (cat_name in estimable_cats) {
  
  n_vec  <- monthly_counts(news, cat_name, months)
  c_vec  <- monthly_counts(cong, cat_name, months)
  sparse <- (sum(n_vec > 0) < MIN_NONZERO |
               sum(c_vec > 0) < MIN_NONZERO)
  
  ts_mat  <- cbind(news_esa = n_vec, cong_esa = c_vec)
  var_fit <- tryCatch(
    VAR(ts_mat, p = LAG_ORDER, type = "const"),
    error = function(e) NULL
  )
  
  if (is.null(var_fit)) next
  
  all_fits[[cat_name]] <- var_fit
  
  ne <- coef(var_fit)$news_esa
  ce <- coef(var_fit)$cong_esa
  
  results[[cat_name]] <- data.frame(
    Category = cat_name,
    Sparse   = if (sparse) "\u2020" else "",
    
    # β₁: news own-lag (news equation)
    b1_est = round(ne["news_esa.l1", "Estimate"],  4),
    b1_se  = round(ne["news_esa.l1", "Std. Error"],4),
    b1_t   = round(ne["news_esa.l1", "t value"],   3),
    b1_p   = round(ne["news_esa.l1", "Pr(>|t|)"],  3),
    
    # β₂: congress -> news cross-lag (news equation)
    b2_est = round(ne["cong_esa.l1", "Estimate"],  4),
    b2_se  = round(ne["cong_esa.l1", "Std. Error"],4),
    b2_t   = round(ne["cong_esa.l1", "t value"],   3),
    b2_p   = round(ne["cong_esa.l1", "Pr(>|t|)"],  3),
    
    # β₃: congress own-lag (congress equation)
    b3_est = round(ce["cong_esa.l1", "Estimate"],  4),
    b3_se  = round(ce["cong_esa.l1", "Std. Error"],4),
    b3_t   = round(ce["cong_esa.l1", "t value"],   3),
    b3_p   = round(ce["cong_esa.l1", "Pr(>|t|)"],  3),
    
    # β₄: news -> congress cross-lag (congress equation)
    b4_est = round(ce["news_esa.l1", "Estimate"],  4),
    b4_se  = round(ce["news_esa.l1", "Std. Error"],4),
    b4_t   = round(ce["news_esa.l1", "t value"],   3),
    b4_p   = round(ce["news_esa.l1", "Pr(>|t|)"],  3),
    
    R2_news = round(summary(var_fit)$varresult$news_esa$r.squared, 3),
    R2_cong = round(summary(var_fit)$varresult$cong_esa$r.squared, 3),
    
    stringsAsFactors = FALSE
  )
}

# SIGNIFICANCE
coef_df <- bind_rows(results) %>%
  mutate(
    # significance stars
    sig_b1 = case_when(b1_p < 0.001 ~ "***", b1_p < 0.01 ~ "**",
                       b1_p < 0.05  ~ "*",   b1_p < 0.10 ~ "†",
                       TRUE ~ ""),
    sig_b2 = case_when(b2_p < 0.001 ~ "***", b2_p < 0.01 ~ "**",
                       b2_p < 0.05  ~ "*",   b2_p < 0.10 ~ "†",
                       TRUE ~ ""),
    sig_b3 = case_when(b3_p < 0.001 ~ "***", b3_p < 0.01 ~ "**",
                       b3_p < 0.05  ~ "*",   b3_p < 0.10 ~ "†",
                       TRUE ~ ""),
    sig_b4 = case_when(b4_p < 0.001 ~ "***", b4_p < 0.01 ~ "**",
                       b4_p < 0.05  ~ "*",   b4_p < 0.10 ~ "†",
                       TRUE ~ "")
  )

#RESULTS 
coef_df %>%
  select(Category, Sparse,
         b4_est, b4_se, b4_t, b4_p, sig_b4,
         b2_est, b2_se, b2_t, b2_p, sig_b2,
         R2_news, R2_cong) %>%
  print(row.names = FALSE)


# SAVE 
write.csv(coef_df, "var_p1_results.csv", row.names = FALSE)

# ============================================================
#WHY USING P VALUES
# ============================================================

#The specific reason p-values fit your VAR design.

#Your VAR produces OLS t-statistics for each coefficient. 
#The p-value is simply the probability associated with that t-statistic under the null hypothesis that the coefficient equals zero. 
#This is not a choice you make — it is the natural output of OLS regression and the standard way 
#to evaluate VAR coefficients in the time series literature.
#Every VAR study you cite — including Barberá et al. (2019) — reports p-values or equivalent confidence intervals for exactly this reason.

# The p-value quantifies how likely my observed coefficients are under the null hypothesis that no cross-institutional relationship exists.
# This is the standard inferential tool for OLS (Ordinary Least Squares)-estimated VAR coefficients in the political science time series literature. 
# I report all four conventional significance levels — p < 0.001, p < 0.01, p < 0.05, and p < 0.10 — so 
# the strength of evidence is visible at a glance rather than reduced to a single binary significant or not significant judgment
# ============================================================