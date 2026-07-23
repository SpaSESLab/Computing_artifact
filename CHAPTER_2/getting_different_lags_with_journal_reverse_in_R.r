library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)

# Read files
cong <- read_csv(
  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/LLM_model/congress_extracted_new.csv",
  show_col_types = FALSE
)

news <- read_csv(
  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/LLM_model/wildlife_extracted_with_3154_journal.csv",
  show_col_types = FALSE
)

# Keep only ESA Species Listing and ESA Species Delisting
keep_categories <- c("ESA Species Listing", "ESA Species Delisting")

cong <- cong %>%
  filter(meta_category %in% keep_categories)

news <- news %>%
  filter(meta_category %in% keep_categories)

# Parse congressional date
cong <- cong %>%
  mutate(
    date = ymd(str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}")),
    month = floor_date(date, unit = "month")
  )

# Parse news date
news <- news %>%
  mutate(
    date = as.Date(Date),
    month = floor_date(date, unit = "month")
  )

# Monthly counts
cong_monthly <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

news_monthly <- news %>%
  group_by(month, journal) %>%
  summarise(news_t = n(), .groups = "drop")

# All months and journals
month_seq <- seq(
  min(c(cong_monthly$month, news_monthly$month), na.rm = TRUE),
  max(c(cong_monthly$month, news_monthly$month), na.rm = TRUE),
  by = "month"
)

journals <- news %>%
  distinct(journal) %>%
  pull(journal)

# Base monthly panel with journal retained
base_df <- tidyr::expand_grid(
  month = month_seq,
  journal = journals
) %>%
  left_join(news_monthly, by = c("month", "journal")) %>%
  left_join(cong_monthly, by = "month") %>%
  arrange(journal, month) %>%
  mutate(
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L)
  )

# 1-MONTH REVERSE-DIRECTION DATASET
lag1_rev <- base_df %>%
  group_by(journal) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    cong_lag1 = lag(cong_t, 1),
    news_lag1 = lag(news_t, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(cong_lag1), !is.na(news_lag1)) %>%
  mutate(
    cong_lag1 = as.integer(cong_lag1),
    news_lag1 = as.integer(news_lag1),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, journal, cong_lag1, news_lag1, news_t, cong_t)

write_csv(lag1_rev, "reverse_lag_1_month_new_check_with_journal.csv")

# 2-MONTH REVERSE-DIRECTION DATASET
lag2_rev <- base_df %>%
  group_by(journal) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    cong_lag2 = lag(cong_t, 2),
    news_lag2 = lag(news_t, 2)
  ) %>%
  ungroup() %>%
  filter(!is.na(cong_lag2), !is.na(news_lag2)) %>%
  mutate(
    cong_lag2 = as.integer(cong_lag2),
    news_lag2 = as.integer(news_lag2),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, journal, cong_lag2, news_lag2, news_t, cong_t)

write_csv(lag2_rev, "reverse_lag_2_months_new_check_with_journal.csv")

# 2-WEEK REVERSE-DIRECTION DATASET
cong_2w <- cong %>%
  mutate(
    week_start = date,
    week_end = date + days(13),
    date_lag = date + days(14),
    month_t = floor_date(date_lag, unit = "month")
  ) %>%
  group_by(week_start, week_end, month_t) %>%
  summarise(cong_lag2w = n(), .groups = "drop")

news_2w <- news %>%
  mutate(
    week_start = date,
    week_end = date + days(13),
    date_lag = date + days(14),
    month_t = floor_date(date_lag, unit = "month")
  ) %>%
  group_by(week_start, week_end, month_t, journal) %>%
  summarise(news_lag2w = n(), .groups = "drop")

journals_2w <- news %>%
  distinct(journal) %>%
  pull(journal)

months_2w <- seq(
  min(c(cong_2w$month_t, news_2w$month_t), na.rm = TRUE),
  max(c(cong_2w$month_t, news_2w$month_t), na.rm = TRUE),
  by = "month"
)

base_2w <- tidyr::expand_grid(
  month_t = months_2w,
  journal = journals_2w
)

lag2w_rev <- base_2w %>%
  left_join(news_2w, by = c("month_t", "journal")) %>%
  left_join(cong_2w, by = "month_t") %>%
  mutate(
    news_lag2w = replace_na(news_lag2w, 0L),
    cong_lag2w = replace_na(cong_lag2w, 0L)
  ) %>%
  arrange(journal, month_t) %>%
  select(week_start, week_end, month_t, journal, cong_lag2w, news_lag2w)

write_csv(lag2w_rev, "reverse_lag_2_weeks_new_check_with_journal.csv")