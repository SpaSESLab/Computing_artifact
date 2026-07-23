library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)

cong <- read_csv(
  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/LLM_model/congress_extracted_new.csv",
  show_col_types = FALSE
)

news <- read_csv(
  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/LLM_model/wildlife_extracted_with_3154_journal.csv",
  show_col_types = FALSE
)

keep_categories <- c("ESA Species Listing", "ESA Species Delisting")

cong <- cong %>%
  filter(meta_category %in% keep_categories)

news <- news %>%
  filter(meta_category %in% keep_categories)

cong <- cong %>%
  mutate(
    date = ymd(str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}")),
    month = floor_date(date, unit = "month")
  )

news <- news %>%
  mutate(
    date = as.Date(Date),
    month = floor_date(date, unit = "month")
  )

cong_monthly <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

news_monthly <- news %>%
  group_by(month, journal) %>%
  summarise(news_t = n(), .groups = "drop")

news_journal_list <- news_monthly %>%
  distinct(journal)

base_months <- full_join(
  cong_monthly,
  news_monthly,
  by = "month"
) %>%
  arrange(month)

base_df <- tidyr::expand_grid(
  month = seq(min(base_months$month), max(base_months$month), by = "month"),
  journal = news_journal_list$journal
) %>%
  left_join(news_monthly, by = c("month", "journal")) %>%
  left_join(cong_monthly, by = "month") %>%
  arrange(journal, month) %>%
  mutate(
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L)
  )

lag1 <- base_df %>%
  group_by(journal) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    news_lag1 = lag(news_t, 1),
    cong_lag1 = lag(cong_t, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(news_lag1), !is.na(cong_lag1)) %>%
  mutate(
    news_lag1 = as.integer(news_lag1),
    cong_lag1 = as.integer(cong_lag1),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, journal, news_lag1, cong_lag1, news_t, cong_t)

write_csv(lag1, "lag_1_month_new_check_with_journal.csv")

lag2 <- base_df %>%
  group_by(journal) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    news_lag2 = lag(news_t, 2),
    cong_lag2 = lag(cong_t, 2)
  ) %>%
  ungroup() %>%
  filter(!is.na(news_lag2), !is.na(cong_lag2)) %>%
  mutate(
    news_lag2 = as.integer(news_lag2),
    cong_lag2 = as.integer(cong_lag2),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, journal, news_lag2, cong_lag2, news_t, cong_t)

write_csv(lag2, "lag_2_months_new_check_with_journal.csv")

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

lag2w <- full_join(
  news_2w,
  cong_2w,
  by = c("week_start", "week_end", "month_t")
) %>%
  arrange(journal, month_t, week_start) %>%
  mutate(
    news_lag2w = replace_na(news_lag2w, 0L),
    cong_lag2w = replace_na(cong_lag2w, 0L)
  )

news_current <- news %>%
  group_by(month, journal) %>%
  summarise(news_t = n(), .groups = "drop")

cong_current <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

lag2w <- lag2w %>%
  left_join(news_current, by = c("month_t" = "month", "journal")) %>%
  left_join(cong_current, by = c("month_t" = "month")) %>%
  mutate(
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L),
    news_lag2w = as.integer(news_lag2w),
    cong_lag2w = as.integer(cong_lag2w),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(week_start, week_end, month_t, journal, news_lag2w, cong_lag2w, news_t, cong_t)

write_csv(lag2w, "lag_2_weeks_new_check_with_journal.csv")