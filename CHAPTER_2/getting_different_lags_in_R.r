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
  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/LLM_model/wildlife_extracted_with_3154.csv",
  show_col_types = FALSE
)

# Keping only ESA Species Listing and ESA Species Delisting
keep_categories <- c("ESA Species Listing", "ESA Species Delisting")

cong <- cong %>%
  filter(meta_category %in% keep_categories)

news <- news %>%
  filter(meta_category %in% keep_categories)


#Parsing dates for the congressional date from document_id like CREC-2015-01-30-pt1-PgH666-4

cong <- cong %>%
  mutate(
    date = ymd(str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}")),
    month = floor_date(date, unit = "month")
  )

# News date from Date column
news <- news %>%
  mutate(
    date = as.Date(Date),
    month = floor_date(date, unit = "month")
  )


#monthly counts

cong_monthly <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

news_monthly <- news %>%
  group_by(month) %>%
  summarise(news_t = n(), .groups = "drop")

# Base monthly panel
base_df <- full_join(news_monthly, cong_monthly, by = "month") %>%
  arrange(month) %>%
  mutate(
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L)
  )


# 1-month lag dataset

lag1 <- base_df %>%
  mutate(
    news_lag1 = lag(news_t, 1),
    cong_lag1 = lag(cong_t, 1)
  ) %>%
  filter(!is.na(news_lag1), !is.na(cong_lag1)) %>%
  mutate(
    news_lag1 = as.integer(news_lag1),
    cong_lag1 = as.integer(cong_lag1),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, news_lag1, cong_lag1, news_t, cong_t)

write_csv(lag1, "lag_1_month_new_check.csv")


# 2-month lag dataset

lag2 <- base_df %>%
  mutate(
    news_lag2 = lag(news_t, 2),
    cong_lag2 = lag(cong_t, 2)
  ) %>%
  filter(!is.na(news_lag2), !is.na(cong_lag2)) %>%
  mutate(
    news_lag2 = as.integer(news_lag2),
    cong_lag2 = as.integer(cong_lag2),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(month_t = month, news_lag2, cong_lag2, news_t, cong_t)

write_csv(lag2, "lag_2_months_new_check.csv")


# 2-week lag dataset

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
  group_by(week_start, week_end, month_t) %>%
  summarise(news_lag2w = n(), .groups = "drop")

lag2w <- full_join(
  news_2w,
  cong_2w,
  by = c("week_start", "week_end", "month_t")
) %>%
  arrange(month_t, week_start) %>%
  mutate(
    news_lag2w = replace_na(news_lag2w, 0L),
    cong_lag2w = replace_na(cong_lag2w, 0L)
  )

# Current-period counts aggregated by month
news_current <- news %>%
  group_by(month) %>%
  summarise(news_t = n(), .groups = "drop")

cong_current <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

lag2w <- lag2w %>%
  left_join(news_current, by = c("month_t" = "month")) %>%
  left_join(cong_current, by = c("month_t" = "month")) %>%
  mutate(
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L),
    news_lag2w = as.integer(news_lag2w),
    cong_lag2w = as.integer(cong_lag2w),
    news_t = as.integer(news_t),
    cong_t = as.integer(cong_t)
  ) %>%
  select(week_start, week_end, news_lag2w, cong_lag2w, news_t, cong_t)

write_csv(lag2w, "lag_2_weeks_new_check.csv")