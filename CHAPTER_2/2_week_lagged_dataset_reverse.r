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
  filter(meta_category %in% keep_categories) %>%
  mutate(
    date = ymd(str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}")),
    month = floor_date(date, unit = "month")
  )

news <- news %>%
  filter(meta_category %in% keep_categories) %>%
  mutate(
    date = as.Date(Date),
    month = floor_date(date, unit = "month")
  )

# 2-week Congress windows
cong_2w <- cong %>%
  mutate(
    week_start = date,
    week_end = date + days(13),
    date_lag = date + days(14),
    month_t = floor_date(date_lag, unit = "month")
  ) %>%
  group_by(week_start, week_end, month_t) %>%
  summarise(cong_lag2w = n(), .groups = "drop")

# 2-week News windows with journal
news_2w <- news %>%
  mutate(
    week_start = date,
    week_end = date + days(13),
    date_lag = date + days(14),
    month_t = floor_date(date_lag, unit = "month")
  ) %>%
  group_by(week_start, week_end, month_t, journal) %>%
  summarise(news_lag2w = n(), .groups = "drop")

# Current counts by month and journal
news_current <- news %>%
  group_by(month, journal) %>%
  summarise(news_t = n(), .groups = "drop")

cong_current <- cong %>%
  group_by(month) %>%
  summarise(cong_t = n(), .groups = "drop")

# Final 2-week reverse panel
lag2w_rev <- news_2w %>%
  left_join(cong_2w, by = c("week_start", "week_end", "month_t")) %>%
  left_join(news_current, by = c("month_t" = "month", "journal")) %>%
  left_join(cong_current, by = c("month_t" = "month")) %>%
  mutate(
    news_lag2w = replace_na(news_lag2w, 0L),
    cong_lag2w = replace_na(cong_lag2w, 0L),
    news_t = replace_na(news_t, 0L),
    cong_t = replace_na(cong_t, 0L)
  ) %>%
  arrange(journal, month_t, week_start) %>%
  select(week_start, week_end, month_t, journal, news_lag2w, cong_lag2w, news_t, cong_t)

write_csv(lag2w_rev, "reverse_lag_2_weeks_new_check_with_journal.csv")