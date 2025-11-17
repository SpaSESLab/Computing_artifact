
# Load packages
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggwordcloud)
library(lsa)


beta <- read.csv(here("stm_beta.csv"))

gamma <- read.csv(here("stm_gamma.csv"))

top_beta <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
set.seed(9)  # for reproducible layout
plt1 <- ggplot(top_beta, aes(label = term, size = beta, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Beta")) +
  theme_minimal()
plt1


ggsave(
  here("stm_terms.png"),
  plt1,
  width = 8,
  height = 4,
  dpi = 300
)


bert_terms <- read.csv(here("bert_terms.csv"))


word_weights <- bert_terms %>%
  group_by(keyword) %>%
  summarise(weight = sum(weight)) %>%
  ungroup()


top_bert_terms <- bert_terms %>%
  group_by(topic) %>%
  slice_max(weight, n = 10)
set.seed(9)  # for reproducible layout
plt2 <- ggplot(top_bert_terms, aes(label = keyword, size = weight, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Weight")) +
  theme_minimal()
plt2


ggsave(
  here("bert_terms.png"),
  plt2,
  width = 8,
  height = 4,
  dpi = 300
)
