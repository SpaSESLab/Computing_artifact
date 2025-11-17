#Trend between Topics for Wildlife news articles

library(readr)
library(dplyr)
library(ggplot2)


stm_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Wildlife_news_articles_STM_topics.csv")
bert_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Wildlife_news_articles_BERTopic_topics.csv")


# Extracting year
stm_df$Year <- as.numeric(substr(stm_df$Date, 1, 4))
bert_df$Year <- as.numeric(substr(bert_df$Date, 1, 4))

# Filter by topic for comparison (for example STM topic 1 and BERTopic topic 3)
stm_selected <- stm_df %>% filter(topic == 1) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 1")

bert_selected <- bert_df %>% filter(Topic == 3) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "BERTopic Topic 3")

# Combine for plotting
df <- bind_rows(stm_selected, bert_selected)

# Including all years up to 2025 (even if missing)
year_range <- data.frame(Year = seq(min(df$Year), 2025))
df <- df %>% right_join(year_range, by = "Year") %>%
  mutate(
    Document_Count = ifelse(is.na(Document_Count), 0, Document_Count),
    Method = ifelse(is.na(Method), "STM Topic 1", Method)
  )

# Plot
plt <- ggplot(df, aes(x = Year, y = Document_Count, color = Method, group = Method)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2025, by = 5)) +
  scale_y_continuous(limits = c(0, max(df$Document_Count, na.rm = TRUE)), breaks = seq(0, max(df$Document_Count, na.rm = TRUE), by = 10)) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 14, margin = margin(b = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 1.5),
    panel.grid.major = element_line(color = "white", linewidth = 0.6),
    panel.grid.minor = element_line(color = "white", linewidth = 0.3)
  ) +
  labs(
    title = "Trend: STM Topic 1 vs BERTopic Topic 3 -- Wildlife news articles",
    x = "Year",
    y = "Document Count"
  )

print(plt)





#Trend between Topics for Congress

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Loading CSVs
stm_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_STM_topics_and_keywords.csv")
bert_df <- read_csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_BERTopic_topics_and_keywords.csv")

# Year extraction from varied date strings
parse_year <- function(date_str) {
  date_parsed <- parse_date_time(date_str, orders = c("B d, Y", "Y-m-d", "m/d/Y"), exact = FALSE)
  year_val <- year(date_parsed)
  return(year_val)
}

stm_df$Year <- sapply(stm_df$date, parse_year)
bert_df$Year <- sapply(bert_df$date, parse_year)

# Topic filtering for specified year range
stm_selected <- stm_df %>% filter(topic == 4, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "STM Topic 4")

bert_selected <- bert_df %>% filter(topics == 2, Year >= 1990 & Year <= 2025) %>%
  group_by(Year) %>%
  summarise(Document_Count = n()) %>%
  mutate(Method = "BERTopic Topic 2")

# Bind and ensure complete year range for each method
year_range <- seq(1990, 2025)
methods <- unique(c("STM Topic 4", "BERTopic Topic 2"))
full_df <- expand.grid(Year = year_range, Method = methods) %>%
  left_join(bind_rows(stm_selected, bert_selected), by = c("Year", "Method")) %>%
  mutate(Document_Count = ifelse(is.na(Document_Count), 0, Document_Count))

# Plot 
plt <- ggplot(full_df, aes(x = Year, y = Document_Count, color = Method, group = Method)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(
    limits = c(0, max(full_df$Document_Count)),
    breaks = seq(0, max(full_df$Document_Count), by = 10),
    labels = scales::comma
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),  # critical line
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  labs(
    title = "Trend: STM Topic 4 vs BERTopic Topic 2 -- Congress",
    x = "Year",
    y = "Document Count"
  )

print(plt)




