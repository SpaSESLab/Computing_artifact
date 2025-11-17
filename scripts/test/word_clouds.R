# Poster Figures
# ====================================================

# Load packages
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggwordcloud)

# --------------------------------
# Read in News Article Term Data and Reshape
#news_terms <- read.csv(
  #here("data", "original", "wildlife_topics_and_weights.csv")
  
library(here) 
  
news_terms_STM <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords_with_weights.csv")

#filter_news_terms <- news_terms %>% 
  #filter(topic %in% top_topics)




# Term Distribution Word Cloud for News Article Top Topics
set.seed(6)  # for reproducible layout
plt2 <- ggplot(news_terms_STM, aes(label = keyword, size = weight, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = FALSE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Weight")) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.margin = margin(2, 2, 2, 2)
  )

plt2

# Save Word Cloud as PNG
ggsave(
  here("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_wordcloud.png"),
  plt2,
  width = 8,
  height = 4,
  dpi = 300
)


#CONGRESS

#congress_stm_keywords_weights.csv


library(here) 
  
news_terms_BERTopic <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_bertopic_keywords_with_weights.csv")

#filter_congress_terms <- congress_terms %>% 
  #filter(topic %in% top_topics)


# Term Distribution Word Cloud for Congress Documents Top Topics
set.seed(9)  # for reproducible layout
plt4 <- ggplot(news_terms_BERTopic, aes(label = keyword, size = weight, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Weight")) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.margin = margin(2, 2, 2, 2)
  )

plt4

# Save Word Cloud as PNG
ggsave(
  here("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_wordcloud.png"),
  plt4,
  width = 8,
  height = 4,
  dpi = 300
)





# Read in News Article Data and Reshape
news_docs <- read.csv(
  here("data", "original", "wildlife_data_with_topics.csv")
)

news_docs$Date <- as.Date(news_docs$Date, format = "%Y-%m-%d")
news_docs$Year <- format(news_docs$Date, "%Y")

docs_sum <- news_docs %>% 
  group_by(Year, topic) %>%
  summarize(topic_sum = n())

docs_sum <- docs_sum %>% 
  group_by(Year) %>% 
  mutate(year_sum = sum(topic_sum))

docs_sum <- docs_sum %>% 
  mutate(topic_percent = (topic_sum/year_sum)*100)

topic_check <- docs_sum %>% 
  group_by(topic) %>% 
  summarise(total = sum(topic_sum))

top_topics <- c("0", "1")

filter_docs_sum <- docs_sum %>% 
  filter(topic %in% top_topics) %>% 
  filter(Year >= 1995) %>% 
  mutate(
    topic = if_else(topic == "0", "Topic 1", "Topic 2")
  )


# Topic Proportion Plot for News Article Topics
plt1 <- ggplot(
  filter_docs_sum,
  aes(
    x = as.numeric(Year),
    y = topic_percent,
    group = as.character(topic),
    color = as.character(topic)
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_brewer(palette = "Set2") +  # legend title here
  scale_x_continuous(
    limits = c(1995, 2025),
    breaks = seq(1995, 2025, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25)
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10,
                               margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 14, margin = margin(b = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 1.5),  # panel box
    panel.grid.major = element_line(color = "white", linewidth = 0.6),
    panel.grid.minor = element_line(color = "white", linewidth = 0.3)
  ) +
  labs(
    title = "Yearly Trend for Top 2 Topics on the Wildlife News Articles",
    x = "Year",
    y = "Topic Proportion (%)"
  )

plt1 

# Save Plot as PNG
ggsave(
  here("wildlife_topic_plot.png"),
  plt1,
  width = 14,
  height = 4,
  dpi = 300
)

#WORD CLOUD 

# Read in News Article Term Data and Reshape
news_terms <- read.csv(
  here("data", "original", "wildlife_topics_and_weights.csv")
)

filter_news_terms <- news_terms %>% 
  filter(topic %in% top_topics)

# Term Distribution Word Cloud for News Article Top Topics
set.seed(6)  # for reproducible layout
plt2 <- ggplot(filter_news_terms, aes(label = keyword, size = weight, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = FALSE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Weight")) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.margin = margin(2, 2, 2, 2)
  )

plt2

# Save Word Cloud as PNG
ggsave(
  here("wildlife_wordcloud.png"),
  plt2,
  width = 8,
  height = 4,
  dpi = 300
)

# --------------------------------

# Read in Congress Document Data and Reshape
congress_docs <- read.csv(
  here("data", "original", "congress_data_with_topics.csv")
)

congress_docs$date <- as.Date(trimws(congress_docs$date), format = "%B %d, %Y")
congress_docs$Year <- as.integer(format(congress_docs$date, "%Y"))

docs_sum <- congress_docs %>% 
  group_by(Year, topic) %>%
  summarize(topic_sum = n())

docs_sum <- docs_sum %>% 
  group_by(Year) %>% 
  mutate(year_sum = sum(topic_sum))

docs_sum <- docs_sum %>% 
  mutate(topic_percent = (topic_sum/year_sum)*100)

topic_check <- docs_sum %>% 
  group_by(topic) %>% 
  summarise(total = sum(topic_sum))

top_topics <- c("0", "1")

filter_docs_sum <- docs_sum %>% 
  filter(topic %in% top_topics) %>% 
  filter(Year >= 1995) %>% 
  mutate(
    topic = if_else(topic == "0", "Topic 1", "Topic 2")
  )


# Topic Proportion Plot for Congress Documents Topics
plt3 <- ggplot(
  filter_docs_sum,
  aes(
    x = as.numeric(Year),
    y = topic_percent,
    group = as.character(topic),
    color = as.character(topic)
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_brewer(palette = "Set1") +  # legend title here
  scale_x_continuous(
    limits = c(1995, 2025),
    breaks = seq(1995, 2025, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25)
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10,
                               margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 14, margin = margin(b = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 1.5),  # panel box
    panel.grid.major = element_line(color = "white", linewidth = 0.6),
    panel.grid.minor = element_line(color = "white", linewidth = 0.3)
  ) +
  labs(
    title = "Yearly Trend for Top 2 Topics on the Congress Documents",
    x = "Year",
    y = "Topic Proportion (%)"
  )

plt3 

# Save Plot as PNG
ggsave(
  here("congress_topic_plot.png"),
  plt3,
  width = 14,
  height = 4,
  dpi = 300
)


# Read in Congress Document Term Data and Reshape
congress_terms <- read.csv(
  here("data", "original", "congress_topics_and_weights.csv")
)

filter_congress_terms <- congress_terms %>% 
  filter(topic %in% top_topics)


# Term Distribution Word Cloud for Congress Documents Top Topics
set.seed(9)  # for reproducible layout
plt4 <- ggplot(filter_congress_terms, aes(label = keyword, size = weight, color = as.character(topic))) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 20, guide = guide_legend(title = "Weight")) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.margin = margin(2, 2, 2, 2)
  )

plt4

# Save Word Cloud as PNG
ggsave(
  here("congress_wordcloud.png"),
  plt4,
  width = 8,
  height = 4,
  dpi = 300
)