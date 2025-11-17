
# Load packages
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggwordcloud)
library(lsa)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


df <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Bertopic_keyword_counts_for_news.csv", stringsAsFactors = FALSE)

head(df, 5)

set.seed(9)  # for reproducible layout

# Create word cloud plot with heading
plt1 <- ggplot(df, aes(label = keyword, size = count, color = count)) +
  geom_text_wordcloud(shape = "circle", area_corr_power = 1, rm_outside = TRUE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Count")) +
  theme_minimal() +
  ggtitle("Keyword Frequency Word Cloud for Bertopic")   # <-- Add your desired heading here

plt1



library(dplyr)
library(stringr)
library(tidyr)
library(coop)
library(pheatmap)

stm_words <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords_with_weights.csv", stringsAsFactors = FALSE)

bert_words <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_bertopic_keywords_with_weights_01.csv", stringsAsFactors = FALSE)



# Suppose your topics are labeled by numbers (0, 1, 2, ...)

# Get numeric topic IDs from row and column names
stm_ord <- order(as.numeric(gsub("STM_", "", rownames(cosine_mat))))
bert_ord <- order(as.numeric(gsub("BERT_", "", colnames(cosine_mat))))

# Reorder the matrix
cosine_mat_ordered <- cosine_mat[stm_ord, bert_ord]

# Update row and column names for clarity
rownames(cosine_mat_ordered) <- paste0("STM_", sort(as.numeric(gsub("STM_", "", rownames(cosine_mat))))
)
colnames(cosine_mat_ordered) <- paste0("BERT_", sort(as.numeric(gsub("BERT_", "", colnames(cosine_mat))))
)

# Plot heatmap with ordered topics and no dendrograms
pheatmap(
  cosine_mat_ordered,
  display_numbers = TRUE,
  color = colorRampPalette(c("white", "skyblue", "navy"))(100),
  main = "Cosine Similarity between STM and BERTopic Topics",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = FALSE
)








