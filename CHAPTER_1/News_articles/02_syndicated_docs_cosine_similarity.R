#### 02_syndicated docs_cosine_similarity

#install.packages(c("ldatuning"))

# Load necessary libraries
library(readr)
library(dplyr)
library(tidytext)
library(tm)
library(topicmodels)
library(ggplot2)
library(parallel) 
library(slam) 
library(Rmpfr)  
library(reshape2) 
library(scales) 
library(grid)
#library(ldatuning)
library(textmineR)
library(SnowballC)
library(textstem)
library(textclean)
library(stringr)
library(qdapDictionaries)
library(lexicon)
library(here)
#library(lubridate)
library(text2vec)


clean_corpus <- read.csv("clean_text_corpus.csv")

# Create DTM using TF-IDF
tokens <- word_tokenizer(clean_corpus$Text)
it <- itoken(tokens, ids = clean_corpus$GOID, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# Use TF-IDF
tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)


dim(dtm_tfidf)


# Compute cosine similarity
cosine_sim <- sim2(dtm_tfidf, method = "cosine", norm = "l2")

str(cosine_sim)
dim(cosine_sim)


saveRDS(cosine_sim, "whole_cosine_sim.rds")


# Identify syndicated articles based on threshold
similarity_threshold <- 0.95

# Identify syndicated articles based on the threshold
pairs <- expand.grid(Article1 = clean_corpus$GOID, Article2 = clean_corpus$GOID) %>%
  filter(Article1 < Article2) %>%  # Ensure unique pairs and exclude self-comparisons
  mutate(
    Similarity = cosine_sim[cbind(
      match(Article1, clean_corpus$GOID),
      match(Article2, clean_corpus$GOID)
    )]
  ) %>%
  filter(Similarity >= similarity_threshold)



str(pairs)
head(pairs, n = 10)


# Initialize group tracking
pairs2 <- pairs %>%
  mutate(Group = NA_integer_)

head(pairs2)



group_id <- 1

# Assign groups iteratively
while (any(is.na(pairs2$Group))) {
  # Get the first ungrouped pair
  ungrouped <- pairs2 %>% filter(is.na(Group)) %>% slice(1)
  current_group <- c(ungrouped$Article1, ungrouped$Article2)
  
  # Expand the group to include all connected articles
  repeat {
    new_members <- pairs2 %>%
      filter(is.na(Group) & (Article1 %in% current_group | Article2 %in% current_group)) %>%
      select(Article1, Article2) %>%
      unlist() %>%
      unique()
    
    if (all(new_members %in% current_group)) break  # No new members
    current_group <- unique(c(current_group, new_members))
  }
  
  # Assign group ID to all pairs in this group
  pairs2 <- pairs2 %>%
    mutate(Group = ifelse(Article1 %in% current_group | Article2 %in% current_group, group_id, Group))
  
  group_id <- group_id + 1  # Increment group ID
}


str(pairs2)
head(pairs2)
range(pairs2$Group)



group1 <- pairs2 %>% 
  select("GOID" = Article1, Group) %>% 
  distinct()

str(group1)
head(group1)


group2 <- pairs2 %>% 
  select("GOID" = Article2, Group) %>% 
  distinct()

str(group2)
head(group2)


syndicated_groups <- rbind(group2, group1) %>% distinct()

str(syndicated_groups)
head(syndicated_groups)


count <- syndicated_groups %>% 
  count(Group) %>% 
  arrange(desc(n))

str(count)
head(count)





group_check <- syndicated_groups %>% 
  filter(Group == "6")

group_check



# Get syndicated and unique articles
syndicated_ids <- syndicated_groups$GOID

# Syndicated articles
syndicated_articles <- clean_corpus %>%
  filter(GOID %in% syndicated_ids) %>% 
  inner_join(syndicated_groups, by = "GOID")



str(syndicated_ids)
str(syndicated_articles)



write.csv(syndicated_articles,"syndicated_articles.csv")


# Keep only the earliest article from each group, resolving ties by Article_ID
earliest_syndicated <- syndicated_articles %>%
  group_by(Group) %>%
  slice_min(order_by = Date, with_ties = TRUE) %>% # Keep ties for the same date
  slice_min(order_by = GOID, n = 1) %>%      # Pick one article (arbitrarily by ID)
  ungroup()


str(earliest_syndicated)


earliest_syndicated2 <- earliest_syndicated %>% 
  select(-Group)

str(earliest_syndicated2)


# Unique articles
unique_articles <- clean_corpus %>%
  filter(!GOID %in% syndicated_ids)

str(unique_articles)


# Combine unique articles with earliest syndicated articles
final_corpus <- bind_rows(unique_articles, earliest_syndicated2) %>%
  arrange(Date) %>% 
  select(GOID, Title, Date, Text)

str(final_corpus)
head(final_corpus)
