


library(stringr)
library(quanteda)
library(stm)
library(tidytext)
library(dplyr)
library(tidyverse)
library(tm)


DTM <- readRDS("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/clean_dtm.rds")

inspect(DTM)



# Read in the data
wildlife <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Artifact_wildlife_for_STM.csv", stringsAsFactors = FALSE)

head(wildlife, 5)


wildlife$cleaned_text <- as.character(wildlife$cleaned_text)

# Tokenization & cleaning with quanteda
toks <- tokens(
  wildlife$cleaned_text,
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove("")    # Remove any empty strings

# Construct Document-Feature Matrix
dfm_obj <- dfm(toks)

# Prune features based on relative document frequency
dfm_obj <- dfm_trim(dfm_obj, min_docfreq = 0.005, max_docfreq = 0.99, docfreq_type = "prop", verbose = TRUE)

# Check top features after preprocessing
topfeatures(dfm_obj, n = 15, scheme = "docfreq")

# Removing specific irrelevant words
dfm_obj <- dfm_remove(dfm_obj, c("s", "pm")) # Add more terms as needed

# Converting DFM to STM input format
dfm_stm <- convert(dfm_obj, to = "stm")

# Fit STM model
model <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = 10,
  verbose = TRUE
)

# Plotting model topics
plot(model)

# Extract keywords per topic
labels <- labelTopics(model, n=10)


# Get top-10 keywords per topic 
news_keywords <- unique(unlist(labels$prob[, 1:10]))

# Optionally: Save as a vector, or print
print(news_keywords)


# Convert the keywords matrix to a data frame for easier export
topic_keywords_df <- as.data.frame(labels$prob)
colnames(topic_keywords_df) <- paste0("Top", 1:10)
topic_keywords_df$Topic <- paste0("Topic", 1:nrow(topic_keywords_df))

# Reorder columns: Topic, then keywords
topic_keywords_df <- topic_keywords_df[, c("Topic", paste0("Top", 1:10))]

# Save as CSV
write.csv(topic_keywords_df, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords.csv", row.names = FALSE)


#EXPORTING KEYWORDS AND TOPICS AND WEIGHTS


# labels$prob gives just the top N words, but for weights, use model$beta
# model$beta is a list of matrices (usually length 1, for single content covariate)
# First, access the matrix (topics x words, in log-prob space)
beta <- exp(model$beta$logbeta[[1]]) # Convert log-probabilities to probabilities

# Get vocab names
keywords <- model$vocab

# Build dataframe: long format with topic, word, and weight (probability)
beta_df <- map_dfr(1:nrow(beta), function(topic_idx) {
  tibble(
    topic   = paste0(topic_idx),
    keyword = keywords,
    weight  = beta[topic_idx, ]
  )
})

# (Optional) Save only the top N words per topic
top_n <- 10
top_keywords_df <- beta_df %>%
  group_by(topic) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  slice(1:top_n) %>%
  ungroup()

# Save to CSV: each row is topic, keyword, weight
write.csv(top_keywords_df, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/news_stm_topic_keywords_with_weights.csv", row.names = FALSE)




#Getting the best K and also to rate the top most words


#After printing or saving your keywords from the news articles (e.g., news_keywords), 
#i should use the unique set of these keywords as your custom vocabulary for the Congress data. 
#This ensures that your Congress topic modeling is focused on the terms most relevant to public/media discourse.

#Preprocess Congress documents:
#Clean and tokenize Congress texts just as you did for news articles.



# Assuming your keywords matrix is called 'labels$prob'

# Flatten keywords matrix to a vector and get all unique terms
keyword_vector <- unique(as.vector(labels$prob))
# Remove any empty strings, if present
keyword_vector <- keyword_vector[nchar(keyword_vector) > 0]
print(keyword_vector)


#Read and Prepare Your Congress Data

congress <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Artifact_congress_for_STMM.csv", stringsAsFactors = FALSE)

# Ensure the text column is character
congress$cleaned_text <- as.character(congress$cleaned_text)

(congress$cleaned_text)

#Filter Rows Using the Keywords
#You want to keep only rows where cleaned_text contains any of your keywords:


#Filter Your Congress Data Using All These Keywords

library(dplyr)
library(stringr)

# Read Congress dataset
congress <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Artifact_congress_for_STMM.csv", stringsAsFactors = FALSE)

# Character text column
congress$cleaned_text <- as.character(congress$cleaned_text)

# Build regex: matches any keyword as a word, case-insensitive
keyword_pattern <- paste0("\\b(", paste(keyword_vector, collapse = "|"), ")\\b")

# Filter dataset for any row containing any STM topic keyword
congress_filtered <- congress %>%
  filter(str_detect(cleaned_text, regex(keyword_pattern, ignore_case = TRUE)))

# Check result
cat("Number of matched rows:", nrow(congress_filtered), "\n")
head(congress_filtered$cleaned_text, 5)


# Save as CSV
write.csv(congress_filtered,
          "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_filtered.csv",
          row.names = FALSE)


# Export the Filtered Dataset
#write.csv(congress_filtered, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Congress_filtered_by_STM_keywords.csv", row.names = FALSE)


#PERFORMING STM ON THE FILTERED CONGRESS DATASET


congress_filtered$cleaned_text <- as.character(congress_filtered$cleaned_text)

# Tokenization & cleaning with quanteda
toks <- tokens(
  congress$cleaned_text,
  what = "word",
  remove_punct = TRUE,
  remove_numbers = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove("")    # Remove any empty strings

# Construct Document-Feature Matrix
dfm_obj <- dfm(toks)

# Prune features based on relative document frequency
dfm_obj <- dfm_trim(dfm_obj, min_docfreq = 0.005, max_docfreq = 0.99, docfreq_type = "prop", verbose = TRUE)

# Check top features after preprocessing
topfeatures(dfm_obj, n = 15, scheme = "docfreq")

# Removing specific irrelevant words
dfm_obj <- dfm_remove(dfm_obj, c(" ment", "tion","ing","congressional","congress","page","print","vote","year")) # Add more terms as needed

# Converting DFM to STM input format
dfm_stm <- convert(dfm_obj, to = "stm")

# Fit STM model
model <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = 10,
  verbose = TRUE
)

# Plotting model topics
plot(model)

# Extract keywords per topic (top 10 per topic)
labels <- labelTopics(model, n = 10)

# Convert STM keyword matrix to a data frame
congress_topic_keywords_df <- as.data.frame(labels$prob)
colnames(congress_topic_keywords_df) <- paste0("Top", 1:10)
congress_topic_keywords_df$Topic <- paste0("Topic", 1:nrow(congress_topic_keywords_df))

# Reorder columns so that 'Topic' comes first
congress_topic_keywords_df <- congress_topic_keywords_df[, c("Topic", paste0("Top", 1:10))]

# Save as CSV
write.csv(congress_topic_keywords_df,
          "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_stm_topic_keywords.csv",
          row.names = FALSE)


#getting the keywords and their weights

library(dplyr)
library(tidyr)
library(purrr)

# Get the per-topic, per-word probability matrix (log scale, so exponentiate)
beta <- exp(model$beta$logbeta[[1]])   # topics (rows) × words (columns)
vocab <- model$vocab

# Gather into long format: topic, keyword, weight
keywords_long <- map_dfr(
  1:nrow(beta),
  function(i) {
    tibble(
      topic = paste0(i),
      keyword = vocab,
      weight = beta[i, ]
    )
  }
)

# Optionally, keep only the top N keywords per topic for easier browsing
top_n <- 10
keywords_topn <- keywords_long %>%
  group_by(topic) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  slice_head(n = top_n) %>%
  ungroup()

# Save as CSV: each row is (topic, keyword, weight)
write.csv(keywords_topn, "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/congress_stm_keywords_weights.csv", row.names = FALSE)





