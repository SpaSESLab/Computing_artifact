library("stringr")
library("quanteda")
library("stm")
library("tidytext")
library("dplyr")   # <--- Needed for arrange(), group_by(), %>%

# Read in the data
wildlife <- read.csv(file = "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/wildlife_for_R.csv")

# Tokenization & cleaning
tokens <- wildlife$final_text %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))

# Apply relative pruning
dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)

# Remove irrelevant words
dfm <- dfm_remove(dfm, c("s","pm"))

# Convert to stm format
dfm_stm <- convert(dfm, to = "stm")

# Fit STM model
model <- stm(documents = dfm_stm$documents,
             vocab = dfm_stm$vocab, 
             K = 15,
             verbose = TRUE)

# ----------------------------
# Extract word probabilities per topic
# ----------------------------
beta <- tidy(model, matrix = "beta")  
# 'beta' gives probabilities of words per topic

# Keep top 20 words per topic by probability
top_terms <- beta %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  arrange(topic, desc(beta))   # fixed

# ----------------------------
# Save results
# ----------------------------
write.csv(top_terms,
          "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Wildlife_topwords_probabilities_newwwayy.csv",
          row.names = FALSE)

# Preview top 10
head(top_terms, 10)
