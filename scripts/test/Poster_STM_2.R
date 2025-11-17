

library("stringr")
library("quanteda")
library("stm")
library("tidytext")
library("dplyr")   # <--- Needed for arrange(), group_by(), %>%


# Read in the data
wildlife <- read.csv(file = "/Users/agnesnamyalo/Downloads/corpus_for_analysis.csv")

head(wildlife, 5)


# Tokenization & cleaning
tokens <- wildlife$Text %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))


#applying relative pruning
dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)


#MakING sure that we did remove all feature with little informative value.
#To check this, we quickly have a look at the top features in our corpus (after preprocessing):

topfeatures(dfm, n = 20, scheme = "docfreq")

#Removing irrelevant words

dfm <- dfm_remove(dfm, c("s","pm","bear","bears","grizzlies","grizzly"))
dfm

dfm_stm <- convert(dfm, to = "stm") #getting the DFM into the right format to use the stm package


# Define candidate topics to evaluate
k_values <- seq(5, 20, by = 5)


# Initialize lists for storing evaluation metrics (optional)
heldout <- make.heldout(dfm_stm$documents, dfm_stm$vocab)
results <- list()

for (k in k_values) {
  cat("Fitting STM with K =", k, "\n")
  stm_fit <- stm(documents = dfm_stm$documents, 
                 vocab = dfm_stm$vocab, 
                 K = k, 
                 prevalence = ~1,    # or add metadata if available
                 max.em.its = 75,
                 data = dfm_stm$meta,
                 init.type = "Spectral")
  
  # Evaluate heldout likelihood or other metrics
  heldout_result <- eval.heldout(stm_fit, heldout$missing)
  
  # Store results for comparison
  results[[as.character(k)]] <- list(stm_fit = stm_fit, heldout = heldout_result)
}


# Summarize heldout likelihoods for each k
heldout_scores <- sapply(results, function(x) x$heldout$expected.heldout)
print(heldout_scores)

# Plot heldout likelihoods against K to find optimal number of topics
plot(k_values, heldout_scores, type = "b", xlab = "Number of Topics (K)", 
     ylab = "Held-out Likelihood", main = "STM Topic Number Selection")



library("stm")
model <- stm(documents = dfm_stm$documents,
             vocab = dfm_stm$vocab, 
             K = 10,
             verbose = TRUE)


plot(model)


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
          "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Wildlife_topwords_probabilities_neww_wayy.csv",
          row.names = FALSE)

# Preview top 10
head(top_terms, 10)



#This helps to get the CSV returning all columns(with the original)
# --- After fitting your STM model ---

# Get document-topic probabilities (theta matrix: rows = docs, cols = topics)
theta <- model$theta  

# Assign each document to its most likely topic
assigned_topics <- apply(theta, 1, which.max)

# Use your top probability words for each topic
labels <- labelTopics(model, n = 50)  # top 10 words per topic
topic_topwords <- apply(labels$prob, 1, function(words) paste(words, collapse = ", "))

# Add columns to your original wildlife dataframe
wildlife$Assigned_Topic <- assigned_topics
wildlife$Topwords <- topic_topwords[assigned_topics]

# Save updated dataframe with all columns
write.csv(
  wildlife,
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/Wildlife_with_topics_new_way.csv",
  row.names = FALSE
)

