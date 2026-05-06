#03_creating_dtm

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
library(lubridate)
library(text2vec)



# Read in the original CSV file (replace blank.csv with csv name)
#final_corpus <- read.csv("clean_original_docs_corpus.csv")

final_corpus <- read.csv("clean_text_corpus.csv")

str(final_corpus)


dd <- final_corpus %>% 
  select(c("GOID", "Text"))

colnames(dd) <- c("doc_id", "text")

str(dd)


s_corpus <- Corpus(DataframeSource(dd))
inspect(s_corpus[1:1])


# Create a Document-Term Matrix using term frequency weighting
dtm_tf <- DocumentTermMatrix(s_corpus, control = list(weighting = weightTf))

# Check the size of the DTM
cat("Number of documents in DTM:", nDocs(dtm_tf), "\n")
cat("Number of terms in DTM:", nTerms(dtm_tf), "\n") 

inspect(dtm_tf)


# Remove the species from the dtm
dtm_filtered_1 <- dtm_tf[, !colnames(dtm_tf) %in% c("endanger", "species", "act","u.s.")]

inspect(dtm_filtered_1)



# Calculate document frequency (the number of documents each term appears in)
doc_freq <- colSums(as.matrix(dtm_filtered_1) > 0)

# Set the threshold (85% of the documents)
threshold <- 0.85 * nrow(dtm_filtered_1)

# Remove terms that appear in more than 85% of the documents
dtm_filtered_2 <- dtm_filtered_1[, doc_freq <= threshold]



inspect(dtm_filtered_2)


empty_docs <- rowSums(as.matrix(dtm_filtered_2)) == 0
print(sum(empty_docs))
empty_doc_ids <- rownames(dtm_filtered_2)[empty_docs]
print(empty_doc_ids)


term_frequencies <- slam::col_sums(dtm_filtered_2)
terms_once_across_all <- names(term_frequencies[term_frequencies == 1])
print(terms_once_across_all)


document_counts <- slam::col_sums(dtm_filtered_2 > 0)
terms_in_one_document <- names(document_counts[document_counts == 1])
print(terms_in_one_document)


terms_to_remove <- union(terms_once_across_all, terms_in_one_document)
print(terms_to_remove)


dtm_filtered_3 <- dtm_filtered_2[, !(colnames(dtm_filtered_2)%in% terms_to_remove)]
inspect(dtm_filtered_3)

terms <- Terms(dtm_filtered_3)
long_terms <- terms[nchar(terms) >= 18]
long_terms


empty_docs <- rowSums(as.matrix(dtm_filtered_3)) == 0
print(sum(empty_docs))
empty_doc_ids <- rownames(dtm_filtered_3)[empty_docs]
print(empty_doc_ids)


dtm_filtered_4 <- dtm_filtered_3[!empty_docs, ]
inspect(dtm_filtered_4)



saveRDS(
  dtm_filtered_4,"clean_dtm_new.rds")

