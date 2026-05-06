# Load libraries
library(readr)
library(dplyr)
library(tidytext)
library(tm)
library(textmineR)
library(SnowballC)
library(textstem)
library(textclean)
library(stringr)
library(qdapDictionaries)
library(lexicon)
library(here)
library(text2vec)


# Read in the original CSV file (replace blank.csv with csv name)

original_corpus <- read.csv("/Users/agnesnamyalo/Desktop/RESEARCH/DATA/esa_9_30_2025.csv", stringsAsFactors = FALSE)

str(original_corpus)

# Remove rows where Text is empty or contains only whitespace using dplyr
original_corpus <- original_corpus %>%
  filter(nchar(trimws(Text)) > 0)

str(original_corpus)


# Function for preserving abbreviations
preserve_abbreviations <- function(text) {
  matches <- gregexpr("\\b([a-z](?:\\.[a-z])+)\\.?", text, ignore.case = TRUE)
  regmatches(text, matches) <- lapply(regmatches(text, matches), function(abbrev) {
    gsub("\\.", "DOTDOTDOT", abbrev)
  })
  
  return(text)
}

# Function to convert possessive forms
convert_possessives <- function(text) {
  text <- gsub("\\b(\\w+)'s\\b", "\\1 ", text, ignore.case = TRUE)
  text <- gsub("\\b(\\w+)'\\b", "\\1 ", text, ignore.case = TRUE)
  
  return(text)
}


corpus_clean <- original_corpus %>% 
  mutate(
    Text = as.character(Text),
    Text = preserve_abbreviations(Text),
    Text = gsub("\\b\\S*\\.(com|org|gov|edu|htm|net)\\S*\\b", " ", Text, ignore.case = TRUE),
    Text = gsub("\\S*@\\S*", " ", Text, ignore.case = TRUE),
    Text = gsub("\\S*\\d+\\S*", " ", Text, ignore.case = TRUE),
    Text = replace_contraction(Text),
    Text = gsub(paste0("\\b(", paste(stopwords("en"), collapse = "|"), ")\\b"), " ", Text, ignore.case = TRUE),
    Text = gsub(paste0("\\b(", paste(BuckleySaltonSWL, collapse = "|"), ")\\b"), " ", Text, ignore.case = TRUE),
    Text = convert_possessives(Text),
    Text = replace_ordinal(Text, num.paste = TRUE, remove = TRUE),
    Text = replace_number(Text, num.paste = TRUE, remove = TRUE),
    Text = add_comma_space(Text)
  )

lemma_dict <- make_lemma_dictionary(
  corpus_clean$Text,
  engine = "hunspell"
)

final_lemma_dict <- lemma_dict %>% 
  filter(nchar(trimws(token)) > 1)

str(final_lemma_dict)
final_lemma_dict


corpus_more_clean <- corpus_clean  %>% 
  mutate( 
    Text = lemmatize_strings(Text, final_lemma_dict),
    Text = strip(Text, char.keep = NULL, apostrophe.remove = TRUE),
    Text = gsub("DOTDOTDOT", ".", Text, ignore.case = TRUE),
    Text = stripWhitespace(Text)
  )

str(original_corpus)
head(original_corpus)


str(corpus_clean)
head(corpus_clean)


str(corpus_more_clean)
head(corpus_more_clean)


# Save the corpus as a csv file

write_csv(corpus_more_clean, "clean_text_corpus.csv")