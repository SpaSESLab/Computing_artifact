
#But SentenceTransformer("all-mpnet-base-v2") itself is a Python object, 
#so in R i accessed it through reticulate rather than calling it natively. 
#The model is available as the Hugging Face Sentence Transformers model sentence-transformers/all-mpnet-base-v2, 
#which produces 768-dimensional embeddings suitable for semantic similarity, and cosine similarity is the standard way to compare them


library(reticulate)
library(readxl)
library(dplyr)

# installing Python packages from R:
#py_install(c("sentence-transformers", "scikit-learn", "pandas"), pip = TRUE)

st <- import("sentence_transformers")
sk <- import("sklearn.metrics.pairwise")
np <- import("numpy")

# Load the model from Python inside R
model <- st$SentenceTransformer("sentence-transformers/all-mpnet-base-v2")

# Read Excel file
xlsx <- read_excel("CODE_BOOK.xlsx", sheet = "GPT_CODED")
human <- read_excel("CODE_BOOK.xlsx", sheet = "HUMAN_CODED")

# Keep GOID + EVENT and rename for clarity
human <- human %>%
  select(GOID, EVENT) %>%
  rename(EVENT_HUMAN = EVENT)

gpt <- xlsx %>%
  select(GOID, EVENT) %>%
  rename(EVENT_GPT = EVENT)

#Merge and keep first 13 rows if you want
df <- inner_join(human, gpt, by = "GOID") %>%
  filter(!is.na(EVENT_HUMAN), !is.na(EVENT_GPT)) %>%
  mutate(
    EVENT_HUMAN = as.character(EVENT_HUMAN),
    EVENT_GPT = as.character(EVENT_GPT)
  ) %>%
  slice(1:13)

#Encoding texts
human_emb <- model$encode(df$EVENT_HUMAN, normalize_embeddings = TRUE)
gpt_emb   <- model$encode(df$EVENT_GPT, normalize_embeddings = TRUE)

#Row-by-row cosine similarity
scores <- mapply(function(i) {
  as.numeric(sk$cosine_similarity(human_emb[i, , drop = FALSE],
                                  gpt_emb[i, , drop = FALSE])[1, 1])
}, seq_len(nrow(df)))

df$cosine_similarity <- scores

print(df %>% select(GOID, EVENT_HUMAN, EVENT_GPT, cosine_similarity))


#write.csv(df, "gpt_human_event_similarity.csv", row.names = FALSE)


