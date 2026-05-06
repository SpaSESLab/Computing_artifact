#  STM FOR ESA NEWS ARTICLES — USING PRE-BUILT DTM

#  Input     : clean_dtm_new.rds, DocumentTermMatrix (tm package) with: 445 documents (rows = GOIDs)

#  Goal      : Run STM directly on the pre-built DTM,map topics to NPF event groups


# loading packages 

library(stm)          # Structural Topic Model
library(tm)           # For DocumentTermMatrix class
library(tidyverse)    # dplyr, ggplot2, stringr, 
library(Matrix)       # Sparse matrix conversion
library(ggplot2)      # Visualisation
library(scales)       # Axis formatting
library(reshape2)     # Data reshaping
library(RColorBrewer) # Colour palettes


#Load the pre-built DTM 

dtm <- readRDS("clean_dtm_new.rds")



# Add metadata (year covariate) 
# STM needs a metadata data frame aligned with DTM rows.
# We reconstruct year from the LLM results CSV using GOID.
# If you don't have the CSV handy, use the fallback below.

# OPTION A: Merge from your LLM results CSV (recommended)
# --------------------------------------------------------
# This ensures your STM metadata aligns exactly with your
# LLM extraction results for the comparison in Step 11.

llm_df <- read.csv("narrative_coded_results_with_baselines_widlife_22.csv",
                   stringsAsFactors = FALSE)

llm_df$year <- as.integer(substr(llm_df$Date, 1, 4))

# Build metadata aligned with DTM row order
dtm_goids <- as.character(rownames(dtm))
meta_df <- data.frame(
  GOID = dtm_goids,
  stringsAsFactors = FALSE
) %>%
  left_join(
    llm_df %>%
      select(GOID, Date, year, event_group, baseline_causal,
             event, story, moral) %>%
      mutate(GOID = as.character(GOID)),
    by = "GOID"
  )

cat("\nMetadata aligned with DTM rows:", nrow(meta_df), "\n")
cat("Year range:", min(meta_df$year, na.rm = TRUE),
    "to", max(meta_df$year, na.rm = TRUE), "\n")
cat("Matched to LLM results:", sum(!is.na(meta_df$event_group)), "\n")

# OPTION B: Year from ESA full CSV (if LLM CSV not available)
# -----------------------------------------------------------
# esa_df <- read.csv("esa_9_30_2025.csv", stringsAsFactors = FALSE)
# esa_df$year <- as.integer(substr(esa_df$Date, 1, 4))
# meta_df <- data.frame(GOID = dtm_goids) %>%
#   left_join(esa_df %>% select(GOID, year) %>%
#               mutate(GOID = as.character(GOID)), by = "GOID")

# Fill any missing years with median year
median_year <- median(meta_df$year, na.rm = TRUE)
meta_df$year[is.na(meta_df$year)] <- median_year
meta_df$year <- as.integer(meta_df$year)


#Convert DTM → STM format 

stm_input <- readCorpus(dtm, type = "slam")

docs  <- stm_input$documents
vocab <- stm_input$vocab


# Choose number of topics K 
# Your LLM identified 7–8 distinct event groups (NPF categories).
# Test K = 5, 7, 9, 11 to find the best match.
#
# searchK metrics to prioritise:
#   Held-out likelihood  → higher = better model fit
#   Semantic coherence   → higher = more interpretable topics
#   Residuals            → lower = better
#   Exclusivity          → higher = more distinct topics

set.seed(2025)

cat("\nRunning searchK (this takes ~5–10 minutes)...\n")

k_search <- searchK(
  documents  = docs,
  vocab      = vocab,
  K          = c(5, 7, 8, 9, 11),
  prevalence = ~ year,
  data       = meta_df,
  verbose    = TRUE
)


saveRDS(k_search, "stm_searchK_results.rds")

# Plot all four diagnostics
plot(k_search)

# ggsave("fig_stm_searchK.png", width = 9, height = 6, dpi = 300)

print(k_search$results)


#Fit final STM model 


K_FINAL <- 8   # <-- To be adjusted based on searchK plot

set.seed(2025)

stm_model <- stm(
  documents  = docs,
  vocab      = vocab,
  K          = K_FINAL,
  prevalence = ~ year,        # topic prevalence varies by year
  data       = meta_df,
  init.type  = "Spectral",    # stable initialisation
  verbose    = TRUE
)

# Save fitted model
saveRDS(stm_model, paste0("stm_esa_dtm_k", K_FINAL, ".rds"))

