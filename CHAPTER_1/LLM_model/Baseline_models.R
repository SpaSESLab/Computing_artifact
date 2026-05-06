
#title: "BASELINE MODELS"
#Purpose: For validation of models

  
#Purpose:
# -Detect ESA mentions
# -Detect causal language
# -Provide validation baseline

library(stringr)

baseline_any_esa_keyword <- function(text) {
  
  kws <- c(
    "endangered species act",
    "\\besa\\b",
    "section 7",
    "section 9",
    "critical habitat",
    "listing",
    "delisting",
    "listed as endangered",
    "listed as threatened",
    "threatened species",
    "endangered species",
    "federal protection",
    "federal protections",
    "under federal law",
    "protected under federal",
    "u.s. fish and wildlife service",
    "fish and wildlife service",
    "federal wildlife officials",
    "national marine fisheries service",
    "habitat designation",
    "species protection",
    "wildlife protections"
  )
  
  t <- tolower(text)
  
  pattern <- paste(kws, collapse = "|")
  
  as.integer(str_detect(t, pattern))
}


baseline_contains_cause_word <- function(text) {
  
  cues <- c(
    "because",
    "therefore",
    "as a result",
    "leads to",
    "results in",
    "causes",
    "so that",
    "due to",
    "in response to",
    "following",
    "after",
    "would lead to",
    "could lead to",
    "may result in",
    "is expected to",
    "threatens",
    "jeopardizes",
    "puts at risk",
    "risks",
    "could harm",
    "would harm",
    "prevents",
    "protects",
    "ensures",
    "safeguards",
    "blocks",
    "restricts",
    "limits",
    "requires"
  )
  
  t <- tolower(text)
  
  pattern <- paste(cues, collapse = "|")
  
  as.integer(str_detect(t, pattern))
}
