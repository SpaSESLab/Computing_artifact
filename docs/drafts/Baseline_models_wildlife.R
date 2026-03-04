
#LIBRARIES
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

#Reading data

df <- read.csv("wildlife_news_articles.csv", stringsAsFactors = FALSE)

# Take only first 300 rows
df_test <- df %>% slice(1:300)

#Reading first 5 rows and all columns(columns--document_id, extract_number, extract_text)
#head(df, 5)


# 2. SET API KEY (Store in .Renviron in production)

#API KEY  
Sys.setenv(OPENAI_API_KEY = "sk-proj")  #Provided on request


#SAFE NULL OPERATOR

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#BASELINE MODELS
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

# FIRST NPF PROMPT BUILDER (ESA-CONSTRAINED)

build_npf_prompt <- function(paragraph) {
  
  paste0(
    "You are coding text related to the Endangered Species Act (ESA) using the Narrative Policy Framework (NPF).

IMPORTANT:
Extract narrative elements ONLY if the paragraph concerns:
- The Endangered Species Act
- Federal endangered or threatened species protections
- Species listing or delisting decisions
- Critical habitat designation
- Federal wildlife regulatory actions under endangered species law
- Legal or legislative disputes specifically about ESA protections

If the paragraph does NOT concern ESA-related policy, return:

{
  \"event\": null,
  \"story\": null,
  \"moral\": null
}

If the paragraph IS ESA-related, extract:

1. Event – The specific ESA-related policy action or inaction being described 
   (e.g., bill introduction, amendment, moratorium, regulatory decision, listing decision, court ruling).

2. Story – The causal plot that links actors, actions, and consequences 
   (who does what, causing what outcome, for whom).

3. Moral – The implied policy prescription or evaluation 
   (what should be done, or whether the policy/action is good or bad).

Return STRICTLY a valid JSON object with this schema:

{
  \"event\": \"...\",
  \"story\": \"...\",
  \"moral\": \"...\"
}

Now code this paragraph:

\"\"\"", paragraph, "\"\"\"\n"
  )
}


# SECOND PROMPT BUILDER (ESA-CONSTRAINED STORY ELEMENTS)

build_story_prompt <- function(paragraph) {
  
  paste0(
    "You are coding text related to the Endangered Species Act (ESA).

IMPORTANT:
Extract narrative elements ONLY if the paragraph concerns:
- The Endangered Species Act
- Federal endangered or threatened species protections
- Species listing/delisting decisions
- Critical habitat designation
- Federal regulatory actions under ESA
- ESA-related litigation or legislative reform

If the paragraph does NOT concern ESA-related policy, return:

{
  \"setting\": null,
  \"characters\": null,
  \"plot\": null,
  \"moral\": null
}

If the paragraph IS ESA-related, extract:

1. Setting – The institutional or political context where the action occurs 
   (e.g., congressional hearing, federal agency decision, court ruling, regulatory process).

2. Characters – The key actors involved 
   (e.g., legislators, agencies, environmental groups, landowners, industries, courts).

3. Plot – The sequence of actions and consequences 
   (who does what, and what results).

4. Moral – The implied lesson, policy message, or evaluative takeaway.

Return STRICTLY a valid JSON object with this schema:

{
  \"setting\": \"...\",
  \"characters\": \"...\",
  \"plot\": \"...\",
  \"moral\": \"...\"
}

Now analyze this paragraph:

\"\"\"", paragraph, "\"\"\""
  )
}

#GENERIC OPENAI CALL

call_openai_json <- function(prompt_text, timeout = 30) {
  
  body <- list(
    model = "gpt-4o-mini",
    temperature = 0,
    max_tokens = 300,
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = "Respond ONLY with valid JSON."),
      list(role = "user", content = prompt_text)
    )
  )
  
  resp <- tryCatch(
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(timeout)
    ),
    error = function(e) return(NULL)
  )
  
  if (is.null(resp) || httr::http_error(resp)) return(NULL)
  
  content_raw <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(content_raw, simplifyVector = FALSE)
  
  if (is.null(parsed$choices)) return(NULL)
  
  msg <- parsed$choices[[1]]$message$content
  
  tryCatch(
    jsonlite::fromJSON(msg, simplifyVector = FALSE),
    error = function(e) return(NULL)
  )
}


#EXTRACTION WRAPPERS

extract_npf <- function(paragraph) {
  
  result <- call_openai_json(build_npf_prompt(paragraph))
  
  if (is.null(result)) {
    return(list(event = NA, story = NA, moral = NA))
  }
  
  list(
    event = paste(result$event %||% NA, collapse = "; "),
    story = paste(result$story %||% NA, collapse = "; "),
    moral = paste(result$moral %||% NA, collapse = "; ")
  )
}

extract_story_elements <- function(paragraph) {
  
  result <- call_openai_json(build_story_prompt(paragraph))
  
  if (is.null(result)) {
    return(list(setting = NA, characters = NA, plot = NA, moral_story = NA))
  }
  
  list(
    setting = paste(result$setting %||% NA, collapse = "; "),
    characters = paste(result$characters %||% NA, collapse = "; "),
    plot = paste(result$plot %||% NA, collapse = "; "),
    moral_story = paste(result$moral %||% NA, collapse = "; ")
  )
}


final_df <- df_test %>%
  mutate(
    baseline_esa = map_int(cleaned_text, baseline_any_esa_keyword),
    baseline_causal = map_int(cleaned_text, baseline_contains_cause_word),
    npf = map(cleaned_text, extract_npf),
    story_struct = map(cleaned_text, extract_story_elements)
  ) %>%
  mutate(
    event = map_chr(npf, "event"),
    story = map_chr(npf, "story"),
    moral = map_chr(npf, "moral"),
    setting = map_chr(story_struct, "setting"),
    characters = map_chr(story_struct, "characters"),
    plot = map_chr(story_struct, "plot"),
    moral_story = map_chr(story_struct, "moral_story")
  ) %>%
  select(-npf, -story_struct)

head(final_df)



#write_csv(final_df, "narrative_coded_results_with_baselines_widlife_100.csv")

write_csv(final_df, "narrative_coded_results_with_baselines_widlife_300.csv")

#Loading my wildlife csv
library(dplyr)
library(stringr)
library(readr)

#final_df <- read_csv("narrative_coded_results_with_baselines_widlife_100.csv")

final_df <- read_csv("narrative_coded_results_with_baselines_widlife_300.csv")
#PUTTING EXTRACTED EVENT INTO CATEGORIES


final_df <- final_df %>%
  mutate(event_type = case_when(
    
    # Listing / Delisting
    str_detect(tolower(event), "list") & 
      !str_detect(tolower(event), "delist") ~ "Listing",
    
    str_detect(tolower(event), "delist") ~ "Delisting",
    
    # Habitat-related
    str_detect(tolower(event), "habitat") ~ "Habitat Regulation",
    
    # Conservation agreements
    str_detect(tolower(event), "agreement|conservation plan") ~ "Conservation Agreement",
    
    # Petitions & legal action
    str_detect(tolower(event), "petition|lawsuit|file|sue|court") ~ "Litigation / Petition",
    
    # Congressional / legislative reform
    str_detect(tolower(event), "bill|amendment|reform|rollback") ~ "Legislative Reform",
    
    # Regulatory decision (agency action)
    str_detect(tolower(event), "regulator|agency|decision|rule") ~ "Agency Decision",
    
    # Industry conflict
    str_detect(tolower(event), "drilling|logging|tower|development|land use") ~ "Industry Conflict",
    
    # Climate-related ESA action
    str_detect(tolower(event), "climate|global warming") ~ "Climate-Linked ESA",
    
    TRUE ~ "Other"
  ))

table(final_df$event_type)
prop.table(table(final_df$event_type))

#Moral Direction Classification
##OUTTING EXTRACTED MORALS INTO CATEGORIES

final_df <- final_df %>%
  mutate(moral_direction = case_when(
    
    # Strong protection language
    str_detect(tolower(moral),
               "protect|conservation|preserve|safeguard|endangered|habitat protection") 
    ~ "Pro-protection",
    
    # Climate urgency framing
    str_detect(tolower(moral),
               "climate|global warming|urgent|threat") 
    ~ "Climate-Protection",
    
    # Pro-reform / economic emphasis
    str_detect(tolower(moral),
               "reform|rollback|ease|reduce regulation|economic burden|jobs|industry") 
    ~ "Pro-reform / Industry",
    
    # Balanced compromise framing
    str_detect(tolower(moral),
               "balance|flexible|accommodate|both economic and") 
    ~ "Balanced",
    
    # Regulatory critique
    str_detect(tolower(moral),
               "overreach|too strict|burden|restrict") 
    ~ "Anti-Regulation",
    
    TRUE ~ "Other"
  ))

table(final_df$moral_direction)
prop.table(table(final_df$moral_direction))







