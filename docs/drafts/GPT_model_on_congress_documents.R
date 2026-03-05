
#LIBRARIES
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

#Reading data

df_full <- read.csv(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/extracted_outputs_full_dataset.csv",
  stringsAsFactors = FALSE
)

# Take only first 300 rows
df <- df_full %>% slice(1:300)

# SET API KEY (Store in .Renviron in production)

#API KEY  --provided on requesr
Sys.setenv(OPENAI_API_KEY = "sk-proj")

#SAFE NULL OPERATOR

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#BASELINE MODELS
#to know if my  results are not just picking up obvious keywords
baseline_any_esa_keyword <- function(text) {
  
  kws <- c(
    "endangered species",
    "esa",
    "critical habitat",
    "listing",
    "delisting"
  )
  
  t <- tolower(text)
  as.integer(any(str_detect(t, kws)))
}

baseline_contains_cause_word <- function(text) {
  
  cues <- c(
    "because",
    "therefore",
    "so that",
    "as a result",
    "leads to",
    "results in",
    "causes"
  )
  
  t <- tolower(text)
  as.integer(any(str_detect(t, cues)))
}


#first NPF PROMPT BUILDER
build_npf_prompt <- function(paragraph) {
  
  paste0(
    "You are coding congressional text using the Narrative Policy Framework (NPF).

From the paragraph below, extract:

1. Event – The specific ESA-related policy action or inaction being described 
   (e.g., bill introduction, amendment, moratorium, regulatory decision).
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

If the paragraph does NOT contain a clear policy narrative in NPF terms 
(i.e., no identifiable event, causal story, or moral), return:

{
  \"event\": null,
  \"story\": null,
  \"moral\": null
}

The example below is only to illustrate the structure and style of event, story, and moral.
Do not copy their content or assume any particular stance. Always base your answer only on
the paragraph provided, even if it supports species protection, economic interests, both, or neither.

Example:

Example 1 (pro-protection narrative)
Input paragraph:
\"Biologists testified that delay in listing the sage grouse under the ESA would push the species
closer to extinction, arguing that immediate federal protections are needed to prevent irreversible
habitat loss.\"

Output:
{
  \"event\": \"Delay in listing the sage grouse under the ESA\",
  \"story\": \"Postponing federal protections increases the risk of extinction due to ongoing habitat loss\",
  \"moral\": \"List the sage grouse immediately and strengthen habitat protections\",
}


Now code this paragraph:

\"\"\"", paragraph, "\"\"\"\n"
  )
}


#SECOND prompt builder (classic narrative elements)

build_story_prompt <- function(paragraph) {
  paste0(
    "You are analyzing a congressional paragraph as a classic narrative story.\n\n",
    
    "From the paragraph below, extract:\n\n",
    
    "1. Setting – The institutional or political context where the action occurs ",
    "(e.g., congressional hearing, committee markup, federal agency decision).\n",
    
    "2. Characters – The key actors involved (e.g., legislators, agencies, ranchers, environmental groups).\n",
    
    "3. Plot – The sequence of actions and consequences (who does what, and what results).\n",
    
    "4. Moral – The implied lesson, policy message, or evaluative takeaway.\n\n",
    
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    
    "{\n",
    "  \"setting\": \"...\",\n",
    "  \"characters\": \"...\",\n",
    "  \"plot\": \"...\",\n",
    "  \"moral\": \"...\"\n",
    "}\n\n",
    
    "If the paragraph does not contain a clear narrative structure, return:\n\n",
    
    "{\n",
    "  \"setting\": null,\n",
    "  \"characters\": null,\n",
    "  \"plot\": null,\n",
    "  \"moral\": null\n",
    "}\n\n",
    
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
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


#APPLY EVERYTHING TO DATAFRAME

final_df_congress <- df %>%
  mutate(
    baseline_esa = map_int(extract_text, baseline_any_esa_keyword),
    baseline_causal = map_int(extract_text, baseline_contains_cause_word),
    npf = map(extract_text, extract_npf),
    story_struct = map(extract_text, extract_story_elements)
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

#RESULTS


head(final_df_congress)


#saving output

#write_csv(final_df_congress, "narrative_coded_results_with_baselines_congress_100.csv")
write_csv(final_df_congress, "narrative_coded_results_with_baselines_congress_300.csv")

#Reading the csv file

#final_df_congress <- read_csv("narrative_coded_results_with_baselines_congress_100.csv")

final_df_congress <- read_csv("narrative_coded_results_with_baselines_congress_300.csv")

#Rule-based classification--PUTTING EXTRACTED EVENT INTO CATEGORIES

final_df_congress <- final_df_congress %>%
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
    
    # Congressional/legislative reform
    str_detect(tolower(event), "bill|amendment|reform|rollback") ~ "Legislative Reform",
    
    # Regulatory decision (agency action)
    str_detect(tolower(event), "regulator|agency|decision|rule") ~ "Agency Decision",
    
    # Industry conflict
    str_detect(tolower(event), "drilling|logging|tower|development|land use") ~ "Industry Conflict",
    
    # Climate-related ESA action
    str_detect(tolower(event), "climate|global warming") ~ "Climate-Linked ESA",
    
    TRUE ~ "Other"
  ))

table(final_df_congress$event_type)
prop.table(table(final_df_congress$event_type))


#Moral Direction Classification
##OUTTING EXTRACTED MORALS INTO CATEGORIES

final_df_congress <- final_df_congress %>%
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

table(final_df_congress$moral_direction)
prop.table(table(final_df_congress$moral_direction))

#Having all outputs in the full dataset

mapped_congress <- final_df_congress %>%
  select(
    document_id,
    extract_number,
    extract_text,
    baseline_esa,
    baseline_causal,
    event,
    story,
    moral,
    setting,
    characters,
    plot,
    moral_story,
    event_type,
    moral_direction
  )

head(mapped_congress)

write.csv(
  mapped_congress,
  "congress_document_outputs.csv",
  row.names = FALSE
)



