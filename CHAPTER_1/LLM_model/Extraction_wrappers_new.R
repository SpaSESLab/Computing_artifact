# Title:   "EXTRACTION WRAPPERS"
# Purpose: Calls prompt builders and OpenAI API functions,
#          returns clean named lists for each document.
#
# Assumes:
#   - build_event_identification_prompt() in Prompt_builder_new.R
#   - build_npf_from_event_prompt()       in Prompt_builder_new.R
#   - build_metacategory_prompt()         in Prompt_builder_new.R
#   - call_openai_json()                  in openai_call.R
#   - call_openai_json_batch()            in openai_call.R
#   - `%||%`                              in config.R

# ============================
# STEP 1 — EVENT IDENTIFICATION
# Calls build_event_identification_prompt()
# Returns: event, event_description
# One API call per document via call_openai_json()
# ============================
extract_event_identification <- function(paragraph) {
  
  result <- call_openai_json(
    build_event_identification_prompt(paragraph)
  )
  
  if (is.null(result)) {
    return(list(
      event             = NA_character_,
      event_description = NA_character_
    ))
  }
  
  list(
    event             = result$event             %||% NA_character_,
    event_description = result$event_description %||% NA_character_
  )
}

# ============================
# STEP 2 — NPF EXTRACTION USING IDENTIFIED EVENT
# Calls build_npf_from_event_prompt()
# Requires event and event_description from Step 1.
# Returns: setting, characters (collapsed string), plot, moral
# One API call per document via call_openai_json()
# ============================
extract_npf_from_event <- function(paragraph,
                                   event,
                                   event_description) {
  
  # If event identification failed skip NPF extraction
  if (is.na(event) || is.na(event_description)) {
    return(list(
      setting    = NA_character_,
      characters = NA_character_,
      plot       = NA_character_,
      moral      = NA_character_
    ))
  }
  
  result <- call_openai_json(
    build_npf_from_event_prompt(
      paragraph,
      event,
      event_description
    )
  )
  
  if (is.null(result)) {
    return(list(
      setting    = NA_character_,
      characters = NA_character_,
      plot       = NA_character_,
      moral      = NA_character_
    ))
  }
  
  # Characters is a list of actor/role pairs — collapse to string
  chars_raw <- result$characters %||% NULL
  chars_str <- if (!is.null(chars_raw) && length(chars_raw) > 0) {
    paste(
      sapply(chars_raw, function(x) {
        paste0(x$actor %||% "Unknown", " [", x$role %||% "Unknown", "]")
      }),
      collapse = "; "
    )
  } else {
    NA_character_
  }
  
  list(
    setting    = paste(result$setting %||% NA_character_, collapse = " "),
    characters = chars_str,
    plot       = paste(result$plot    %||% NA_character_, collapse = " "),
    moral      = paste(result$moral   %||% NA_character_, collapse = " ")
  )
}

# ============================
# STEP 3 — META-CATEGORY ASSIGNMENT
# Calls build_metacategory_prompt()
# Takes the full vectors of event and event_description
# from the entire corpus — runs ONE batch API call.
# Returns: dataframe with row_idx and meta_category columns.
#
# Usage:
#   meta_df <- extract_metacategories(
#     events       = final_df_wildlife$event,
#     descriptions = final_df_wildlife$event_description
#   )
#   final_df_wildlife$meta_category <- meta_df$meta_category
# ============================
extract_metacategories <- function(events, descriptions) {
  
  # Replace NA with fallback strings so every record is sent
  events_clean       <- ifelse(is.na(events),       "unknown",        events)
  descriptions_clean <- ifelse(is.na(descriptions), "no description", descriptions)
  
  # One batch API call for the entire corpus
  result <- call_openai_json_batch(
    build_metacategory_prompt(
      events       = events_clean,
      descriptions = descriptions_clean
    )
  )
  
  if (is.null(result)) {
    message("Meta-category batch call returned NULL.")
    return(
      data.frame(
        row_idx       = seq_along(events),
        meta_category = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Parse the JSON array — one row per document
  cluster_map <- purrr::map_dfr(result, function(x) {
    tibble::tibble(
      row_idx       = as.integer(x$index),
      meta_category = as.character(x$meta_category %||% NA_character_)
    )
  }) %>%
    dplyr::arrange(row_idx)
  
  cluster_map
}