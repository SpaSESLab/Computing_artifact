# EXTRACTION WRAPPERS - Calls prompt builders and OpenAI API functions, returns clean named lists for each document.


#EVENT IDENTIFICATION - Calls build_event_identification_prompt() and then returns: event, event_description


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


#NPF EXTRACTION USING IDENTIFIED EVENT - Calls build_npf_from_event_prompt(), Requires event and event_description from Step 1 and then Returns: setting, characters, plot, moral


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


#META-CATEGORY ASSIGNMENT - Calls build_metacategory_prompt()

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