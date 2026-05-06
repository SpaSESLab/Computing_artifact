
#Title: "EXTRACTION WRAPPERS"
#Purpose: These functions combine the prompt builder and API call.

  

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
    setting     = paste(result$setting %||% NA, collapse = "; "),
    characters  = paste(result$characters %||% NA, collapse = "; "),
    plot        = paste(result$plot %||% NA, collapse = "; "),
    moral_story = paste(result$moral %||% NA, collapse = "; ")
  )
}

extract_event_summary <- function(paragraph) {
  result <- call_openai_json(build_event_summary_prompt(paragraph))
  if (is.null(result)) {
    return(list(event_summary = NA))
  }
  list(
    event_summary = paste(result$event_summary %||% NA, collapse = "; ")
  )
}

