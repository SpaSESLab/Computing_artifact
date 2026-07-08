
#GENERIC OPENAI CALL - for handling all API communication


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


#CALL FOR MULTI-DOCUMENT RESPONSES for build_cluster_prompt, build_category_description_prompt.We increased the max_tokens to 2000 because of longer response needed for batch outputs

call_openai_json_batch <- function(prompt_text,
                                   system_msg = "Return only valid JSON.",
                                   timeout    = 60) {
  
  body <- list(
    model       = "gpt-4o-mini",
    temperature = 0,
    max_tokens  = 2000,
    messages    = list(
      list(role = "system", content = system_msg),
      list(role = "user",   content = prompt_text)
    )
  )
  
  resp <- tryCatch(
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ),
      body   = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(timeout)
    ),
    error = function(e) {
      message("API call failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(resp) || httr::http_error(resp)) return(NULL)
  
  content_raw <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed      <- jsonlite::fromJSON(content_raw, simplifyVector = FALSE)
  
  if (is.null(parsed$choices)) return(NULL)
  
  msg <- parsed$choices[[1]]$message$content
  
  # Strip markdown fences if present (batch responses sometimes include them)
  msg_clean <- trimws(gsub("```json|```", "", msg))
  
  tryCatch(
    jsonlite::fromJSON(msg_clean, simplifyVector = FALSE),
    error = function(e) {
      message("JSON parse failed: ", e$message)
      message("Raw response was:\n", msg)
      return(NULL)
    }
  )
}

