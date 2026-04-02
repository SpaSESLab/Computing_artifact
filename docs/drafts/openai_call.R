
#Title: "GENERIC OPENAI CALL"
#Purpose: Handles all API communication

  
  
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
