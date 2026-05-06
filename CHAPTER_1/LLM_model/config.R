
#title: "Configurations"
#Purpose: Libraries , API KEY & null operator


#LIBRARIES
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# 2. SETTING API KEY 

Sys.setenv(OPENAI_API_KEY = "sk--")

# In config.R
set.seed(42)

#Sys.setenv(OPENAI_API_KEY =  "sk-proj-")


#SAFE NULL OPERATOR (safety operator for missing values returned by the LLM)

#Why it is needed
#Sometimes the model may return NULL for a field (for example if no event is detected).
#This operator ensures that missing outputs are replaced with a default value.


`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#Returns NA instead of crashing the code