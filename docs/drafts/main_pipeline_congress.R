
#Title: "Main workflow"
#Purpose: loads modules, runs extraction, returns dataframe..


# ============================
# SOURCE MODULES
# ============================
source("config.R")          # defines LIBRARIES, OPENAI_API_KEY and `%||%`
source("Baseline_models.R") # baseline_any_esa_keyword, baseline_contains_cause_word
source("openai_call.R")     # call_openai_json()
source("Prompt_builder.R") # all prompts + extract_* functions
source("Extraction_wrappers.R") #combines the prompt builder and API call.


# ============================
# READING DATA
# ============================
df <- read.csv(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/extracted_outputs_full_dataset_2.csv",
  stringsAsFactors = FALSE
)

# Take only first 10 rows for testing
df_test <- df %>% slice(1:10)

# ============================
# RUN LLM EXTRACTION
# ============================
final_df_congress <- df_test %>%
  mutate(
    baseline_esa    = map_int(merged_text, baseline_any_esa_keyword),
    baseline_causal = map_int(merged_text, baseline_contains_cause_word),
    npf             = map(merged_text, extract_npf),
    story_struct    = map(merged_text, extract_story_elements),
    event_summ      = map(merged_text, extract_event_summary)
  ) %>%
  mutate(
    event        = map_chr(npf, "event"),
    story        = map_chr(npf, "story"),
    moral        = map_chr(npf, "moral"),
    setting      = map_chr(story_struct, "setting"),
    characters   = map_chr(story_struct, "characters"),
    plot         = map_chr(story_struct, "plot"),
    moral_story  = map_chr(story_struct, "moral_story"),
    event_summary = map_chr(event_summ, "event_summary")
  ) %>%
  select(-npf, -story_struct, -event_summ)


final_df_congress <- final_df_congress %>%
  mutate(
    event_group_raw = map2(
      merged_text,           # full paragraph
      event_summary,          # short label from earlier step
      ~ extract_event_group(.x, .y)
    )
  ) %>%
  mutate(
    event_group          = map_chr(event_group_raw, "event_group"),
    event_explanation     = map_chr(event_group_raw, "explanation")
    #event_explanation     = map_chr(label_expl, "explanation")
    #event_group_expl     = map_chr(event_group_raw, "explanation")
  ) %>%
  select(-event_group_raw)


# ============================
# ADD EXPLANATIONS FOR EVENT LABELS
# ============================
#final_df <- final_df %>%
#  mutate(
#    label_expl = map2(
#     cleaned_text,
#      event_summary,
#    )
#  ) %>%
#  mutate(
#    #event_label_explained = map_chr(label_expl, "event_label"),
#   event_explanation     = map_chr(label_expl, "explanation")
#  ) %>%
#  select(-label_expl)

# ============================
# SAVE OUTPUTS
# ============================
head(final_df_congress)

write_csv(final_df_congress, "congress_document_outputs_2.csv")


final_df_congress %>%
  count(event_group, name = "n") %>%
  mutate(prop = n / sum(n))



#final_df %>%
#  count(event_summary, name = "n") %>%
#  mutate(prop = n / sum(n))

#write_csv(final_df, "wildlife_narratives_with_labels_and_explanations_10.csv")
