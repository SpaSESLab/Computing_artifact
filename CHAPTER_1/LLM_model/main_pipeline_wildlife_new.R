
# MAIN PIPELINE — NPF Extraction 
# Output columns:event, event_description,setting, characters, plot, moral,meta_category

#SOURCE MODULES
source("config.R")
source("openai_call.R")
source("Prompt_builder_news_articles.R")
source("Extraction_wrappers_new.R")


# Loading the data
df <- read.csv(
  "/Users/agnesnamyalo/Desktop/RESEARCH/DATA/esa_9_30_2025.csv",
  stringsAsFactors = FALSE
)

#head(df[, 1:5])

df_test <- df %>% slice(1:20)

final_df_wildlife <- df_test %>%
  mutate(
    
    #event identification 
    event_id = map(Text, function(para) {
      Sys.sleep(0.3)
      extract_event_identification(para)
    }),
    event             = map_chr(event_id, "event"),
    event_description = map_chr(event_id, "event_description"),
    
    #NPF extraction using identified event
    npf_raw = map2(Text, event_id, function(para, ev) {
      Sys.sleep(0.3)
      extract_npf_from_event(
        para,
        ev$event,
        ev$event_description
      )
    }),
    setting    = map_chr(npf_raw, "setting"),
    characters = map_chr(npf_raw, "characters"),
    plot       = map_chr(npf_raw, "plot"),
    moral      = map_chr(npf_raw, "moral")
    
  ) %>%
  
  #droping internal working columns
  select(-event_id, -npf_raw) %>%
  
  #creating the meta-category
  { df_temp <- .
  meta_df  <- extract_metacategories(
    events       = df_temp$event,
    descriptions = df_temp$event_description
  )
  df_temp$meta_category <- meta_df$meta_category
  df_temp } %>%
  
  #columns to extract
  select(GOID,Title,Date,Text,event,
         event_description,
         setting,
         characters,
         plot,
         moral,
         meta_category)


#checking the first 5 rows
final_df_wildlife %>%
  select(GOID,Title,Date,Text,event,event_description,
         setting, characters, plot, moral,
         meta_category) %>%
  head(5) %>%
  as.data.frame() %>%
  print()

#meta-category distribution
final_df_wildlife %>%
  count(meta_category, name = "n") %>%
  mutate(prop = round(n / sum(n), 3)) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  print()


#SAVE
write_csv(final_df_wildlife,
          "widlife_extracted_20.csv")



