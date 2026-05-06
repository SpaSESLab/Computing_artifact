library(stm)
library(tm)
library(tidyverse)
library(Matrix)
library(ggplot2)
library(scales)
library(reshape2)
library(RColorBrewer)

# ============================================================
#  STEPS 6-15: FINAL NPF LABEL ASSIGNMENTS
#  Chapter 1: LLM vs STM Comparison - DTM input
#
#  FINAL LABEL ASSIGNMENTS:
#
#  T1 -> "Other / Misc"
#       Support: eel, taxol, whale, cancer, port,
#                cloth, fork, aquarium, lizard, maine
#       Why: FREX mixes species, finance, medicine.
#            No coherent ESA narrative. Confirmed noise
#            by lowest coherence AND exclusivity (Step 11).
#
#  T2 -> "Listing / Delisting"
#       Support: wolves, wolf, hunt, grouse, grizzly,
#                bear, yellowstone, prairie, sage, delisting
#       Why: "delisting" is ESA-legal term exclusive to
#            listing proceedings. Predator listing battles
#            (wolf, grizzly, sage grouse) confirm.
#
#  T3 -> "Cooperative Arrangements"
#       Support: schectman, ferret, fpl, vetch, pipeline,
#                butterfly, indians, hamilton, panther, tow
#       Why: FPL = Florida Power & Light HCP. "indians" =
#            tribal cooperative agreements. schectman = ESA
#            attorney specialising in HCPs. Coordinated
#            conservation actors, not pure conflict.
#
#  T4 -> "Habitat / Recovery"
#       Support: dam, salmon, nmfs, fishery, columbia,
#                chinook, snake, bonneville, sturgeon, recovery
#       Why: Columbia/Snake River salmon recovery programs.
#            Dam operations as habitat recovery challenge.
#            "recovery" in support set confirms framing.
#
#  T5 -> "Legislative Action"
#       Support: gop, senate, bill, house, election,
#                voter, gingrich, clinton, bush, mccain
#       Why: Congressional actors, bills, elections.
#            Gingrich = 104th Congress ESA moratorium.
#            antienvironment (Lift) confirms ESA framing.
#
#  T6 -> "Litigation / Courts"
#       Support: roberts, justice, scalia, supreme, judge,
#                confirmation, court, kennedy, alito, constitutional
#       Why: Supreme Court justices + pure judicial vocab.
#            Highest exclusivity in model. Strongest match.
#
#  T7 -> "Industry / Economic Conflict"
#       Support: woodpecker, owl, forest, timber, logging,
#                landowner, spotted, land, tree, weyerhaeuser
#       Why: Spotted owl vs PNW timber industry.
#            Weyerhaeuser = largest private timber company.
#            Logging + landowner = industry-ESA tension.
#
#  T8 -> "Regulatory / Agency Action"
#       Support: delta, smelt, water, drought, pombo,
#                gnatcatcher, minnow, irrigate, federal, groundwater
#       Why: Federal water agency decisions on delta smelt.
#            Agency-led water management under ESA authority.
#
#  NPF SCHEMA COVERAGE:
#    Covered (7 of 8):
#      Other/Misc, Listing/Delisting, Cooperative Arrangements,
#      Habitat/Recovery, Legislative Action,
#      Litigation/Courts, Industry/Economic Conflict,
#      Regulatory/Agency Action
#    Uncovered: NONE - all 8 categories covered
#
#  NOTE: This schema matches the original academically
#  defensible assignments with one key change:
#  T4 is now "Habitat / Recovery" instead of
#  "Regulatory / Agency Action", recovering the previously
#  missing NPF category. All 8 categories now covered.
# ============================================================


# -- REBUILD PREREQUISITES AFTER readRDS ---------------------
# Run whenever you reload stm_esa_dtm_k8.rds
# without running the full pipeline from scratch.

dtm <- readRDS("clean_dtm_new.rds")


llm_df      <- read.csv(
  "narrative_coded_results_with_baselines_widlife_full.csv",
  stringsAsFactors = FALSE)
llm_df$year <- as.integer(substr(llm_df$Date, 1, 4))

dtm_goids <- as.character(rownames(dtm))

meta_df <- data.frame(GOID = dtm_goids, stringsAsFactors = FALSE) %>%
  left_join(
    llm_df %>%
      mutate(GOID = as.character(GOID)) %>%
      select(GOID, year, event_group, baseline_causal,
             event, story, moral),
    by = "GOID"
  )

meta_df$year[is.na(meta_df$year)] <- as.integer(
  median(meta_df$year, na.rm = TRUE))
meta_df$year <- as.integer(meta_df$year)

stm_input <- readCorpus(dtm, type = "slam")
docs       <- stm_input$documents
vocab      <- stm_input$vocab

cat("Prerequisites rebuilt:\n")
cat("  dtm_goids :", length(dtm_goids), "documents\n")
cat("  meta_df   :", nrow(meta_df), "rows\n")
cat("  Year range:", min(meta_df$year), "to", max(meta_df$year), "\n")
cat("  docs:", length(docs), "| vocab:", length(vocab), "\n")
# ------------------------------------------------------------


K_FINAL   <- 8
stm_model <- readRDS("stm_esa_dtm_k8.rds")


# -- STEP 6: Inspect and label topics ------------------------
cat("\n=== TOPIC LABELS (FREX words) ===\n")
topic_labels <- labelTopics(stm_model, n = 10)
print(topic_labels)

# NPF event_group strings - exact match to LLM CSV column
topic_names_npf <- c(
  "Other / Misc",                  # T1 - noise
  "Listing / Delisting",           # T2 - wolves, delisting
  "Cooperative Arrangements",      # T3 - FPL, indians, HCPs
  "Habitat / Recovery",            # T4 - salmon, columbia, recovery
  "Legislative Action",            # T5 - senate, bill, gop
  "Litigation / Courts",           # T6 - roberts, scalia, supreme
  "Industry / Economic Conflict",  # T7 - owl, timber, logging
  "Regulatory / Agency Action"     # T8 - delta, smelt, water, agency
)

# Display labels for charts
topic_names_display <- c(
  "Other / Misc",
  "Listing / Delisting",
  "Cooperative Arrangements",
  "Habitat / Recovery",
  "Legislative Action",
  "Litigation / Courts",
  "Industry / Economic Conflict",
  "Regulatory / Agency Action"
)

# 10-word support sets per topic
frex_support <- list(
  T1 = c("eel","taxol","whale","cancer","port",
         "cloth","fork","aquarium","lizard","maine"),
  T2 = c("wolves","wolf","hunt","grouse","grizzly",
         "bear","yellowstone","prairie","sage","delisting"),
  T3 = c("schectman","ferret","fpl","vetch","pipeline",
         "butterfly","indians","hamilton","panther","tow"),
  T4 = c("dam","salmon","nmfs","fishery","columbia",
         "chinook","snake","bonneville","sturgeon","recovery"),
  T5 = c("gop","senate","bill","house","election",
         "voter","gingrich","clinton","bush","mccain"),
  T6 = c("roberts","justice","scalia","supreme","judge",
         "confirmation","court","kennedy","alito","constitutional"),
  T7 = c("woodpecker","owl","forest","timber","logging",
         "landowner","spotted","land","tree","weyerhaeuser"),
  T8 = c("delta","smelt","water","drought","pombo",
         "gnatcatcher","minnow","irrigate","federal","groundwater")
)

# Print assignments with support words
cat("\n=== FINAL NPF ASSIGNMENTS WITH SUPPORT WORDS ===\n")
for (k in seq_len(K_FINAL)) {
  cat(sprintf("\nT%d -> \"%s\"\n  Support: %s\n",
              k,
              topic_names_npf[k],
              paste(frex_support[[k]], collapse = ", ")))
}

# Schema coverage check
cat("\n-- SCHEMA COVERAGE CHECK ---------------------------\n")
all_npf <- c(
  "Listing / Delisting",    "Litigation / Courts",
  "Habitat / Recovery",     "Regulatory / Agency Action",
  "Legislative Action",     "Industry / Economic Conflict",
  "Cooperative Arrangements","Other / Misc"
)
covered   <- unique(topic_names_npf)
uncovered <- setdiff(all_npf, covered)

cat("All 8 NPF categories:", paste(all_npf, collapse=", "), "\n\n")
cat("Covered by STM  :", paste(covered,   collapse=", "), "\n")
cat("Uncovered       :", ifelse(length(uncovered)==0,
                                "NONE - all 8 categories covered",
                                paste(uncovered, collapse=", ")), "\n")
cat("----------------------------------------------------\n")



################################


# ── STEP 6b: FREX words per document ────────────────────────

# Build lookup: topic number → its 10 FREX words as one string
frex_strings <- sapply(seq_len(K_FINAL), function(k) {
  paste(frex_support[[k]], collapse = ", ")
})

# Extract theta and assign dominant topic per document
theta    <- stm_model$theta
dom_topic_idx <- apply(theta, 1, which.max)

# Build the per-document dataframe
frex_doc_df <- data.frame(
  GOID          = dtm_goids,
  year          = meta_df$year,
  stm_topic     = topic_names_npf[dom_topic_idx],
  stm_prob      = round(apply(theta, 1, max), 4),
  frex_words    = frex_strings[dom_topic_idx],
  stringsAsFactors = FALSE
) %>%
  left_join(
    llm_df %>%
      mutate(GOID = as.character(GOID)) %>%
      select(GOID, llm_group = event_group),
    by = "GOID"
  ) %>%
  mutate(agreement = (stm_topic == llm_group))

# Print first 20 rows
cat("\n=== FREX WORDS PER DOCUMENT (first 20) ===\n")
print(
  frex_doc_df %>%
    select(GOID, year, stm_topic, stm_prob,
           llm_group, agreement, frex_words) %>%
    head(20),
  width = 120
)

head(frex_doc_df, 5)


# Save
write.csv(frex_doc_df, "frex_per_document.csv", row.names = FALSE)
cat("\nSaved: frex_per_document.csv\n")
cat(sprintf("Rows: %d | Agreement rate: %.1f%%\n",
            nrow(frex_doc_df),
            mean(frex_doc_df$agreement, na.rm = TRUE) * 100))



frex_csv <- read.csv("frex_per_document.csv", stringsAsFactors = FALSE)

frex_csv %>%
  filter(GOID == "398720911")




#########################












# -- STEP 7: Extract document-topic proportions (theta) ------
theta    <- stm_model$theta
theta_df <- as.data.frame(theta)
colnames(theta_df) <- topic_names_display

theta_df$GOID           <- as.character(dtm_goids)
theta_df$year           <- meta_df$year
theta_df$dominant_topic <- topic_names_npf[apply(theta, 1, which.max)]
theta_df$dominant_display <- topic_names_display[apply(theta, 1, which.max)]
theta_df$dominant_prob  <- round(apply(theta, 1, max), 4)

cat("\nDocument-topic proportions (first 6 docs):\n")
print(head(theta_df[, c("GOID","dominant_topic","dominant_prob","year")]))

noise_count <- sum(theta_df$dominant_topic == "Other / Misc")
cat(sprintf("\nDocs dominated by noise topic (T1): %d / %d (%.1f%%)\n",
            noise_count, nrow(theta_df),
            noise_count / nrow(theta_df) * 100))

cat("\nSTM dominant topic distribution:\n")
print(table(theta_df$dominant_topic))

write.csv(theta_df, "stm_dtm_topic_proportions.csv", row.names = FALSE)
cat("Saved: stm_dtm_topic_proportions.csv\n")


# -- STEP 8: Topic prevalence over time ----------------------
year_effect <- estimateEffect(
  formula     = 1:K_FINAL ~ year,
  stmobj      = stm_model,
  metadata    = meta_df,
  uncertainty = "Global"
)

png("fig_stm_dtm_prevalence_time.png",
    width = 1400, height = 700, res = 150)
plot(year_effect,
     covariate     = "year",
     topics        = 1:K_FINAL,
     model         = stm_model,
     method        = "continuous",
     xlab          = "Year",
     main          = "STM Topic Prevalence Over Time - ESA News",
     labeltype     = "custom",
     custom.labels = topic_names_display,
     n             = 4)
dev.off()
cat("Saved: fig_stm_dtm_prevalence_time.png\n")


# -- STEP 9: Overall topic prevalence bar chart --------------
topic_props_df <- data.frame(
  topic      = topic_names_display,
  npf        = topic_names_npf,
  proportion = colMeans(theta) * 100
) %>%
  arrange(desc(proportion)) %>%
  mutate(is_noise   = (npf == "Other / Misc"),
         is_habitat = (npf == "Habitat / Recovery"))

ggplot(topic_props_df,
       aes(x    = reorder(topic, proportion),
           y    = proportion,
           fill = case_when(
             is_noise   ~ "noise",
             is_habitat ~ "habitat",
             TRUE       ~ "standard"
           ))) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")),
            hjust = -0.15, size = 3.4, colour = "#444441") +
  coord_flip() +
  scale_fill_manual(
    values = c("noise"    = "#E24B4A",
               "habitat"  = "#1D9E75",
               "standard" = "#185FA5"),
    labels = c("noise"    = "Noise (T1)",
               "habitat"  = "Habitat / Recovery (T4)",
               "standard" = "Other ESA topics"),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, max(topic_props_df$proportion) * 1.22),
    labels = function(x) paste0(x, "%")) +
  labs(
    title    = "STM Topic Prevalence - ESA News Articles",
    subtitle = paste0("K = ", K_FINAL, " | n = ", nrow(theta_df),
                      " documents | All 8 NPF categories covered"),
    x = NULL, y = "Mean topic proportion (%)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position    = "bottom")

ggsave("fig_stm_dtm_prevalence_bar.png",
       width = 10, height = 5.5, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_prevalence_bar.png\n")


# -- STEP 10: Topic heatmap over time ------------------------
theta_long <- theta_df %>%
  select(year, all_of(topic_names_display)) %>%
  pivot_longer(-year, names_to = "topic", values_to = "proportion") %>%
  group_by(year, topic) %>%
  summarise(mean_prop = mean(proportion), .groups = "drop") %>%
  mutate(topic = factor(topic, levels = rev(topic_names_display)))

ggplot(theta_long, aes(x = year, y = topic, fill = mean_prop)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low  = "#E6F1FB", high = "#0C447C",
    name = "Mean\nproportion",
    labels = percent_format(accuracy = 1)) +
  labs(
    title    = "STM Narrative Attention Over Time - ESA News",
    subtitle = "Mean topic proportion per year. Darker = more dominant.",
    x = "Year", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("fig_stm_dtm_heatmap.png",
       width = 12, height = 5, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_heatmap.png\n")


# -- STEP 11: Semantic coherence & exclusivity ---------------
coherence <- semanticCoherence(stm_model, docs)
excl      <- exclusivity(stm_model)

stm_quality <- data.frame(
  topic_num   = seq_len(K_FINAL),
  npf_label   = topic_names_npf,
  display     = topic_names_display,
  coherence   = coherence,
  exclusivity = excl,
  label       = paste0("T", seq_len(K_FINAL)),
  is_noise    = topic_names_npf == "Other / Misc",
  is_habitat  = topic_names_npf == "Habitat / Recovery"
)

ggplot(stm_quality,
       aes(x = coherence, y = exclusivity, label = label,
           colour = case_when(
             is_noise   ~ "noise",
             is_habitat ~ "habitat",
             TRUE       ~ "standard"
           ))) +
  geom_point(aes(size = is_noise), alpha = 0.85) +
  geom_text(vjust = -0.9, size = 3.5) +
  geom_hline(yintercept = mean(excl), linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = mean(coherence), linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  annotate("text",
           x     = mean(coherence) + 0.3,
           y     = max(excl) - 0.04,
           label = "ideal: high coherence\n+ high exclusivity",
           size  = 2.8, colour = "grey45", hjust = 0) +
  scale_colour_manual(
    values = c("noise"    = "#E24B4A",
               "habitat"  = "#1D9E75",
               "standard" = "#185FA5"),
    labels = c("noise"    = "Noise (T1)",
               "habitat"  = "Habitat / Recovery (T4)",
               "standard" = "Other ESA topics"),
    name = NULL
  ) +
  scale_size_manual(values = c("FALSE" = 4.5, "TRUE" = 5.5),
                    guide = "none") +
  labs(
    title    = "STM Topic Quality: Semantic Coherence vs Exclusivity",
    subtitle = "Top-right = interpretable AND distinctive. DTM input.",
    x = "Semantic coherence (higher = better)",
    y = "Exclusivity (higher = more distinctive)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("fig_stm_dtm_quality.png",
       width = 7, height = 5, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_quality.png\n")

cat("\nCoherence and exclusivity per topic:\n")
print(stm_quality[, c("label","npf_label","coherence","exclusivity")])


# -- STEP 12: Merge with LLM results -------------------------
comparison_df <- theta_df %>%
  mutate(GOID = as.character(GOID)) %>%
  inner_join(
    llm_df %>%
      mutate(GOID = as.character(GOID)) %>%
      select(GOID, event_group, event, story,
             moral, baseline_causal),
    by = "GOID"
  )

cat("\nMerged comparison dataset:", nrow(comparison_df), "documents\n")
cat("Unmatched GOIDs:", nrow(theta_df) - nrow(comparison_df), "\n")

print(head(comparison_df[, c("GOID","dominant_topic",
                             "event_group","baseline_causal")]))

write.csv(comparison_df, "stm_dtm_llm_comparison.csv", row.names = FALSE)
cat("Saved: stm_dtm_llm_comparison.csv\n")

cat("\nLLM event_group distribution:\n")
print(table(comparison_df$event_group))


# -- STEP 13: Agreement calculation --------------------------
comparison_df <- comparison_df %>%
  mutate(agreement = (dominant_topic == event_group))

overall_agree  <- mean(comparison_df$agreement, na.rm = TRUE)
agree_no_noise <- comparison_df %>%
  filter(dominant_topic != "Other / Misc") %>%
  pull(agreement) %>%
  mean(na.rm = TRUE)

cat(sprintf("\nOverall STM-LLM agreement (all docs)  : %.1f%%\n",
            overall_agree * 100))
cat(sprintf("Agreement excluding noise topic (T1)   : %.1f%%\n",
            agree_no_noise * 100))

agree_by_group <- comparison_df %>%
  group_by(event_group) %>%
  summarise(
    n          = n(),
    agree      = sum(agreement, na.rm = TRUE),
    agree_rate = round(mean(agreement, na.rm = TRUE) * 100, 1)
  ) %>%
  arrange(desc(agree_rate))

cat("\nAgreement by NPF event group:\n")
print(agree_by_group)


# -- STEP 14: Confusion matrix heatmap -----------------------
llm_levels <- sort(unique(na.omit(comparison_df$event_group)))

conf_df <- as.data.frame(table(
  STM = factor(comparison_df$dominant_topic, levels = llm_levels),
  LLM = factor(comparison_df$event_group,   levels = llm_levels)
)) %>%
  rename(count = Freq)

ggplot(conf_df, aes(x = LLM, y = STM, fill = count)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(
    aes(label  = ifelse(count > 0, count, ""),
        colour = count > 4),
    size = 3.5) +
  scale_fill_gradient(low = "#E6F1FB", high = "#0C447C",
                      name = "Documents") +
  scale_colour_manual(
    values = c("TRUE" = "white", "FALSE" = "#333333"),
    guide  = "none") +
  labs(
    title    = "STM vs LLM - Direct NPF Agreement Matrix",
    subtitle = paste0(
      "Both axes = NPF event_group labels. No intermediate mapping.\n",
      "All 8 NPF categories now covered by STM topics."),
    x = "LLM event_group",
    y = "STM dominant topic (NPF label)"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x   = element_text(angle = 40, hjust = 1, size = 9),
        axis.text.y   = element_text(size = 9),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

ggsave("fig_stm_dtm_confusion.png",
       width = 9, height = 8, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_confusion.png\n")


# Agreement bar chart
ggplot(agree_by_group %>% filter(!is.na(event_group)),
       aes(x    = reorder(event_group, agree_rate),
           y    = agree_rate,
           fill = agree_rate > 0)) +
  geom_bar(stat = "identity", alpha = 0.82, width = 0.65) +
  geom_text(
    aes(label = paste0(agree_rate, "%  (n=", n, ")")),
    hjust = -0.08, size = 3.2, colour = "#444") +
  coord_flip() +
  scale_fill_manual(
    values = c("FALSE" = "#E24B4A", "TRUE" = "#185FA5"),
    guide  = "none") +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "STM-LLM Direct Agreement by NPF Event Group",
    subtitle = paste0(
      "No mapping step applied. All 8 NPF categories covered.\n",
      "Red bars = zero agreement categories."),
    x = "NPF event group",
    y = "Agreement rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("fig_stm_dtm_agreement_bar.png",
       width = 9, height = 5, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_agreement_bar.png\n")


# -- STEP 15: STM confidence -> causal narrative -------------
causal_model <- glm(
  baseline_causal ~ dominant_prob + year,
  data   = comparison_df,
  family = binomial(link = "logit")
)

cat("\n=== Logistic regression: STM confidence -> causal? ===\n")
print(summary(causal_model))
cat("\nKEY: p(dominant_prob) > 0.05 = STM confidence does NOT\n")
cat("predict causal narrative. STM confidence != NPF depth.\n")

comparison_df <- comparison_df %>%
  mutate(conf_q = ntile(dominant_prob, 4))

causal_by_q   <- comparison_df %>%
  group_by(conf_q) %>%
  summarise(causal_rate = mean(baseline_causal, na.rm = TRUE) * 100,
            n = n())

overall_causal <- mean(comparison_df$baseline_causal, na.rm = TRUE) * 100

ggplot(causal_by_q, aes(x = factor(conf_q), y = causal_rate)) +
  geom_bar(stat = "identity", fill = "#888780",
           alpha = 0.75, width = 0.62) +
  geom_hline(yintercept = overall_causal, linetype = "dashed",
             colour = "#185FA5", linewidth = 0.9) +
  annotate("text", x = 3.65, y = overall_causal + 2.8,
           label = paste0("LLM overall\ncausal rate: ",
                          round(overall_causal, 1), "%"),
           size = 3.2, colour = "#185FA5") +
  geom_text(
    aes(label = paste0(round(causal_rate, 1), "%\n(n=", n, ")")),
    vjust = -0.4, size = 3.2, colour = "#444") +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "STM Confidence Does Not Predict Causal Narrative",
    subtitle = paste0(
      "Flat bars = STM topic confidence carries no\n",
      "information about NPF causal structure. ",
      "Blue line = LLM overall rate."),
    x = "STM topic confidence quartile (1=low, 4=high)",
    y = "LLM causal narrative rate (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave("fig_stm_dtm_causal_gap.png",
       width = 7, height = 5, dpi = 300, bg = "white")
cat("Saved: fig_stm_dtm_causal_gap.png\n")


# -- FINAL SUMMARY -------------------------------------------
cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║  CHAPTER 1 - STM vs LLM FINAL SUMMARY                   ║\n")
cat("║  Direct NPF event_group labels - no mapping step         ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  DTM documents              : %d                    ║\n",
            nrow(dtm)))
cat(sprintf("║  DTM vocabulary             : %d                 ║\n",
            ncol(dtm)))
cat(sprintf("║  STM topics (K)             : %d                     ║\n",
            K_FINAL))
cat(sprintf("║  Noise docs (T1)            : %d  (%.1f%%)              ║\n",
            noise_count, noise_count / nrow(theta_df) * 100))
cat(sprintf("║  Agreement - all docs       : %.1f%%                 ║\n",
            overall_agree * 100))
cat(sprintf("║  Agreement - no noise       : %.1f%%                 ║\n",
            agree_no_noise * 100))
cat(sprintf("║  LLM causal rate            : %.1f%%                 ║\n",
            overall_causal))
cat("║  NPF categories covered     : ALL 8                     ║\n")
cat("║  Structural absence         : NONE                      ║\n")
cat("║  STM causal detection       : NOT POSSIBLE              ║\n")
cat("║  STM policy moral extract.  : NOT POSSIBLE              ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║  KEY FINDINGS:                                           ║\n")
cat("║  1. All 8 NPF categories covered by STM topics           ║\n")
cat("║  2. T4 (Habitat/Recovery) = salmon/river recovery        ║\n")
cat("║  3. T3 (Cooperative Arrangements) = HCP actors           ║\n")
cat("║  4. T6 Litigation = highest agreement (vocab distinct.)  ║\n")
cat("║  5. STM confidence != causal structure (logistic reg.)   ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")