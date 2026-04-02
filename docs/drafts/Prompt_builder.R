
#Title: "PROMPT BUILDER"
#Purpose: Handles all LLM prompt generation,Prompt_builder.R – all prompt builders + explanation helper.

# Assumes:
# - call_openai_json() is defined in openai_call.R
# - `%||%` is defined in config.R)

# ============================
# NPF PROMPT (EVENT/STORY/MORAL)
# ============================
build_npf_prompt <- function(paragraph) {
  paste0(
    "You are coding text related to the Endangered Species Act (ESA) using the Narrative Policy Framework (NPF).\n\n",
    "IMPORTANT:\n",
    "Extract narrative elements ONLY if the paragraph concerns:\n",
    "- The Endangered Species Act\n",
    "- Federal endangered or threatened species protections\n",
    "- Species listing or delisting decisions\n",
    "- Critical habitat designation\n",
    "- Federal wildlife regulatory actions under endangered species law\n",
    "- Legal or legislative disputes specifically about ESA protections\n\n",
    "If the paragraph does NOT concern ESA-related policy, return:\n\n",
    "{\n",
    "  \"event\": null,\n",
    "  \"story\": null,\n",
    "  \"moral\": null\n",
    "}\n\n",
    "If the paragraph IS ESA-related, extract:\n\n",
    "1. Event – The specific ESA-related policy action or inaction being described\n",
    "   (e.g., bill introduction, amendment, moratorium, regulatory decision, listing decision, court ruling).\n\n",
    "2. Story – The causal plot that links actors, actions, and consequences\n",
    "   (who does what, causing what outcome, for whom).\n\n",
    "3. Moral – The implied policy prescription or evaluation\n",
    "   (what should be done, or whether the policy/action is good or bad).\n\n",
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"event\": \"...\",\n",
    "  \"story\": \"...\",\n",
    "  \"moral\": \"...\"\n",
    "}\n\n",
    "Now code this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\"\n"
  )
}

# ============================
# STORY PROMPT (SETTING/CHARACTERS/PLOT/MORAL)
# ============================
build_story_prompt <- function(paragraph) {
  paste0(
    "You are coding text related to the Endangered Species Act (ESA).\n\n",
    "IMPORTANT:\n",
    "Extract narrative elements ONLY if the paragraph concerns:\n",
    "- The Endangered Species Act\n",
    "- Federal endangered or threatened species protections\n",
    "- Species listing/delisting decisions\n",
    "- Critical habitat designation\n",
    "- Federal regulatory actions under ESA\n",
    "- ESA-related litigation or legislative reform\n\n",
    "If the paragraph does NOT concern ESA-related policy, return:\n\n",
    "{\n",
    "  \"setting\": null,\n",
    "  \"characters\": null,\n",
    "  \"plot\": null,\n",
    "  \"moral\": null\n",
    "}\n\n",
    "If the paragraph IS ESA-related, extract:\n\n",
    "1. Setting – The institutional or political context where the action occurs\n",
    "   (e.g., congressional hearing, federal agency decision, court ruling, regulatory process).\n\n",
    "2. Characters – The key actors involved\n",
    "   (e.g., legislators, agencies, environmental groups, landowners, industries, courts).\n\n",
    "3. Plot – The sequence of actions and consequences\n",
    "   (who does what, and what results).\n\n",
    "4. Moral – The implied lesson, policy message, or evaluative takeaway.\n\n",
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"setting\": \"...\",\n",
    "  \"characters\": \"...\",\n",
    "  \"plot\": \"...\",\n",
    "  \"moral\": \"...\"\n",
    "}\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

# ============================
# EVENT SUMMARY PROMPT (1–3 WORD LABEL)
# ============================
build_event_summary_prompt <- function(paragraph) {
  paste0(
    "You are coding policy events related to the U.S. Endangered Species Act (ESA).\n\n",
    "From the paragraph below, identify the SINGLE main ESA-related policy event.\n\n",
    "Describe this event using a very short, neutral label (ideally 1–3 words).\n\n",
    "Guidelines:\n",
    "- Focus on WHAT happened or is proposed (e.g., listing decision, lawsuit, rule change).\n",
    "- Do NOT write a full sentence.\n",
    "- Do NOT include opinions, causes, or consequences.\n",
    "- Use only a few words (1–3) that capture the type of policy event.\n",
    "The example below is only to illustrate the structure and style of an answer.\n",
    "Do not copy its content or assume any particular stance.\n",
    "Always base your answer only on the paragraph provided, even if it supports species protection,\n",
    "economic interests, both, or neither.\n\n",
    "Return STRICTLY a JSON object:\n",
    "{\n",
    "  \"event_summary\": \"...\"\n",
    "}\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

# ============================
# PROMPT FOR REASONS FOR EVENT LABELS
# ============================
build_event_label_explainer <- function(paragraph, event_summary) {
  paste0(
    "You are validating event labels for text about the U.S. Endangered Species Act (ESA).\n\n",
    "You are given:\n",
    "- A short event label that was previously assigned: \"", event_summary, "\"\n",
    "- The original paragraph from which it was derived.\n\n",
    "Your task:\n",
    "Briefly explain WHY this label is a reasonable description of the main policy event\n",
    "in the paragraph. Refer to concrete actions, actors, or decisions in the text.\n\n",
    "If the label does NOT fit the paragraph, say so and briefly explain why.\n\n",
    "Return STRICTLY a JSON object:\n",
    "{\n",
    "  \"event_label\": \"", event_summary, "\",\n",
    "  \"explanation\": \"1–2 sentences explaining the fit (or misfit) between the label and the paragraph.\"\n",
    "}\n\n",
    "The example above is only to illustrate the structure and style of the explanation.\n",
    "Do not copy its content or assume any stance.\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

# META-CATEGORY PROMPT (LLM-BASED EVENT GROUPING)

#Instead of using string detection to assign event types, I now give the LLM a small, 
#theory-based set of meta-categories and ask it to classify each event (using the full paragraph and the event label), 
#returning both the category and a short justification. Those meta-categories are what I use for grouping and visualization.



build_event_group_prompt <- function(paragraph, event_summary) {
  paste0(
    "You are classifying policy events related to the U.S. Endangered Species Act (ESA).\n\n",
    "You are given:\n",
    "- A short event label extracted from the text: \"", event_summary, "\"\n",
    "- The original paragraph from which this label came.\n\n",
    "Your task:\n",
    "Assign this event to ONE of the following meta-categories and briefly explain why:\n\n",
    "1. Listing / Delisting\n",
    "   Events where a species is proposed for listing, listed, or removed from ESA protection.\n\n",
    "2. Litigation / Courts\n",
    "   Lawsuits, legal challenges, court cases, rulings, injunctions, or judicial nominations related to ESA issues.\n\n",
    "3. Habitat / Recovery\n",
    "   Habitat designations, recovery plans, restoration efforts, conservation plans, or similar habitat/recovery actions.\n\n",
    "4. Regulatory / Agency Action\n",
    "   Rules, regulations, guidelines, moratoria, or other administrative actions by agencies implementing ESA-related policies.\n\n",
    "5. Legislative Action\n",
    "   Bills, laws, amendments, ballots, measures, or explicit legislative proposals related to ESA or wildlife policy.\n\n",
    "6. Industry / Economic Conflict\n",
    "   Conflicts involving industries or economic interests (e.g., pipelines, logging, energy, tariffs, development) in tension with ESA protections.\n\n",
    "7. Cooperative Arrangements\n",
    "   Agreements, treaties, programs, or alliances aimed at coordinating conservation or wildlife management.\n\n",
    "8. Other / Misc\n",
    "   Events that do not clearly fit any of the above categories.\n\n",
    "The example categories above are only to illustrate the structure and style of grouping.\n",
    "Do NOT invent new category names. Choose exactly one from the list above.\n",
    "Always base your answer only on the paragraph and event label provided, even if they support species protection,\n",
    "economic interests, both, or neither.\n\n",
    "Return STRICTLY a JSON object with this schema:\n",
    "{\n",
    "  \"event_group\": \"one of: Listing / Delisting, Litigation / Courts, Habitat / Recovery, Regulatory / Agency Action, Legislative Action, Industry / Economic Conflict, Cooperative Arrangements, Other / Misc\",\n",
    "  \"explanation\": \"1–3 sentences explaining why this category fits based on the paragraph and event label.\"\n",
    "}\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

#VS 


#build_event_group_prompt <- function(paragraph, event_summary) {
# paste0(
#   "You are analyzing policy events related to the U.S. Endangered Species Act (ESA).\n\n",
#    "You are given:\n",
#    "- A short event label extracted from the text: \"", event_summary, "\"\n",
#    "- The original paragraph from which this label came.\n\n",
#    "Your task:\n",
#    "1. Propose a concise category name (1–4 words) that best describes the TYPE of policy event.\n",
#    "2. Briefly explain WHY this category fits, based on the paragraph and the event label.\n\n",
#    "Guidelines for the category name:\n",
#    "- It should describe the kind of policy event (e.g., listing decision, lawsuit, rule change, funding decision).\n",
#    "- It must be neutral and generic, not a full sentence.\n",
#    "- Do NOT include actors' names, species names, dates, or locations in the category name.\n",
#    "- Base it ONLY on the information in the paragraph and event label.\n\n",
#   "The examples above are only to illustrate the structure and style of a category name.\n",
#   "Do NOT copy any example wording. Invent category names that are appropriate to each case.\n\n",
#   "Return STRICTLY a JSON object with this schema:\n",
#    "{\n",
#    "  \"event_group\": \"a short, neutral category name (1–4 words)\",\n",
#    "  \"explanation\": \"1–3 sentences explaining why this category fits based on the paragraph and event label.\"\n",
#    "}\n\n",
#    "Now analyze this paragraph:\n\n",
#    "\"\"\"", paragraph, "\"\"\""
#  )
#}


# ============================
# HELPERS USING THESE PROMPTS
# ============================

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
    event_summary = result$event_summary %||% NA
  )
}

extract_event_label_explanation <- function(paragraph, event_summary) {
  result <- call_openai_json(build_event_label_explainer(paragraph, event_summary))
  if (is.null(result)) {
    return(list(event_label = event_summary, explanation = NA))
  }
  list(
    event_label  = result$event_label  %||% event_summary,
    explanation  = result$explanation %||% NA
  )
}

#new aded
extract_event_group <- function(paragraph, event_summary) {
  result <- call_openai_json(build_event_group_prompt(paragraph, event_summary))
  if (is.null(result)) {
    return(list(
      event_group  = NA,
      explanation  = NA
    ))
  }
  list(
    event_group = result$event_group %||% NA,
    explanation = result$explanation %||% NA
  )
}


