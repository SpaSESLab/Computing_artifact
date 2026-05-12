
# Title:   "PROMPT BUILDER"
# Purpose: Handles all LLM prompt generation.

# EVENT IDENTIFICATION PROMPT

build_event_identification_prompt <- function(paragraph) {
  paste0(
    "You are coding policy events related to the U.S. Endangered Species Act (ESA).\n\n",
    "From the paragraph below, identify the SINGLE main ESA-related policy event.\n\n",
    "Describe this event using a very short, neutral (ideally 1-3 words).\n\n",
    "Guidelines:\n",
    "- Focus on WHAT happened or is proposed (e.g., listing decision, lawsuit, rule change).\n",
    "- Do NOT write a full sentence.\n",
    "- Do NOT include actor names, outcomes,opinions, causes, or consequences.\n",
    "- Use only a few words (1-3) that capture the type of policy event.\n",
    "The example below is only to illustrate the structure and style of an answer.\n",
    "Do not copy its content or assume any particular stance.\n",
    "Always base your answer only on the paragraph provided, even if it supports species protection,\n",
    "economic interests, both, or neither.\n\n",
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"event\":       \"1-3 word event type, or null\",\n",
    "  \"event_description\": \"one sentence describing the main ESA policy event, or null\"\n",
    "}\n\n",
    "Examples of event values:\n",
    "  'species listing', 'court ruling', 'habitat designation',\n",
    "  'legislative amendment', 'regulatory moratorium', ",
    "'delisting decision', 'agency rulemaking'\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}


# STORY PROMPT (SETTING / CHARACTERS / PLOT / MORAL)

build_npf_from_event_prompt <- function(paragraph, event, event_description) {
  paste0(
    "You are a policy narrative analyst coding text related to the ",
    "U.S. Endangered Species Act (ESA) using the ",
    "Narrative Policy Framework (NPF).\n\n",
    "A previous step has already identified the main policy event ",
    "in this paragraph:\n\n",
    "  Event type:        \"", event, "\"\n",
    "  Event description: \"", event_description, "\"\n\n",
    "Using this identified event as your anchor, extract the five ",
    "NPF narrative elements from the paragraph below.\n\n",
    "DEFINITIONS:\n",
    "1. Setting — The institutional, geographic, or political context ",
    "   where the ESA-related event takes place.\n",
    "   Examples: a federal agency rulemaking process, a congressional ",
    "   hearing, a federal court case, a habitat designation process.\n\n",
    "2. Characters — The key actors involved and their NPF roles.\n",
    "   Assign each actor ONE role: Hero, Villain,  or Victim.\n",
    "   - Hero:     actor working to protect species or uphold ESA\n",
    "   - Villain:  actor undermining ESA protections or harming species\n",
    "   - Victim:   the species, ecosystem, or community harmed\n",
    "3. Plot — The causal chain of actions and consequences ",
    "   specific to the identified event.\n",
    "   Who does what, causing what outcome, for whom.\n\n",
    "4. Moral — The explicit or implied solution, evaluative judgment, ",
    "   or recommendation the narrative points toward.\n",
    "   What action, reform, or decision does the narrative call for?\n\n",
    "INSTRUCTIONS:\n",
    "- Ground every element in the identified event above.\n",
    "- Be concise — one to three sentences per element.\n",
    "- Do not invent details not present in the paragraph.\n",
    "- If an element is genuinely absent from the paragraph, return null.\n\n",
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"setting\": \"institutional or political context, or null\",\n",
    "  \"characters\": [\n",
    "    {\n",
    "      \"actor\": \"actor name or group\",\n",
    "      \"role\":  \"Hero | Villain | Victim\"\n",
    "    }\n",
    "  ],\n",
    "  \"plot\":             \"causal sequence of actions and consequences, or null\",\n",
    "  \"moral\":            \"explicit or implied policy recommendation, or null\"\n",
    "}\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}





build_metacategory_prompt <- function(events, descriptions) {

  numbered_records <- paste(
    seq_along(events),
    paste0(
      "event: \"",       events,       "\" | ",
      "description: \"", descriptions, "\""
    ),
    sep      = ". ",
    collapse = "\n"
  )

  paste0(
    "You are a policy researcher organising event records ",
    "from ESA (Endangered Species Act) news coverage.\n\n",
    "Below is a numbered list of event records. ",
    "Each record contains:\n",
    "  - A short event label (1-3 words, e.g., species listing)\n",
    "  - A brief description (one sentence about the ESA policy event)\n\n",
    "RECORDS:\n",
    numbered_records, "\n\n",
    "Your task:\n",
    "Assign each record to exactly ONE ESA policy meta-category.\n\n",
    "GROUPING RULES:\n",
    "- Use the event label as your PRIMARY grouping criterion.\n",
    "- Use the description to RESOLVE AMBIGUITY.\n",
    "- Merge labels that describe the SAME TYPE of ESA policy event.\n\n",
    "Assign each record to one of these categories:\n",
    "  ESA Species Listing\n",
    "  ESA Species Delisting\n",
    "  ESA Habitat / Recovery\n",
    "  ESA Regulatory / Agency Action\n",
    "  ESA Legislative Action\n",
    "  ESA Litigation\n",
    "  ESA Judicial Review\n",
    "  ESA Conservation Agreements\n",
    "  ESA Funding and Budgeting\n",
    "  Other / Non-ESA\n\n",
    "Return STRICTLY a JSON array:\n",
    "[\n",
    "  {\n",
    "    \"index\":         record index number,\n",
    "    \"meta_category\": \"ESA-framed category name\"\n",
    "  }\n",
    "]\n\n",
    "Every index from 1 to ", length(events),
    " must appear exactly once.\n",
    "Return the JSON array only — no preamble, no explanation outside the array."
  )
}
