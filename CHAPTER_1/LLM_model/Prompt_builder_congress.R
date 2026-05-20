#LLM prompt generation.

# EVENT IDENTIFICATION PROMPT

build_event_identification_prompt <- function(paragraph) {
  paste0(
    "You are coding policy events from U.S. Congressional Record speech ",
    "related to the Endangered Species Act (ESA).\n\n",
    "From the paragraph below, identify the SINGLE main ESA-related policy event ",
    "being discussed.\n\n",
    "Describe this event using a very short, neutral (ideally 1-3 words).\n\n",
    "Guidelines:\n",
    "- Focus on WHAT policy event is being discussed or proposed.\n",
    "- Do NOT write a full sentence.\n",
    "- Do NOT include actor names, outcomes, opinions, causes, or consequences.\n",
    "- Use only a few words (1-3) that capture the type of policy event.\n",
    "- The paragraph is a floor speech, extension of remarks, or committee statement.\n",
    "  Identify the ESA policy event being discussed — not the act of speaking itself.\n",
    "  For example: if a member says 'I rise to oppose the delisting of the gray wolf', ",
    "  the event is 'delisting decision', not 'floor speech'.\n",
    "- Always base your answer only on the paragraph provided.\n\n",
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"event\":             \"1-3 word event type, or null\",\n",
    "  \"event_description\": \"one sentence describing the main ESA policy event, or null\"\n",
    "}\n\n",
    "Examples of event values:\n",
    "  'species listing', 'delisting decision', 'legislative amendment',\n",
    "  'regulatory moratorium', 'habitat designation', 'agency rulemaking',\n",
    "  'congressional oversight', 'ESA reform proposal', 'budget appropriation'\n\n",
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

#NPF Extraction prompt

build_npf_from_event_prompt <- function(paragraph, event, event_description) {
  paste0(
    "You are a policy narrative analyst coding text from the U.S. Congressional Record ",
    "related to the Endangered Species Act (ESA) using the Narrative Policy Framework (NPF).\n",
    "The paragraph is a floor speech, extension of remarks, or committee statement ",
    "by a member of Congress. The speaker is presenting their own explicit position — ",
    "they are an actor in the narrative, not a neutral observer.\n\n",
    
    "A previous step has already identified the main policy event ",
    "in this paragraph:\n\n",
    "  Event type:        \"", event, "\"\n",
    "  Event description: \"", event_description, "\"\n\n",
    
    "Using this identified event as your anchor, extract the four ",
    "NPF narrative elements from the paragraph below.\n\n",
    
    "DEFINITIONS:\n",
    "1. Setting — The institutional, geographic, or political context ",
    "   where the ESA-related event takes place.\n",
    "   For congressional speech this includes the legislative chamber, ",
    "   committee, or hearing where the event is being debated.\n\n",
    
    "2. Characters — The key actors and their narrative roles.\n",
    "   Read the paragraph carefully and assign each actor ONE role ",
    "   based strictly on how the speaker portrays them.\n\n",
    "   Ask these questions about each actor as they appear in the text:\n",
    "   - Who is doing something RIGHT — acting correctly, ",
    "     protecting, solving, or standing up for something? → Hero\n",
    "   - Who is doing something WRONG — causing harm, acting ",
    "     unfairly, or creating the problem? → Villain\n",
    "   - Who is being ATTACKED, harmed, or suffering as a direct ",
    "     result of what the Villain is doing? → Victim\n\n",
    "   CRITICAL RULES:\n",
    "   - Assign roles based ONLY on how the speaker portrays each actor.\n",
    "   - The member of Congress speaking is themselves often a character — ",
    "     they may cast themselves as a Hero defending a cause.\n",
    "   - Any actor can hold any role — a government agency can be a ",
    "     Villain, a corporation can be a Hero, a community can be a ",
    "     Victim, a species can be a Victim or absent entirely.\n",
    "   - Do NOT assume the species is always the Victim.\n",
    "   - Do NOT assume the regulator is always the Hero.\n",
    "   - If the paragraph does not clearly portray an actor as doing ",
    "     something right, doing something wrong, or being attacked, ",
    "     do not assign them a role.\n",
    "   - If no clear roles emerge from the text, return null.\n\n",
    
    "3. Plot — The causal chain of actions and consequences ",
    "   specific to the identified event.\n",
    "   Who does what, causing what outcome, for whom — ",
    "   as the speaker describes it.\n\n",
    
    "4. Moral — The explicit policy position, recommendation, or evaluative judgment ",
    "   the speaker is arguing for.\n",
    "   In congressional speech the moral is usually stated directly — ",
    "   as a policy demand, a call for reform, a defence of a position, ",
    "   or a recommendation for or against a specific action.\n",
    "   What action, change, or decision is the speaker calling for?\n\n",
    
    "INSTRUCTIONS:\n",
    "- Base every element strictly on what the paragraph says ",
    "  and how it says it — not on external knowledge.\n",
    "- Be concise — one to three sentences per element.\n",
    "- Do not invent details not present in the paragraph.\n",
    "- If an element is genuinely absent, return null.\n\n",
    
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"setting\":    \"institutional or political context, or null\",\n",
    "  \"characters\": [\n",
    "    {\n",
    "      \"actor\": \"actor name or group\",\n",
    "      \"role\":  \"Hero | Villain | Victim\"\n",
    "    }\n",
    "  ],\n",
    "  \"plot\":  \"causal sequence of actions and consequences, or null\",\n",
    "  \"moral\": \"explicit policy position or recommendation, or null\"\n",
    "}\n\n",
    
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

#PROMPT FOR ESA META-CATEGORIES

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
    "from ESA (Endangered Species Act) Congressional Record documents.\n\n",
    
    "Below is a numbered list of event records. ",
    "Each record contains:\n",
    "  - A short 'event' label (1-3 words, e.g., 'delisting decision')\n",
    "  - A brief 'description' (one sentence about the ESA policy event)\n\n",
    
    "RECORDS:\n",
    numbered_records, "\n\n",
    
    "Your task:\n",
    "Assign each record to exactly ONE ESA policy meta-category ",
    "from the list below.\n\n",
    
    "GROUPING RULES:\n",
    "- Use the 'event' label as your PRIMARY grouping criterion.\n",
    "- Use the 'description' to RESOLVE AMBIGUITY when two event labels ",
    "  look similar but describe different policy actions.\n",
    "- Merge event labels that describe the SAME TYPE of ESA policy event ",
    "  even if worded differently.\n",
    "- Congressional documents often use legislative language — ",
    "  'floor amendment', 'markup', 'oversight hearing' — ",
    "  map these to the appropriate ESA policy category based on the description.\n\n",
    
    "META-CATEGORY NAMING RULES:\n",
    "- Every meta-category name MUST be framed in ESA policy terms.\n",
    "- Do NOT create generic legislative or procedural categories ",
    "  such as 'Floor Speech' or 'Committee Hearing'.\n",
    "- Assign each record to one of these ESA-specific categories:\n",
    "    'ESA Species Listing'\n",
    "    'ESA Species Delisting'\n",
    "    'ESA Habitat / Recovery'\n",
    "    'ESA Regulatory / Agency Action'\n",
    "    'ESA Legislative Action'\n",
    "    'ESA Litigation'\n",
    "    'ESA Judicial Review'\n",
    "    'ESA Conservation Agreements'\n",
    "    'ESA Funding and Budgeting'\n",
    "    'Other / Non-ESA'\n",
    "- Group by the TYPE of ESA policy event — not by species, ",
    "  actor names, locations, chamber, or legislative procedure.\n",
    "- Place any record with no clear ESA policy connection in ",
    "  'Other / Non-ESA'.\n\n",
    
    "Return STRICTLY a JSON array with this schema:\n",
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