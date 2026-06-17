#LLM prompt generation--congress

# EVENT IDENTIFICATION PROMPT

build_event_identification_prompt <- function(paragraph) {
  paste0(
    #SYSTEM IDENTITY
    "You are a policy analyst identifying ESA-related policy events from ",
    "texts that discuss endangered species issues in the United States. ",
    "These texts may come from news reporting, legislative floor speeches, ",
    "committee statements, or extensions of remarks. Your task is the same ",
    "regardless of text type: identify the single main ESA-related policy ",
    "event being discussed.\n\n",
    
    "From the paragraph below, identify the SINGLE main ESA-related policy ",
    "event being discussed.\n\n",
    
    "Describe this event using a very short, neutral label (ideally 1-3 words).\n\n",
    
    "Guidelines:\n",
    "- Focus on WHAT policy event is being discussed or proposed.\n",
    "- Do NOT write a full sentence.\n",
    "- Do NOT include actor names, outcomes, opinions, causes, or consequences.\n",
    "- Use only a few words (1-3) that capture the type of policy event.\n",
    "- The text may be a news article, a floor speech, an extension of remarks, ",
    "  or a committee statement. Identify the ESA policy event being discussed — ",
    "  not the act of reporting or speaking itself.\n",
    "  For example: if a member says 'I rise to oppose the delisting of the gray wolf', ",
    "  the event is 'delisting decision', not 'floor speech'.\n",
    "  If a news article reports on a court ruling over habitat boundaries, ",
    "  the event is 'habitat litigation', not 'news report'.\n",
    "- Always base your answer only on the paragraph provided.\n\n",
    
    "Return STRICTLY a valid JSON object with this schema:\n\n",
    "{\n",
    "  \"event\":             \"1-3 word event type, or null\",\n",
    "  \"event_description\": \"one sentence describing the main ESA policy event, or null\"\n",
    "}\n\n",
    
    "Examples of event values:\n",
    "  'species listing', 'delisting decision', 'legislative amendment',\n",
    "  'regulatory moratorium', 'habitat designation', 'agency rulemaking',\n",
    "  'congressional oversight', 'ESA reform proposal', 'budget appropriation',\n",
    "  'court ruling', 'agency settlement', 'critical habitat litigation',\n",
    "  'regulatory rollback', 'species recovery plan'\n\n",
    
    "Now analyze this paragraph:\n\n",
    "\"\"\"", paragraph, "\"\"\""
  )
}

#NPF Extraction prompt

build_npf_from_event_prompt <- function(paragraph, event, event_description) {
  paste0(
    #SYSTEM IDENTITY 
    "You are a policy narrative analyst coding text using the Narrative ",
    "Policy Framework (NPF). The texts you are coding come from a variety ",
    "of discussions surrounding endangered species in the United States — ",
    "these include news reporting, legislative floor speeches, committee ",
    "statements, and extensions of remarks. In some cases the text describes ",
    "a policy issue; in other cases it prescribes a solution to one. Your ",
    "task is the same regardless of text type: extract the NPF narrative ",
    "elements based strictly on how the text portrays actors and events.\n\n",
    
    #PRIOR CONTEXT 
    "A previous step has already identified the main policy event ",
    "in this paragraph:\n\n",
    "  Event type:        \"", event, "\"\n",
    "  Event description: \"", event_description, "\"\n\n",
    "Use this identified event as your anchor for all four NPF elements.\n\n",
    
    #DEFINITIONS 
    "DEFINITIONS:\n",
    
    "1. Setting — The institutional, geographic, or political context ",
    "   where the ESA-related event takes place.\n",
    "   Examples: federal agency rulemaking, a federal court case, a ",
    "   congressional hearing, a habitat designation process, a legislative ",
    "   chamber or committee debate.\n\n",
    
    "2. Characters — The key actors and their narrative roles.\n",
    "   Read the paragraph carefully and assign each actor ONE role ",
    "   based strictly on how the text portrays them.\n\n",
    "   Ask these questions about each actor as they appear in the text:\n",
    "   - Who is doing something RIGHT — acting correctly, ",
    "     protecting, solving, or standing up for something? → Hero\n",
    "   - Who is doing something WRONG — causing harm, acting ",
    "     unfairly, or creating the problem? → Villain\n",
    "   - Who is being ATTACKED, harmed, or suffering as a direct ",
    "     result of what the Villain is doing? → Victim\n\n",
    "   CRITICAL RULES:\n",
    "   - Assign roles based ONLY on how this text portrays each actor.\n",
    "   - Any actor can hold any role — a government agency can be a ",
    "     Villain, a corporation can be a Hero, a community can be a ",
    "     Victim, a species can be a Victim or absent entirely.\n",
    "   - Do NOT assume the species is always the Victim.\n",
    "   - Do NOT assume the regulator is always the Hero.\n",
    "   - If the text does not clearly portray an actor as doing something ",
    "     right, doing something wrong, or being attacked, do not assign ",
    "     them a role.\n",
    "   - If no clear roles emerge from the text, return null.\n\n",
    
    "3. Plot — The causal chain of actions and consequences ",
    "   specific to the identified event.\n",
    "   Who does what, causing what outcome, for whom — ",
    "   as the text presents it.\n\n",
    
    #MORAL
    # "4. Moral — The problem identified and the preferred solution the ",
    # "   text points toward, based on its characters, framing, and sentiment.\n",
    # "   Ask: given who the heroes, villains, and victims are in this text — ",
    # "   what is the problem, and what does the text suggest should be done?\n",
    # "   This should capture what the text wants the audience to think should happen next.\n",
    # "   The audience may be Congress, policymakers, an agency, or the public\n",
    # "   The moral should be action-oriented, persuasive, and evaluative — not just a summary and not desriptive.\n",
    # "   reflect what the text is trying to convince the reader of.\n",
    # "   If the text is purely informational and does not advocate for any ",
    # "   position or solution, return null.\n\n",
    # 
    
    "4. Moral — The problem identified and the preferred solution the ",
    "   text points toward, based on its characters, framing, and sentiment.\n",
    "   Ask: given who the heroes, villains, and victims are in this text — ",
    "   what is the problem, and what does the text suggest should be done?\n",
    "   This should capture what the text wants the audience to think should happen next.\n",
    "   The audience may be Congress, policymakers, an agency, or the public.\n",
    "   The moral should be action-oriented, persuasive, and evaluative — not just a summary and not descriptive.\n",
    "   Reflect what the text is trying to convince the reader of.\n",
    "   If the text is purely informational and does not advocate for any ",
    "   position or solution, return null.\n\n",
    
    "   CONSISTENCY RULE: The moral must be consistent with the Hero's position.\n",
    "   The Hero is the actor the narrative approves of — the moral must reflect\n",
    "   what the Hero is fighting for, not what the Villain is proposing.\n\n",
    
    "   Before returning the moral, apply this two-question test:\n",
    "   (1) Would the Hero in this text AGREE with this moral?\n",
    "   (2) Would the Villain in this text DISAGREE with this moral?\n",
    "   If the answer to (1) is NO or the answer to (2) is YES — rewrite the moral.\n\n",
    
    "   EXAMPLE:\n",
    "   Characters: environmental groups [Hero], Congressman Pombo [Villain],\n",
    "               endangered species [Victim]\n",
    "   WRONG moral: 'The ESA is failing and a revision is necessary.' \n",
    "     → This is the Villain's argument. The Hero would not agree.\n",
    "   CORRECT moral: 'Pombo's bill threatens endangered species and undermines\n",
    "     ESA protections — Congress should reject this rollback and defend\n",
    "     the conservation framework that environmental groups are fighting to preserve.'\n",
    "     → The Hero (environmental groups) would agree with this.\n",
    "     → The Villain (Pombo) would disagree with this.\n\n",
    
    #INSTRUCTIONS 
    "INSTRUCTIONS:\n",
    "- Base every element strictly on what the paragraph says ",
    "  and how it says it — not on external knowledge.\n",
    "- Be concise — one to three sentences per element.\n",
    "- Do not invent details not present in the paragraph.\n",
    "- If an element is genuinely absent, return null.\n\n",
    
    #OUTPUT SCHEMA
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
    "  \"moral\": \"problem identified and preferred solution, or null\"\n",
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
    #SYSTEM IDENTITY 
    "You are a policy researcher organising event records from texts ",
    "that discuss endangered species issues in the United States. ",
    "These records may come from news reporting, legislative floor speeches, ",
    "committee statements, or extensions of remarks. Your task is the same ",
    "regardless of source: assign each record to the appropriate ",
    "ESA policy meta-category based on the type of policy event described.\n\n",
    
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
    "- Texts may use legislative language ('floor amendment', 'markup', ",
    "  'oversight hearing') or journalistic language ('court ruling', ",
    "  'agency settlement', 'regulatory rollback') — map all of these ",
    "  to the appropriate ESA policy category based on the description.\n\n",
    
    "META-CATEGORY NAMING RULES:\n",
    "- Every meta-category name MUST be framed in ESA policy terms.\n",
    "- Do NOT create generic legislative, procedural, or journalistic ",
    "  categories such as 'Floor Speech', 'Committee Hearing', or 'News Report'.\n",
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
    "  actor names, locations, text source, or legislative procedure.\n",
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
