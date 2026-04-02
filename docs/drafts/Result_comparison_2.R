
# ESA LISTING TREND — MEDIA vs CONGRESS
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load datasets
media_df <- read_csv("narrative_coded_results_with_baselines_widlife_22.csv")
congress_df <- read_csv("congress_document_outputs_2.csv")

# Extract Year

# MEDIA
media_df <- media_df %>%
  mutate(
    Year = as.numeric(substr(Date, 1, 4))
  )

# CONGRESS (extract date from document_id)
congress_df <- congress_df %>%
  mutate(
    Date = str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}"),
    Year = as.numeric(substr(Date, 1, 4))
  )

# Filter Listing events using event_type
media_agency_action <- media_df %>%
  filter(event_group == "Regulatory / Agency Action") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Media")

congress_agency_action <- congress_df %>%
  filter(event_group == "Regulatory / Agency Action") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Congress")

# Combine datasets
combined_df <- bind_rows(media_agency_action, congress_agency_action)

# Create full year sequence
year_range <- data.frame(Year = seq(1980, 2025))
sources <- unique(combined_df$Source)

full_df <- expand.grid(Year = year_range$Year, Source = sources) %>%
  left_join(combined_df, by = c("Year", "Source")) %>%
  mutate(Document_Count = ifelse(is.na(Document_Count), 0, Document_Count))

# Plot
plt <- ggplot(full_df,
              aes(x = Year, y = Document_Count,
                  color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6, alpha = 0.95) +
  scale_color_manual(values = c("Media" = "darkblue",
                                "Congress" = "red")) +
  scale_x_continuous(limits = c(1980, 2025),
                     breaks = seq(1980, 2025, by = 5)) +
  theme_light() +
  labs(
    title = "Trend of ESA Regulatory / Agency Action Events: Media vs Congress",
    x = "Year",
    y = "Frequency"
  )

plt

ggsave(
  filename = "ESA_Regulatory_Media_vs_Congress.png",
  plot = plt,
  width = 10,
  height = 6,
  dpi = 300
)
