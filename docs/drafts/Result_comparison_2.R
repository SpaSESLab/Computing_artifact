# =============================================
# ESA LISTING TREND — MEDIA vs CONGRESS
# (using event_type column)
# =============================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load datasets
media_df <- read_csv("narrative_coded_results_with_baselines_widlife_300.csv")
congress_df <- read_csv("congress_document_outputs.csv")

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
media_listing <- media_df %>%
  filter(event_type == "Listing") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Media")

congress_listing <- congress_df %>%
  filter(event_type == "Listing") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Congress")

# Combine datasets
combined_df <- bind_rows(media_listing, congress_listing)

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
    title = "Trend of ESA Listing Events: Media vs Congress",
    x = "Year",
    y = "Frequency"
  )

plt

ggsave(
  filename = "ESA_Listing_Media_vs_Congress.png",
  plot = plt,
  width = 10,
  height = 6,
  dpi = 300
)


# =============================================
# ESA PRO-PROTECTION MORAL TREND — MEDIA vs CONGRESS
# (using moral_direction column)
# =============================================

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load datasets
media_df <- read_csv("narrative_coded_results_with_baselines_widlife_300.csv")
congress_df <- read_csv("congress_document_outputs.csv")

# Extract Year

media_df <- media_df %>%
  mutate(
    Year = as.numeric(substr(Date, 1, 4))
  )

congress_df <- congress_df %>%
  mutate(
    Date = str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}"),
    Year = as.numeric(substr(Date, 1, 4))
  )

# Filter Pro-protection morals
media_protection <- media_df %>%
  filter(moral_direction == "Pro-protection") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Media")

congress_protection <- congress_df %>%
  filter(moral_direction == "Pro-protection") %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Congress")

# Combine datasets
combined_df <- bind_rows(media_protection, congress_protection)

# Create full year sequence
year_range <- data.frame(Year = seq(1980, 2025))
sources <- unique(combined_df$Source)

full_df <- expand.grid(Year = year_range$Year, Source = sources) %>%
  left_join(combined_df, by = c("Year", "Source")) %>%
  mutate(Document_Count = ifelse(is.na(Document_Count), 0, Document_Count))

# Plot
plt_protection <- ggplot(full_df,
                         aes(x = Year, y = Document_Count,
                             color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6, alpha = 0.95) +
  scale_color_manual(values = c("Media" = "darkblue",
                                "Congress" = "darkgreen")) +
  scale_x_continuous(limits = c(1980, 2025),
                     breaks = seq(1980, 2025, by = 5)) +
  theme_light() +
  labs(
    title = "Trend of Pro-Protection Moral Framing: Media vs Congress",
    x = "Year",
    y = "Frequency"
  )

plt_protection

ggsave(
  filename = "ESA_ProProtection_Media_vs_Congress.png",
  plot = plt_protection,
  width = 10,
  height = 6,
  dpi = 300
)
