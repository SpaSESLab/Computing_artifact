
# ESA LISTING TREND — MEDIA vs CONGRESS 


library(readr)
library(dplyr)
library(ggplot2)
library(stringr)


#Load datasets

media_df <- read_csv("narrative_coded_results_with_baselines_widlife_300.csv")
congress_df <- read_csv("narrative_coded_results_with_baselines_congress_300.csv")


#Extract Date & Year


# MEDIA (Date already exists)
media_df <- media_df %>%
  mutate(
    Year = as.numeric(substr(Date, 1, 4))
  )

# CONGRESS (Extract Date from document_id)
congress_df <- congress_df %>%
  mutate(
    Date = str_extract(document_id, "\\d{4}-\\d{2}-\\d{2}"),
    Year = as.numeric(substr(Date, 1, 4))
  )


# Filter only Listing Events
# (exclude Delisting)


media_listing <- media_df %>%
  filter(str_detect(tolower(event), "list") &
           !str_detect(tolower(event), "delist")) %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Media")

congress_listing <- congress_df %>%
  filter(str_detect(tolower(event), "list") &
           !str_detect(tolower(event), "delist")) %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Congress")

# Combine both datasets


combined_df <- bind_rows(media_listing, congress_listing)


# Create complete year range


year_range <- data.frame(Year = seq(1980, 2025))
sources <- unique(combined_df$Source)

full_df <- expand.grid(Year = year_range$Year, Source = sources) %>%
  left_join(combined_df, by = c("Year", "Source")) %>%
  mutate(
    Document_Count = ifelse(is.na(Document_Count), 0, Document_Count)
  )


# Plot of comparative trend


plt <- ggplot(full_df,
              aes(x = Year, y = Document_Count,
                  color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("Media" = "darkblue",
                                "Congress" = "red")) +
  scale_x_continuous(
    limits = c(1980, 2025),
    breaks = seq(1980, 2025, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, max(full_df$Document_Count, na.rm = TRUE)),
    breaks = seq(0,
                 max(full_df$Document_Count, na.rm = TRUE),
                 by = 2)
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10,
                               margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10,
                               margin = margin(r = 5)),
    axis.title.x = element_text(size = 14,
                                margin = margin(b = 10)),
    axis.title.y = element_text(size = 14,
                                margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5,
                              size = 18,
                              margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30",
                                fill = NA,
                                linewidth = 1.5),
    panel.grid.major = element_line(color = "white",
                                    linewidth = 0.6),
    panel.grid.minor = element_line(color = "white",
                                    linewidth = 0.3)
  ) +
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

#Morals
# ESA PRO-PROTECTION MORAL TREND — MEDIA vs CONGRESS


library(readr)
library(dplyr)
library(ggplot2)
library(stringr)


#Loading datasets

media_df <- read_csv("narrative_coded_results_with_baselines_widlife_300.csv")
congress_df <- read_csv("narrative_coded_results_with_baselines_congress_300.csv")


#Extract Date & Year


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

# Filter Pro-Protection Morals


media_protection <- media_df %>%
  filter(str_detect(tolower(moral),
                    "protect|conservation|preserve|safeguard|endangered|habitat protection")) %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Media")

congress_protection <- congress_df %>%
  filter(str_detect(tolower(moral),
                    "protect|conservation|preserve|safeguard|endangered|habitat protection")) %>%
  group_by(Year) %>%
  summarise(Document_Count = n(), .groups = "drop") %>%
  mutate(Source = "Congress")


# Combine both dataset


combined_df <- bind_rows(media_protection, congress_protection)


#Creating complete year range


year_range <- data.frame(Year = seq(1980, 2025))
sources <- unique(combined_df$Source)

full_df <- expand.grid(Year = year_range$Year, Source = sources) %>%
  left_join(combined_df, by = c("Year", "Source")) %>%
  mutate(
    Document_Count = ifelse(is.na(Document_Count), 0, Document_Count)
  )


# Plot of trend


plt_protection <- ggplot(full_df,
                         aes(x = Year, y = Document_Count,
                             color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6, alpha = 0.95, shape = 16) +
  scale_color_manual(values = c("Media" = "darkblue",
                                "Congress" = "darkgreen")) +
  scale_x_continuous(
    limits = c(1980, 2025),
    breaks = seq(1980, 2025, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, max(full_df$Document_Count, na.rm = TRUE)),
    breaks = seq(0,
                 max(full_df$Document_Count, na.rm = TRUE),
                 by = 2)
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10,
                               margin = margin(t = 5, b = 10)),
    axis.text.y = element_text(size = 10,
                               margin = margin(r = 5)),
    axis.title.x = element_text(size = 14,
                                margin = margin(b = 10)),
    axis.title.y = element_text(size = 14,
                                margin = margin(r = 15, l = 15)),
    plot.title = element_text(hjust = 0.5,
                              size = 18,
                              margin = margin(t = 10, b = 5)),
    plot.margin = margin(2, 2, 2, 2),
    panel.border = element_rect(color = "gray30",
                                fill = NA,
                                linewidth = 1.5),
    panel.grid.major = element_line(color = "white",
                                    linewidth = 0.6),
    panel.grid.minor = element_line(color = "white",
                                    linewidth = 0.3)
  ) +
  labs(
    title = "Trend of Pro-Protection Moral Framing: Media vs Congress",
    x = "Year",
    y = "Frequency"
  )

plt_protection

ggsave("ESA_ProProtection_Media_vs_Congress.pdf",
       plot = plt_protection,
       width = 10,
       height = 6)


