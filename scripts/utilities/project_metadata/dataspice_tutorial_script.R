# =============================================================
# TUTORIAL: Creating Metadata using the Dataspice Package
# =============================================================

# Load necessary libraries
library(readr)
library(dataspice)
library(listviewer)
library(dplyr)
library(jsonlite)
library(here)

# Pathway Defined for Easier Usage
# -------------------------------------------------------------
tutorial_path <- here("scripts", "utilities", "project_metadata")

# Example Data
# -------------------------------------------------------------
df <- data.frame(
  id = 1:6,
  date = c("2025-04-02", "2025-03-26", "2025-03-28", "2025-04-15", "2025-04-12", "2025-03-04"),
  lat = c(43.6150, 43.1366, 42.5629, 43.5407, 43.6629, 43.6105),
  lon = c(-116.2023, -115.6961, -114.4609, -116.5635, -116.6874, -116.3915),
  satisfaction = c(4, 5, 3, 4, 2, 5)
)

# Save data as .csv file
write_csv(df, file.path(tutorial_path, "data.csv"))

# =============================================================
# STEP 1: Create Metadata Templates
# =============================================================
# We start by creating a metadata folder, located in this script's directory,
# containing 4 basic metadata template .csv files (access.csv, attributes.csv,
# biblio.csv, and creators.csv) in which to collect metadata related to our
# example dataset using function create_spice().
create_spice(dir = file.path(tutorial_path))

# NOTE: Without argument specification of directory and file pathways, 
# dataspice functions will resort to their defaults.

# =============================================================
# STEP 2: Record Metadata 
# =============================================================
# Shiny helpers (edit_biblio(), edit_creators(), etc.) let you fill in those
# metadata templates.
# Helper functions, prep_access() and prep_attributes(), will extract the data's
# information and auto-populate those metadata templates. 
# REMEMBER to click on Save when you're done editing!

# ------------------------CREATORS.CSV-------------------------- 
# This contains details of the dataset creators.
# We can open and edit the file using an interactive shiny app using the
# edit_creators() function.
edit_creators(metadata_dir = file.path(tutorial_path, "metadata"))

# -------------------------ACCESS.CSV---------------------------
# The access.csv contains details about where the data can be accessed.
# Before manually completing any details in the access.csv, we can use the 
# prep_access() function to extract relevant information from the data files themselves.
prep_access(
  data_path = file.path(tutorial_path, "data.csv"),
  access_path = file.path(tutorial_path, "metadata", "access.csv")
)
# Next, we can use function edit_access() to complete the final details required.
edit_access(file.path(tutorial_path, "metadata"))

# ------------------------BIBLIO.CSV----------------------------
# The biblio.csv contains dataset level metadata like title, description,
# licence and spatial and temporal coverage.
# Get the data's temporal and spatial extent using base R function range().
range(df$date)
range(df$lat) # South/North boundaries
range(df$lon) # West/East boundaries
# Now we can complete the fields in the biblio.csv file using function edit_biblio().
edit_biblio(file.path(tutorial_path, "metadata"))

# -----------------------ATTRIBUTES.CSV-------------------------
# The attributes.csv contains details about the variables in your data.
# Again, dataspice provides functionality to populate the attributes.csv by extracting
# the variable names from individual data files using function prep_attributes().
prep_attributes(
  data_path = file.path(tutorial_path, "data.csv"),
  attributes_path = file.path(tutorial_path, "metadata", "attributes.csv")
)
# We can now use edit_attributes() to fill in the final details, namely the description
# and units associated with the variables.
edit_attributes(file.path(tutorial_path, "metadata"))

# =============================================================
# STEP 3: Create Metadata JSON-LD File
# =============================================================
# Now that all our metadata template .csv files are complete, we can compile it
# all into a structured dataspice.json file using function write_spice().
write_spice(file.path(tutorial_path, "metadata"))

# Here’s an interactive view of the dataspice.json file we just created:
read_json(file.path(tutorial_path, "metadata", "dataspice.json")) %>% 
  jsonedit()

# =============================================================
# STEP 4: Build README Site
# =============================================================
# The function build_site() will use the dataspice.json file we just created and
# a simple HTML template to generate a static “README”‑style webpage. 
build_site(
  path = file.path(tutorial_path, "metadata", "dataspice.json"),
  out_path = file.path(tutorial_path, "index.html")
)
# The README site can include auto‑generated tables of variables, interactive
# maps, and data previews.
