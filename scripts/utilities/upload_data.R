# =============================================================
# TEMPLATE: Upload Files to a Google Drive Folder
# =============================================================

# Load Required Packages
library(googledrive)  # For Google Drive interactions
library(here)         # For constructing relative file paths

# STEP 1: Authenticate with Google Drive
# -------------------------------------------------------------
# When running `drive_auth()`, a browser window will open asking for 
# authorization. Log in and grant permission for R to access your Google Drive.
# This will store your credentials locally, so you won’t need to repeat this 
# unless the token expires.
drive_auth()

# STEP 2: Specify the Google Drive Folder
# -------------------------------------------------------------
# Replace 'YOUR_FOLDER_ID_HERE' with the ID of the corresponding folder you
# want to upload files to.
# The folder ID is the part of the Google Drive URL that looks like:
# https://drive.google.com/drive/folders/<Folder ID>?usp=drive_link
gd_data_original <- "YOUR_FOLDER_ID_HERE"
gd_data_processed <- "YOUR_FOLDER_ID_HERE"
gd_data_final <- "YOUR_FOLDER_ID_HERE"
gd_outputs_test <- "YOUR_FOLDER_ID_HERE"
gd_outputs_final <- "YOUR_FOLDER_ID_HERE"
gd_docs_drafts <- "YOUR_FOLDER_ID_HERE"
gd_docs_final <- "YOUR_FOLDER_ID_HERE"

# Replace 'gd_FOLDER_OBJECT_NAME_HERE' with the folder's object name (e.g. gd_data_final)
# that you want to upload files to.
gd_folder_id <- gd_FOLDER_OBJECT_NAME_HERE

# STEP 3: Specify Local Files to Upload
# -------------------------------------------------------------
# Objects for local directories pathways created
ld_data_original <- "data/original"
ld_data_processed <- "data/processed"
ld_data_final <- "data/final"
ld_outputs_test <- "outputs/test"
ld_outputs_final <- "outputs/final"
ld_docs_drafts <- "docs/drafts"
ld_docs_final <- "docs/final"

# Replace 'ld_FOLDER_OBJECT_NAME_HERE' with the folder's object name (e.g. ld_data_final)
# that you want to download the files from.
ld_folder_path <- ld_FOLDER_OBJECT_NAME_HERE

# Use `list.files()` to specify the files you want to upload.
# - Use `here()` for relative paths within your project.
# - Use an absolute path for a specific folder location.
local_files <- list.files(
  path = here(ld_folder_path), # Replace with your desired folder
  full.names = TRUE    # Includes full file paths for uploading
)

# STEP 4: Upload Files to Google Drive
# -------------------------------------------------------------
# Loop through the files in `local_files` and upload them to the specified 
# Google Drive folder.
# Use `overwrite = FALSE` to avoid overwriting existing files. 
# Change to `overwrite = TRUE` if you want to replace existing files.

lapply(
  local_files,  # Loop through local file paths
  function(file) {
    drive_upload(
      media = file,        # Local file path
      path = as_id(gd_folder_id), # Target Google Drive folder
      overwrite = FALSE    # Set to TRUE to overwrite existing files
    )
  }
)

# =============================================================
# CUSTOMIZATION NOTES:
# -------------------------------------------------------------
# 1. Customize the Google Drive folder objects and local directories pathways to fit your needs.
# 2. To specify files for upload:
#    - Update the `path` argument in `list.files()`:
#      a) Use `here()` for relative paths within your project (e.g., here("data")).
#      b) Use an absolute file path for a specific location (e.g., "/path/to/your/folder").
#    - Add filters like `pattern = "\\.csv$"` to upload only specific file types (e.g., `.csv` files).
# 3. Use `overwrite = TRUE` only if you want to replace files in the target folder.
# 4. If you encounter upload errors, ensure the local directory and file paths are correct.
# =============================================================
