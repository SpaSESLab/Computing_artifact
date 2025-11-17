# =============================================================
# TEMPLATE: Download Files from the SpaSES Google Drive
# =============================================================

# Load Required Packages
library(googledrive)  # For Google Drive interactions
library(here)         # For constructing relative file paths

# STEP 1: Authenticate with Google Drive
# -------------------------------------------------------------
# When running `drive_auth()`, a browser window will open asking for 
# authorization. You need to log in and grant permission for R to access
# your Google Drive. This will store your credentials locally, so you 
# won’t have to repeat the process unless the token expires.
drive_auth()

# STEP 2: Specify the Google Drive Folder
# -------------------------------------------------------------
# Replace 'YOUR_FOLDER_ID_HERE' with the ID of the corresponding folder you
# want to download files from.
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
# that you want to download files from.
gd_folder_id <- gd_FOLDER_OBJECT_NAME_HERE

# Retrieve the folder information using the folder ID
gd_folder <- drive_get(as_id(gd_folder_id))

# STEP 3: List Files in the Folder
# -------------------------------------------------------------
# Use `drive_ls()` to list all files in the specified folder.
# Note: If there are subfolders, this function will not list files 
# in the subfolders. Each subfolder must be processed separately.
gdrive_files <- drive_ls(gd_folder)

# STEP 4: Specify Local Directory Pathways
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
# that you want to upload the files to.
ld_folder_path <- ld_FOLDER_OBJECT_NAME_HERE

# STEP 5: Download Files to a Local Directory
# -------------------------------------------------------------
# Download all files listed in `gdrive_files`
lapply(
  gdrive_files$id,  # Loop through the IDs of the files in the folder
  function(file_id) {
    drive_download(
      as_id(file_id),  # Google Drive file ID
      path = here(ld_folder_path, gdrive_files[gdrive_files$id == file_id, ]$name),
      overwrite = FALSE  # Change to TRUE if you want to overwrite existing files
    )
  }
)

# =============================================================
# CUSTOMIZATION NOTES:
# -------------------------------------------------------------
# 1. Customize the Google Drive folder objects and local directories pathways to fit your needs.  
# 2. To change the local file path:
#    - Update the `path` argument in `drive_download()`:
#      a) Use `here()` for relative paths within your project (e.g., here("data", ...)).
#      b) Use an absolute file path for a specific location (e.g., "/path/to/your/folder").
#    - Ensure the directory exists before running the script.
# 3. If the folder contains subfolders, you must handle each subfolder separately 
#    by updating `folder_id` and re-running the script for each subfolder.
# 4. Use `overwrite = TRUE` only if you want to replace files in the local directory.
# 5. If you encounter download errors, ensure the local directory and file paths are correct.
# =============================================================
