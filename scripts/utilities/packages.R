# =============================================================
# TEMPLATE: Install Required Packages
# =============================================================

# STEP 1: Specify Required Packages
# -------------------------------------------------------------
# Below is a list of common packages required for data manipulation, visualization, and integration.
# These packages are used frequently in data analysis workflows, but you can customize this list as needed.
# You can add any additional packages you require or remove those that aren't necessary for your specific use case.
# Ensure that the package names are spelled correctly.
required_packages <- c(
  "dplyr",       # For data manipulation
  "ggplot2",     # For data visualization
  "readr",       # For reading CSV and other data files
  "googledrive", # For Google Drive integration
  "here",        # For constructing relative file paths
  "tidyr",       # For data tidying
  "stringr",     # For string manipulation
  "dataspice",   # For creating metadata for datasets
  "listviewer",  # For viewing and exploring JSON structures interactively
  "readxl",      # For reading Excel files
  "jsonlite",    # For working with JSON data
  "lintr",       # For checking script adherence to the style guide
  "styler",      # For formatting code to the style guide
  "prettycode"   # Used by styler to provide color-coded output
)

# STEP 2: Install Required Packages
# -------------------------------------------------------------
# Install the packages listed in `required_packages`.
install.packages(required_packages)

# =============================================================
# NOTES:
# -------------------------------------------------------------
# 1. To add more packages, include them in the `required_packages` vector.
# 2. This script will install the packages from CRAN, but not load them into the environment.
# 3. If a package fails to install, ensure:
#    - The package name is correct.
#    - You have internet connectivity.
#    - Your system meets the package's requirements.
# 4. To install packages from GitHub or other sources, see the example below.
# =============================================================

# STEP 3 (Optional): Install Packages from GitHub or Other Sources
# -------------------------------------------------------------
# If you need to install packages from GitHub or other sources, you can include them here.
# Uncomment and modify the lines below as needed.

# Install `remotes` package if not already installed
# if (!"remotes" %in% installed.packages()[, "Package"]) install.packages("remotes")

# Example: Install GitHub packages
# github_packages <- c(
#   "r-lib/usethis",   # Example: 'usethis' package from r-lib
#   "hadley/scales"    # Example: 'scales' package from Hadley Wickham's repo
# )
# lapply(github_packages, function(pkg) {
#   remotes::install_github(pkg)
# })

# message("Package installation process completed!") # Optional message, can be uncommented if needed
