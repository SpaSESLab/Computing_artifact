
![SpaSES_Logo](https://github.com/user-attachments/assets/1b41df1c-47c8-480e-92b1-c4dc5fc4571e)

# Project Repository Template

Welcome to the SpaSES Lab project repository template! This repository is designed to help organize your project with standardized practices for managing code, data, and metadata. The following sections provide instructions on how to effectively use this repository template.

## Table of Contents
1. [How to Use This Repository](#how-to-use-this-repository)
2. [Creating Project Metadata](#creating-project-metadata)
3. [Downloading and Uploading Data to and from the SpaSES Google Drive](#downloading-and-uploading-data-to-and-from-the-spases-google-drive)
4. [Standards for Documenting Code](#standards-for-documenting-code)
5. [Committing, Pushing, and Pulling Changes](#committing-pushing-and-pulling-changes)
6. [Using `.gitignore`](#using-gitignore)
7. [`README.md` File Guidelines](#readmemd-file-guidelines)

---

## How to Use This Repository

This repository serves as a template for managing your project and collaborating with others in the SpaSES Lab. It provides:

- A consistent file structure for storing code, data, and metadata.
- Templates for downloading and uploading project data to and from the SpaSES Google Drive.
- Tutorials for using styling and linting packages.
- Dataspice package tutorial for creating machine‑readable metadata and human‑friendly documentation.
- Guidelines for documenting your code, managing version control, and maintaining a well-organized project.

To use this repository:
- Click on the `Use this template` button, located in the upper right corner, then click `Create new repository`.
- You'll be directed to a new page, fill out the neccessary criteria, relevant to your project, then click `Create repository`. 
- Toward the right side of the screen, click on the `Code` button, then copy your repository's URL to your clipboard.
- Open RStudio on your local environment, in the `Project` dropdown menu, upper right corner, click `New Project` > `Version Control` > `Git`.
- Paste your repository's URL, provide a name and location for your directory, then click `Create Project`.

Once you’ve created your own repository with this template and cloned it to your local environment, follow the instructions in each section to properly set up and maintain your project.

---

## Creating Project Metadata

Well‑crafted metadata is in most cases a neccessity, as it transforms raw files into FAIR (Findable, Accessible, Interoperable, Reusable) assets—maximizing your data’s visibility, utility, and longevity, often described as "data about data". 

Some find that manually creating comprehensive metadata can be time‑consuming and error‑prone, as it requires meticulously documenting every aspect of your data by hand. To streamline that effort, the `dataspice` package provides a simple, scriptable workflow to auto‑generate metadata templates, guide you through populating them, and create machine‑readable metadata (standardized `JSON-LD` file) and human‑friendly documentation (a `README`-style webpage). Located in the `scripts/utilities/project_metadata/` directory, you'll find a script, called `dataspice_tutorial_script.R`, that acts as a tutorial on how to use the `dataspice` package, using fake example data, providing guidence on applying this package to your workflow.

Another `dataspice` tutorial can be found here: https://annakrystalli.me/dataspice-tutorial/


The `dataspice` GitHub Repository can be found here: https://github.com/ropensci/dataspice

#### Ensure that the metadata is kept up to date as the project progresses.

## The `docs/` Directory

This directory serves a similar role to metadata, but it's specifically intended to store documentation that helps humans understand your data, rather than information used by machines. This includes, but is not limited to:

- The `dataspice` `README`-style `HTML`containing tables of variables, interactive maps, data previews, etc.
- R Markdown files or similar tools for documenting analyses that require detailed explanations and results
- Other documentation materials
- Help Files
- And More

---

## Downloading and Uploading Data to and from the SpaSES Google Drive

This repository includes templates for downloading and uploading data from/to the SpaSES Google Drive. For both downloading and uploading, ensure you have the necessary permissions to access the Google Drive folder. Here’s how to use the provided templates:

### 1. Downloading Data from SpaSES Google Drive

To download project data from the SpaSES Google Drive to your local R environment, you can use the provided script `download_data.R` found in the `scripts/utilities/` directory. Run the script, following the provided steps and customizing as needed, to authenticate and download the files into your local project directory.

### 2. Uploading Data to SpaSES Google Drive

To upload data from your local R environment to the SpaSES Google Drive, use the provided script `upload_data.R` found in the `scripts/utilities/` directory. Run the script, following the provided steps and customizing as needed, to authenticate and upload the files to the designated project folder.

---

## Standards for Documenting Code

Writing clear, readable, and well-documented code is essential for collaboration and long-term project sustainability. Follow these best practices for documenting your code:

### Commenting Code
- Use comments to explain the purpose of functions, key steps, and non-obvious logic.
- Use inline comments to clarify specific lines of code where needed.
- Keep comments concise but descriptive.

### File, Function, and Variable Naming
- Use meaningful names that clearly describe the purpose of the function or variable.
- Follow a consistent naming convention (e.g., `snake_case`) throughout your scripts.

### Script Organization
- Organize scripts into sections using clear comments (e.g., `### Data Cleaning`, `### Data Analysis`).
- Consider separating large scripts into smaller, modular functions or scripts based on functionality.

### Code Formatting (Style Guide)
- A style guide is a set of standards for the formatting of information, consistency of this creates a shared understanding within and across files and projects.
- A commonly used style guide in the R community is the `tidyverse style guide`, which outlines clear conventions for writing clean, consistent, and readable code that fits naturally with the tidyverse ecosystem.
- Packages that support code maintenance and consistent style adherence include:
  - `styler`: Automatically formats R code according to a consistent style guide to improve readability and maintainability.
  - `lintr`: Analyzes R code to detect potential issues, enforce coding standards, and suggest improvements.
- A `lintr` and `styler` tutorial, called `style_guide_tutorial.rmd` can be found in the `scripts/utilities/` directory, showing the utility of these packages.

The `styler` GitHub Repository can be found here: https://github.com/r-lib/styler


The `lintr` GitHub Repository can be found here: https://github.com/r-lib/lintr

### Documentation Tools
- Use R Markdown or similar tools for documenting analyses that require detailed explanations and results.

### Managing Dependencies with `packages.R`
- A script template called `packages.R` is included in the `scripts/utilities/` directory, and can be used for managing required packages.
- Use `packages.R` to install all the necessary packages for running your scripts. Add or remove package installation commands as needed for your project.
- To load packages, use `library()` calls in your individual scripts to load all required packages at the very beginning of each file. This ensures that dependencies are loaded before executing the rest of the code.

---

## Committing, Pushing, and Pulling Changes

Effective version control is essential for tracking changes, collaborating with others, and ensuring your project evolves in an organized manner. Follow these GitHub best practices:

### Committing Changes
- Commit your changes regularly with clear, descriptive commit messages.
- Each commit should represent a logical unit of work, such as a completed feature or a fixed bug.
- Example:
```r
git commit -m "Added function for data cleaning"
```

### Pushing Changes:
- Push your changes to the remote repository after committing them locally.
- Example:
```r
git push origin main
```

### Pulling Changes:
- Before starting work each day, pull the latest changes from the main branch to keep your local repository up to date.
- Example: 
```r
git pull origin main
```

### Branching:
- Use branches to develop new features or fix bugs. Once a branch is complete, merge it back into the main branch.

---

## Using `.gitignore`

You can configure Git to ignore files you don't want to check in to GitHub.

- The `.gitignore` file in you repository's root directory will tell Git which files and directories to ignore when you make a commit.


Here is an example of how it can be used, by adding this into the `.gitignore` file... 
```r
data/original/*
```
... it tells Git to ignore everying inside the `data/original` directory when commiting. 

### Using `.gitkeep` with `.gitignore`:
However, Git doesn't track empty folders. 
If you want to ensure the `data/original` directory still exists in the repository, while still ignoring it's contents, you will first need to create a `.gitkeep` file.
You can do so like this in the terminal...
```r
touch data/original/.gitkeep
```
... and then add the following lines in the `.gitignore` file...
```r
# ignore files in the folder
data/original/*

# but keep the folder
!data/original/.gitkeep
```
... the `!` negates the ignore rule, meaning Git will track the `.gitkeep` file inside the `data/original/` directory. 
- This is helpful when you want to ignore everything inside a directory, but at the same time want to check-in the folder and keep the directory structure.

### For Mac Users: What are the `.DS_Store` Files?

The `.DS_Store` (Desktop Service Store) file is a hidden macOS system file that appears in every directory that a Mac user opens. It is automatically created by Finder, to store the metadata about the folder it is in, things like; icon positions, view options, window size and position, sorting preferences, etc.. 

They are not needed for most projects and can be ignored in Git by adding these lines to your `.gitignore` file...
```r
# simple approach for ignoring .DS_Store files everywhere in the repository
.DS_Store

# more explicit approach for ignoring .DS_Store file in all subdrectories
**/.DS_Store
```
... I suggest adding both lines, as it just enures all `.DS_Store` files, even deepy nested ones, will be ignored, as sometimes `.gitignore` can have unusual rules affecting recursion. 


---

## `README.md` File Guidelines
A good `README.md` file is essential for explaining the purpose of the project, how to use it, and providing instructions for others who may be working with the repository. Here are the key components to include:

### Project Title and Description:
- Provide a clear title and description of the project’s goals and objectives.

### Installation Instructions:
- Explain how to set up the project environment, including necessary dependencies and tools (e.g., R packages, external software).

### Usage Instructions:
- Provide clear examples of how to use the scripts, templates, or functions included in the repository.

### Contributing:
- If you are open to contributions, provide guidelines for contributing to the project.

### License Information:
- Include licensing information to indicate how others can use and modify the project.

### Contact Information:
- Provide information on how to contact the project maintainers for questions or issues.

#### Remember, a well-structured `README.md` improves the usability and collaboration potential of the repository!

---
