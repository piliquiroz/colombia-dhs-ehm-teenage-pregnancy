# Preamble for Teenage Pregnancy Analysis
# Purpose: Load libraries, set up project paths, and define utility functions
# for analyzing DHS Colombia survey data

# Load libraries
library(dplyr)      # Data manipulation
library(haven)      # Read .dta files
library(here)       # Consistent file paths across computers
library(tidyverse)  # Comprehensive data science toolkit
library(survey)     # Tools for complex survey analysis
library(knitr)      # Generate markdown tables

# Project Directory Setup ------------------------------------------------------
# Centralized path management for consistent file referencing
pth.root     <- here()                        # Project root directory
pth.dat_dir  <- file.path(pth.root, "data")   # Data directory
pth.raw_dir  <- file.path(pth.dat_dir, "raw") # Raw DHS data directory

# Survey-specific data paths
pth.ir_dat   <- file.path(pth.raw_dir, "IR", "COIR72FL.DTA") # Individual responses
pth.pr_dat   <- file.path(pth.raw_dir, "PR", "COPR72FL.DTA") # Household roster
pth.hr_dat   <- file.path(pth.raw_dir, "HR", "COHR72FL.DTA") # Household data

# Column specification paths
pth.ir_col     <- file.path(pth.raw_dir, "ir_columns.md") # IR column selections
pth.pr_col     <- file.path(pth.raw_dir, "pr_columns.md") # PR column selections
pth.hr_col     <- file.path(pth.raw_dir, "hr_columns.md") # HR column selections

# Output paths
pth.processed_dir <- file.path(pth.dat_dir, "processed")       # Processed data directory
pth.sample_dat <- file.path(pth.processed_dir, "sample.dta")   # Preprocessed data file

# DHS Survey Indexing ----------------------------------------------------------
# Standard indices for referencing individuals and households
ir_idx <- c("x_cluster", "x_hh", "x_line") # Individual index columns
hh_idx <- c("x_cluster", "x_hh")           # Household index columns

# Utility Functions ------------------------------------------------------------

# Load columns from DTA file based on markdown specification
load_cols <- function(f, c) {
  # Read column specifications from markdown file
  cols <- read.csv(
    c,
    header = TRUE,
    sep = "|",
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )[c("raw_name", "new_name", "description")]

  # Read specified columns from Stata file
  dat <- read_dta(f)[cols$raw_name]
  colnames(dat) <- cols$new_name
  dat
}

# Load preprocessed sample and convert columns to factors
load_sample <- function() {
  # Load preprocessed data
  dat <- read_dta(pth.sample_dat)
  # Convert columns to factors with meaningful labels

  # Emigrant Household Member (EHM) status
  dat$f_has_ehm <- factor(dat$p_has_ehm, labels = c("None", "At least one"))

  # Pregnancy status
  dat$f_ever_preg <- factor(dat$p_ever_preg, labels = c("Never", "At least once"))
  
  # Returnees
  dat$f_has_exehm <- factor(dat$p_has_exehm, labels = c("None", "At least one returnee"))
  
  # Migration status
  dat$f_was_ehm <- factor(dat$p_was_ehm, labels = c("Local", "Abroad"))
  
  # Foreign contact exposure
  dat$f_foreign_contact <- factor(
    dat$p_has_ehm | dat$p_has_exehm | dat$p_was_ehm,
    labels = c("Not Exposed", "Exposed")
  )
  
  # Wealth categories
  wealthL <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
  dat$f_wealth <- factor(dat$f_wealth, labels = wealthL)
  
  # Education levels
  eduL <- c("None", "Incomplete Primary", "Complete Primary", 
            "Incomplete Secondary", "Complete Secondary", "Higher")
  dat$f_edu <- factor(dat$f_edu, labels = eduL)
  
  # Secondary education status
  secondaryL <- c("No", "Attempting", "Complete")
  dat$f_secondary <- factor(dat$f_secondary, labels = secondaryL)
  
  # Regions
  regionL <- c("Atlantica", "Oriental", "Central", "Pacifica", "Bogota", 
               "Orinoquia/Amazonia")
  dat$f_region <- factor(dat$f_region, labels = regionL)
  
  # Urban/Rural
  urbanL <- c("Urban", "Rural")
  dat$f_urban <- factor(dat$f_urban, labels = urbanL)
  
  # Reasons for dropout
  dropoutL <- c(
    "Got pregnant",
    "Take care of children",
    "Family needed help",
    "Couldn't pay tuition",
    "Too young",
    "In the military",
    "Had to earn money",
    "Graduated/enough study",
    "Didn't pass entry exam",
    "Didn't want to study",
    "No school/too far away",
    "Disabled/handicap",
    "Parents opposed",
    "Other",
    "Don't know"
  )
  dat$f_why_dropout <- factor(dat$f_why_dropout, levels = dropoutL)
  
  # Normalize survey weights
  dat$w_weight <- dat$w_weight / 1000000
  
  return(dat)
}

# Check membership across dataframes
is_member <- function(big, small, cols) {
  tmp <- small[, cols, drop = FALSE]
  tmp$it <- TRUE
  foo <- left_join(big, tmp, by = cols)
  !is.na(foo$it)
}

# Extract the unique values out of a column
get_uniq <- function(c) {
  uc <- c %>% distinct()
  uc
}

# Compare factor with specific value
is_really <- function(p, v) {
  p == v & !is.na(p)
}