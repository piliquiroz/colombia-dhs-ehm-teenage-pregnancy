# Load required libraries
library(dplyr, warn.conflicts=FALSE)      # Data manipulation (filtering, transforming)
library(haven)                            # Read .dta files
library(here)                             # Consistent file paths across different computers
library(tidyverse, warn.conflicts=FALSE)  # Comprehensive data science toolkit
library(survey)                           # Specialized tools for complex survey analysis

# Setup project paths
pth.root       <- here()                                       # Project directory
pth.dat_dir    <- file.path(pth.root, "data")                  # Data directory
pth.raw_dir    <- file.path(pth.dat_dir, "raw")                # DHS raw data directory
pth.ir_dat     <- file.path(pth.raw_dir, "IR", "COIR72FL.DTA") # IR-survey results
pth.pr_dat     <- file.path(pth.raw_dir, "PR", "COPR72FL.DTA") # PR-survey results
pth.hr_dat     <- file.path(pth.raw_dir, "HR", "COHR72FL.DTA") # HR-survey results

# Paths for column specification files
pth.ir_col     <- file.path(pth.raw_dir, "ir_columns.md") # IR column-selections
pth.pr_col     <- file.path(pth.raw_dir, "pr_columns.md") # PR column-selections
pth.hr_col     <- file.path(pth.raw_dir, "hr_columns.md") # HR column-selections

# Output path
pth.processed_dir <- file.path(pth.dat_dir, "processed")       # Processed data directory
pth.sample_dat <- file.path(pth.processed_dir, "sample.dta")   # Preprocessed data file

# Setup standard indices -------------------------------------------------------
# To reference individual members, we need to check their cluster, household and
# line-number. To reference whole households, we reference only the cluster and
# household. These are DHS-specific conventions.
ir_idx <- c("x_cluster", "x_hh", "x_line") # Individual index columns
hh_idx <- c("x_cluster", "x_hh")           # Household index columns

# Setup loading functions -------------------------------------------------------

# Load a tibble from a DTA file selecting only columns from a markdown table
# - f: path to `.DTA` dataframe
# - c: path to markdown table containing column selection/renaming scheme
load_cols <- function(f, c) {
  cols <- read.csv(
    c,                                        # Path to the columns file
    header = TRUE,                            # Treat the first row as column-names
    sep = "|",                                # Treat | as column separator
    stringsAsFactors = FALSE,                 # Don't factorize strings
    strip.white = TRUE                        # Do strip white space from values
  )[c("raw_name", "new_name", "description")] # Drop empty border columns
  dat <- read_dta(f)[cols$raw_name]
  colnames(dat) <- cols$new_name
  dat
}

# Load the preprocessed sample and convert a number of columns to factors
load_sample <- function () {
  dat <- read_dta(pth.sample_dat)
  dat$f_has_florp <- factor(dat$p_has_florp, labels=c("None", "At least one"))
  dat$f_has_florp <- factor(dat$p_has_florp, labels=c("None", "At least one"))#any reason why two of this?
  dat$f_ever_preg <- factor(dat$p_ever_preg, labels=c("Never", "At least once"))
  dat$f_has_exflorp <- factor(dat$p_has_exflorp, labels=c("None", "At least one exflorp"))
  dat$f_was_florp <- factor(dat$p_was_florp, labels=c("Local", "Abroad"))
  dat$f_foreign_contact <- factor(
    dat$p_has_florp | dat$p_has_exflorp | dat$p_was_florp,
    labels=c("Not Exposed", "Exposed"))
  
  wealthL <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
  dat$f_wealth <- factor(dat$f_wealth, labels=wealthL)
  
  kmethod <-c("Nono", "Only folkloric", "Only traditional", "modern")
  dat$f_method <- factor(dat$f_method, labels=kmethod)
  
  eduL <- c("None", "I_primary", "C_primary", "I_secondary", "C_secondary", "higher")
  dat$f_edu <- factor(dat$f_edu, labels=eduL)
  
  secondaryL<- c("No", "Attempting", "Complete")
  dat$f_secondary<- factor(dat$f_secondary, labels=secondaryL)
  
  regionL <- c("Atlantica", "Oriental", " Central", "Pacifica", "Bogota", "Orinoquia/Amazonia")
  dat$f_region <- factor(dat$f_region, labels=regionL)
  
  urbanL <- c("Urban", "Rural")
  dat$f_urban <- factor(dat$f_urban, labels=urbanL)
  
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
    "Don't know")
  dat$f_why_dropout <- factor(dat$f_why_dropout, dropoutL)
  
  # Weights are meant to be divided by 1 million
  dat$w_weight = dat$w_weight / 1000000
  
  dat
}
# Setup mark-membership function -----------------------------------------------

# Return a logical vector describing where `big` occurs in `small`
# - big: target dataframe to mark each row for membership
# - small: test dataframe, to check each row from big for membership
# - cols: columns to use to check for membership between big and small
#
# For each row in big, check the whole dataframe `small`, comparing across the
# provided `cols` subset. If that row occurs in small, label that value as TRUE.
# Used, for example, to mark rows in the PR that also occur in the IR.
is_member <- function(big, small, cols) {
  tmp <- small[,cols]
  tmp$it <- TRUE
  foo <- left_join(big, tmp, by=cols)
  !is.na(foo$it)
}
# Setup little utility functions -----------------------------------------------

# Extract the unique values out of a column
get_uniq <- function(c) {
  uc <- c %>% distinct()
  uc
}

# Setup compare-factor function ------------------------------------------------
# Compare factor with specific value
is_really <- function(p, v) {p == v & !is.na(p)}

