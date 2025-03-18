# 01_data_preparation.R
# Data Preparation Script for DHS Colombia Survey

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
pth.processed_dir <- file.path(pth.dat_dir, "processed")       # Processed data directory
pth.ir_dat     <- file.path(pth.raw_dir, "IR", "COIR72FL.DTA") # IR-survey results
pth.pr_dat     <- file.path(pth.raw_dir, "PR", "COPR72FL.DTA") # PR-survey results
pth.hr_dat     <- file.path(pth.raw_dir, "HR", "COHR72FL.DTA") # HR-survey results

# Paths for column specification files
pth.ir_col     <- file.path(pth.raw_dir, "ir_columns.md") # IR column-selections
pth.pr_col     <- file.path(pth.raw_dir, "pr_columns.md") # PR column-selections
pth.hr_col     <- file.path(pth.raw_dir, "hr_columns.md") # HR column-selections

# Output path
pth.sample_dat <- file.path(pth.processed_dir, "sample.dta")   # Preprocessed data file

# Setup standard indices -------------------------------------------------------
# To reference individual members, we need to check their cluster, household and
# line-number. To reference whole households, we reference only the cluster and
# household. These are DHS-specific conventions.
ir_idx <- c("x_cluster", "x_hh", "x_line") # Individual index columns
hh_idx <- c("x_cluster", "x_hh")           # Household index columns

# Utility Functions ------------------------------------------------------------

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

# Check membership across datasets
is_member <- function(big, small, cols) {
  tmp <- small[,cols]
  tmp$it <- TRUE
  foo <- left_join(big, tmp, by=cols)
  !is.na(foo$it)
}

# Compare factor with specific value
is_really <- function(p, v) {p == v & !is.na(p)}


# Country and Emigrant Categorization -------------------------------------------

# Define countries and their numeric codes
countries <- tibble(
  tag = c("venezuela", "us", "spain", "ecuador", "panama", 
          "canada", "chile", "mexico", "brazil", "argentina", 
          "france", "italy", "uk", "australia", "peru", 
          "germany", "other_country"),
  num = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 96)
)

# Function to count emigrants by country
count_country <- function(tag) {
  num <- countries$num[countries$tag == tag]
  x <- rep(0, nrow(hr))
  for (i in 1:10) {
    s <- sprintf("f_floc_%02d", i)
    x <- x + (!is.na(hr[[s]]) & hr[[s]] == num)
  }
  x
}

# Function to count emigrants by sex
count_sex <- function(tag) {
  sexes <- tibble(
    tag = c("male", "female", "trans"),
    num = c(1, 2, 3)
  )
  
  num <- sexes$num[sexes$tag == tag]
  x <- rep(0, nrow(hr))
  for (i in 1:10) {
    s <- sprintf("f_fsex_%02d", i)
    x <- x + (!is.na(hr[[s]]) & hr[[s]] == num)
  }
  x
}

# Data Processing --------------------------------------------------------------

# Load and filter datasets
ir <- load_cols(pth.ir_dat, pth.ir_col) %>% 
  filter(n_age >= 13 & n_age <= 19)

pr <- load_cols(pth.pr_dat, pth.pr_col)

hr <- load_cols(pth.hr_dat, pth.hr_col) %>% 
  filter(p_has_florp == 1)

# Add country-specific emigrant columns
hr <- hr %>%
  mutate(
    n_venezuela = count_country("venezuela"),
    n_us = count_country("us"),
    n_spain = count_country("spain"),
    n_ecuador = count_country("ecuador"),
    n_panama = count_country("panama"),
    n_canada = count_country("canada"),
    n_chile = count_country("chile"),
    n_mexico = count_country("mexico"),
    n_brazil = count_country("brazil"),
    n_argentina = count_country("argentina"),
    n_france = count_country("france"),
    n_italy = count_country("italy"),
    n_uk = count_country("uk"),
    n_australia = count_country("australia"),
    n_peru = count_country("peru"),
    n_germany = count_country("germany"),
    n_other_country = count_country("other_country"),
    
    # Total emigrants
    n_florp = n_venezuela + n_us + n_spain + n_ecuador + n_panama + 
      n_canada + n_chile + n_mexico + n_brazil + n_argentina + 
      n_france + n_italy + n_uk + n_australia + n_peru + 
      n_germany + n_other_country,
    
    # Emigrant sex counts
    n_florp_man = count_sex("male"),
    n_florp_wom = count_sex("female"),
    n_florp_trans = count_sex("trans")
  )

# Identify households with current and past emigrants
hh_hf <- pr %>% 
  filter(p_has_florp == 1) %>% 
  distinct(x_cluster, x_hh)

hh_xf <- pr %>% 
  filter(p_exflorp & !p_tirl) %>% 
  distinct(x_cluster, x_hh)

# Teenage girls who were also past emigrants
xf_tirls <- pr %>% 
  filter(p_exflorp & p_tirl) %>%
  select(all_of(ir_idx))

# Process Individual Data
ir <- ir %>%
  mutate(
    # Pregnancy status
    p_ever_preg = (p_preg == 1) | (n_birth > 0) | (n_term > 0),
    
    # Emigrant household status
    p_has_florp = is_member(., hh_hf, hh_idx),
    p_has_exflorp = is_member(., hh_xf, hh_idx),
    p_was_florp = is_member(., xf_tirls, ir_idx),
    
    # Education status
    f_secondary = case_when(
      p_in_school == 0 & f_edu < 4 ~ 1,  # No secondary
      p_in_school == 1 & f_edu < 4 ~ 2,  # Attending secondary
      f_edu >= 4 ~ 3                     # Completed secondary
    ),
    
    f_dropout = if_else(f_secondary == 1, 1, 0)
  )

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
  
  freqL <- c("Always", "Almost always", "Few times", "Never", "Does not live with parent")
  dat$f_know_where <- factor(dat$f_know_where, labels=freqL)
  dat$f_know_friend <- factor(dat$f_know_friend, labels=freqL)
  dat$f_know_school <- factor(dat$f_know_school, labels=freqL)
  dat$f_curfew <- factor(dat$f_curfew, labels=freqL)
  
  wealthL <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")
  dat$f_wealth <- factor(dat$f_wealth, labels=wealthL)
  
  countL <- c("None", "Some", "A lot", "All", "Don't know")
  countL2 <- c("None", "Some", "A lot", "All", "Don't know", "Friends not having sex")
  dat$f_friend_sex <- factor(dat$f_friend_sex, labels=countL)
  dat$f_friend_preg<- factor(dat$f_friend_preg, labels=countL)
  dat$f_friend_condom<- factor(dat$f_friend_condom, labels=countL)
  
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
# Join Datasets ----------------------------------------------------------------

# Columns to keep from household data
hr_cols <- c(
  "x_cluster", "x_hh",
  starts_with("n_"),
  starts_with("f_")
)

# Columns to keep from person-level data
pr_cols <- c(
  "x_cluster", "x_hh", "x_line", 
  "f_why_dropout"
)

# Join datasets
ir <- ir %>%
  left_join(hr[, hr_cols], by = c("x_cluster", "x_hh")) %>%
  left_join(pr[, pr_cols], by = c("x_cluster", "x_hh", "x_line"))

# Handle Missing Values --------------------------------------------------------

# Replace NA with 0 for numeric columns
ir <- ir %>%
  mutate(across(starts_with("n_"), ~replace_na(., 0)))

# Regional Aggregations --------------------------------------------------------

ir <- ir %>%
  mutate(
    n_latam = n_venezuela + n_ecuador + n_panama + n_chile + 
      n_mexico + n_brazil + n_argentina + n_peru,
    n_west = n_us + n_spain + n_canada + n_france + n_italy + 
      n_uk + n_australia + n_germany,
    n_northamerica = n_us + n_canada,
    n_europe = n_spain + n_france + n_italy + n_uk + n_germany,
    n_australia_other = n_australia + n_other_country
  )

# Final Data Validation --------------------------------------------------------

# Verify total emigrants match sum of regional emigrants
stopifnot(all(n_florp == (n_latam + n_northamerica + n_europe + n_australia_other)))

# Save Processed Data ----------------------------------------------------------

# Ensure processed data directory exists
dir.create(pth.processed_dir, recursive = TRUE, showWarnings = FALSE)

# Save processed data
saveRDS(ir, file.path(pth.processed_dir, "ir_processed.rds"))
write_dta(ir, pth.sample_dat)

# Print summary
print(summary(ir))