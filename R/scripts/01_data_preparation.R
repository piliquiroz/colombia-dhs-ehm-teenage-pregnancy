# Data Preparation Script for DHS Colombia Survey
# 
# Purpose: Preprocess demographic survey data to analyze teenage pregnancy 
#          in the context of household migration
# 
# Data Sources: 
#   - IR (Individual Responses): Women's survey responses
#   - PR (Household Roster): Person-level household data
#   - HR (Household Data): Household-level information
# 
# Key Preprocessing Steps:
#   1. Load raw survey datasets
#   2. Filter for teenage girls (ages 13-19)
#   3. Identify households with emigrants

# Source project-wide configurations and utility functions
source("R/scripts/preamble.R")

# Data Processing --------------------------------------------------------------

# Load and filter datasets
ir <- load_cols(pth.ir_dat, pth.ir_col) %>% 
  filter(n_age >= 13 & n_age <= 19)

pr <- load_cols(pth.pr_dat, pth.pr_col)

hr <- load_cols(pth.hr_dat, pth.hr_col) %>% 
  filter(p_has_ehm == 1)

# Household Recode (HR) Data Processing ----------------------------------------

# Look through the rows of the HR and create the following columns:
#   - n_ehm: total number of emigrants in this household
#   - n_{country}: number of emigrants to each specific country
#   - n_ehm_man: number of male emigrants
#   - n_ehm_wom: number of female emigrants
#   - n_ehm_trans: number of trans emigrants

# Define countries and their numeric codes
countries <- tibble(
  tag = c("venezuela", "us", "spain", "ecuador", "panama", 
          "canada", "chile", "mexico", "brazil", "argentina", 
          "france", "italy", "uk", "australia", "peru", 
          "germany", "other_country"),
  num = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 96)
)

# Helper function to check if a value is non-NA and matches a specific number
f <- function(c, n) !is.na(c) & (c == n)

# Function to count emigrants by country
# Systematically checks multiple columns for emigrant destination
count_country <- function(tag) {
  num <- countries$num[countries$tag == tag]
  x <- rep(0, nrow(hr))
  for (i in 1:10) {
    s <- sprintf("f_floc_%02d", i)
    x <- x + (!is.na(hr[[s]]) & hr[[s]] == num)
  }
  x
}

# Create column of migrant counts for each country
hr$n_venezuela     <- count_country("venezuela")
hr$n_us            <- count_country("us")
hr$n_spain         <- count_country("spain")
hr$n_ecuador       <- count_country("ecuador")
hr$n_panama        <- count_country("panama")
hr$n_canada        <- count_country("canada")
hr$n_chile         <- count_country("chile")
hr$n_mexico        <- count_country("mexico")
hr$n_brazil        <- count_country("brazil")
hr$n_argentina     <- count_country("argentina")
hr$n_france        <- count_country("france")
hr$n_italy         <- count_country("italy")
hr$n_uk            <- count_country("uk")
hr$n_australia     <- count_country("australia")
hr$n_peru          <- count_country("peru")
hr$n_germany       <- count_country("germany")
hr$n_other_country <- count_country("other_country")

# Add up all the countries to get the n_ehm total
hr$n_ehm <-
  hr$n_venezuela +
  hr$n_us +
  hr$n_spain +
  hr$n_ecuador +
  hr$n_panama +
  hr$n_canada +
  hr$n_chile +
  hr$n_mexico +
  hr$n_brazil +
  hr$n_argentina +
  hr$n_france +
  hr$n_italy +
  hr$n_uk +
  hr$n_australia +
  hr$n_peru +
  hr$n_germany +
  hr$n_other_country

test_n <- function(n) {
  # Look only at the rows where the n_ehm matches the argument
  nr <- hr[hr$n_ehm == n, ]
  
  # Get the column for the n'th ehm
  v1 <- nr[, as.vector(sprintf("f_floc_%02d", n))]
 
   # Get the column for the (n+1)'th ehm
  v2 <- nr[, as.vector(sprintf("f_floc_%02d", n + 1))]
  all(!is.na(v1)) &&  # None of the n'th ehms are missing
    all(is.na(v2))    # All of the (n+1)th ehms are missing
}

# Country extraction seems to be fine
for (i in 1:max(hr$n_ehm)) print(test_n(i))

# Labels and tags for sex-of-migrant
sexes = as_tibble(data.frame(
  tag=c("male", "female", "trans"),
  num=c(1, 2, 3)))

# Check all 10 sex-of-ehm-columns for sex
count_sex <- function (tag) {
  num <- sexes[sexes$tag == tag,]$num
  x <- rep(0, nrow(hr))
  for (i in 1:10) {
    s <- as.vector(sprintf("f_fsex_%02d", i))
    x <- x + f(hr[,s], num)}
  as.vector(x)}

# Create column of migrant counts for each country
hr$n_ehm_man  <- count_sex("male")
hr$n_ehm_wom  <- count_sex("female")
hr$n_ehm_trans  <- count_sex("trans")

b <- (hr$n_ehm_man + hr$n_ehm_wom + hr$n_ehm_trans) == hr$n_ehm
all(b)

# Person Recode (PR) Data Processing ------------------------------------------

# Create derived columns in the PR dataset:
#   - p_tirl: Identifies teenage girls who completed the Individual Recode (IR) survey
#     * TRUE if the person is in the teenage girls sample
#
#   - p_exehm: Identifies individuals with previous international residence
#     * TRUE if:
#       - Previous residence was abroad (f_res_prev == 3)
#       - OR residence 5 years ago was abroad (f_res_5yago == 3)

# Identify Teenage Girls in Survey --------------------------------------------

# Identify teenage girls who completed the Individual Recode (IR) survey
# Uses is_member to match across cluster, household, and line indices
pr$p_tirl <- is_member(pr, ir, ir_idx)

# Validate teenage girl identification
tirl_count <- sum(pr$p_tirl)
message(sprintf("Teenage girls identified: %d", tirl_count))

# International Migration History --------------------------------------------

# Identify individuals with previous international residence
# Checks both previous residence and residence 5 years ago
pr$p_exehm <- is_really(pr$f_res_prev, 3) | is_really(pr$f_res_5yago, 3)

# Validate migration history
exehm_count <- sum(pr$p_exehm)
message(sprintf("Individuals with international residence history: %d", exehm_count))

# Helper Frames for Further Analysis ------------------------------------------

# Identify households currently with emigrants
# Unique households with at least one current emigrant
hh_hf <- pr %>% 
  filter(p_has_ehm == 1) %>% 
  distinct(x_cluster, x_hh)

# Validate current emigrant households
message(sprintf("Households with current emigrants: %d", nrow(hh_hf)))

# Identify households with past emigrants (excluding teenage girls)
# Unique households with past emigrants, excluding teenage girls
hh_xf <- pr %>% 
  filter(p_exehm & !p_tirl) %>% 
  distinct(x_cluster, x_hh)

# Validate past emigrant households
message(sprintf("Households with past emigrants (excluding teenage girls): %d", nrow(hh_xf)))

# Identify teenage girls who were also past emigrants
# Selects identifying information for teenage girls with past emigrant status
xf_tirls <- pr %>% 
  filter(p_exehm & p_tirl) %>%
  select(all_of(ir_idx))

# Validate teenage girls with migration history
message(sprintf("Teenage girls with international residence history: %d", nrow(xf_tirls)))

# Educational Status Processing ----------------------------------------------

# Validate school attendance data
# Ensure p_in_school contains only 0 or 1
stopifnot(all(ir$p_in_school %in% c(0, 1)))

# Educational Level Coding Scheme:
# 0: No education
# 1: Incomplete primary
# 2: Complete primary
# 3: Incomplete secondary
# 4: Complete secondary
# 5: Higher education

# Secondary Education Status Classification:
# 1: Not in school, incomplete secondary
# 2: In school, incomplete secondary
# 3: Complete secondary or higher

# Initialize secondary education status
ir$f_secondary <- 1

# Update status for students
ir$f_secondary[ir$p_in_school == 1] <- 2

# Update status for completed secondary education
ir$f_secondary[ir$f_edu %in% c(4, 5)] <- 3

# Identify potential dropouts
ir$f_dropout <- as.numeric(ir$f_secondary == 1)

# Validation of educational status
edu_summary <- table(ir$f_secondary)
message("Educational Status Distribution:")
print(edu_summary)


# Individual Recode (IR) Data Processing --------------------------------------

# Create derived columns in the IR dataset:
#   - p_ever_preg: Identifies pregnancy status
#     * TRUE if:
#       - Currently pregnant
#       - Has given birth
#       - Has had terminations
#
#   - p_has_ehm: Identifies if in a household with current emigrants
#   - p_has_exehm: Identifies if in a household with past emigrants
#   - p_was_ehm: Identifies if the individual was a past emigrant

# Pregnancy Status Identification
# Defines pregnancy based on multiple indicators:
#   - Current pregnancy
#   - Previous births
#   - Pregnancy terminations
ir$p_ever_preg <- (ir$p_preg == 1) | (ir$n_birth > 0) | (ir$n_term > 0)

# Validate pregnancy status
preg_count <- sum(ir$p_ever_preg)
message(sprintf("Teenage girls with pregnancy history: %d (%.2f%%)", 
                preg_count, 
                (preg_count / nrow(ir)) * 100))

# Household Emigrant Status
# Uses helper frames created in previous processing steps
# Identifies:
#   - Households with current emigrants
#   - Households with past emigrants
#   - Individual migration history
ir$p_has_ehm <- is_member(ir, hh_hf, hh_idx)
ir$p_has_exehm <- is_member(ir, hh_xf, hh_idx)
ir$p_was_ehm <- is_member(ir, xf_tirls, ir_idx)

# Validate emigrant household and migration status
message(sprintf(
  "Emigrant Household Status:\n  Current Emigrants: %d (%.2f%%)\n  
  Past Emigrants: %d (%.2f%%)\n  Individual Migration History: %d (%.2f%%)",
  sum(ir$p_has_ehm), (sum(ir$p_has_ehm) / nrow(ir)) * 100,
  sum(ir$p_has_exehm), (sum(ir$p_has_exehm) / nrow(ir)) * 100,
  sum(ir$p_was_ehm), (sum(ir$p_was_ehm) / nrow(ir)) * 100
))

# Join Datasets ----------------------------------------------------------------

#  join the country/sex ehm-counts from the HR to the IR
hr_cols = c(
  # Household indexing variables
  "x_cluster",
  "x_hh",
  # ehms per target country counts
  "n_venezuela",
  "n_us",
  "n_spain",
  "n_ecuador",
  "n_panama",
  "n_canada",
  "n_chile",
  "n_mexico",
  "n_brazil",
  "n_argentina",
  "n_france",
  "n_italy",
  "n_uk",
  "n_australia",
  "n_peru",
  "n_germany",
  "n_other_country",
  # ehm gender counts
  "n_ehm_man",
  "n_ehm_wom",
  "n_ehm_trans",
  # Total ehm count
  "n_ehm")
ir <- left_join(ir, hr[,hr_cols], by=c("x_cluster", "x_hh"))

# Join the dropout column from the PR to the IR

pr_cols = c(
  # Individual indexing variables
  "x_cluster",
  "x_hh",
  "x_line",
  
  # Values to copy over
  "f_why_dropout"
)
ir <- left_join(ir, pr[,pr_cols], by=c("x_cluster", "x_hh", "x_line"))

# Handle Missing Values --------------------------------------------------------

# Replace NA with 0 for numeric columns
fix_na <- function (s) {
  ir[is.na(ir[, s]), s] <- 0
}

for (s in colnames(ir)) {
  if (startsWith(s, "n_")) {
    ir[is.na(as.vector(ir[,s])), s] <- 0      
  }
}

# Regional Aggregations --------------------------------------------------------

# add up all the latin-american countries
ir$n_latam <-
  ir$n_venezuela +
  ir$n_ecuador +
  ir$n_panama +
  ir$n_chile +
  ir$n_mexico +
  ir$n_brazil +
  ir$n_argentina +
  ir$n_peru

# add up all 'western' countries
ir$n_west <-
  ir$n_us +
  ir$n_spain +
  ir$n_canada +
  ir$n_france +
  ir$n_italy +
  ir$n_uk +
  ir$n_australia +
  ir$n_germany

# add up all North America countries
ir$n_northamerica <-  
  ir$n_us +
  ir$n_canada 

# add up all European Countries 
ir$n_europe <-
  ir$n_spain +
  ir$n_france +
  ir$n_italy +
  ir$n_uk +
  ir$n_germany

#add up Australia and other countries
ir$n_australia_other <-  
  ir$n_australia +
  ir$n_other_country

# note, we skip the 'n_other_country' category
# all(ir$n_ehm == (ir$n_latam + ir$n_west + ir$n_other_country))
all(ir$n_ehm == (ir$n_latam + ir$n_northamerica + ir$n_europe +  ir$n_australia_other))

# Final Data Validation --------------------------------------------------------

# Basic dataset overview
cat("Dataset Validation:\n")
cat("Total sample size:", nrow(ir), "\n")
cat("Age range:", range(ir$n_age), "\n")

# Key variable summaries
validation_summary <- list(
  pregnant_teens = sum(ir$p_ever_preg),
  emigrant_households = sum(ir$p_has_ehm)
)

# Print validation summary
print(validation_summary)

# Ensure critical conditions
stopifnot(
  nrow(ir) > 0,
  all(ir$n_age >= 13 & ir$n_age <= 19),
  all(ir$p_ever_preg %in% c(TRUE, FALSE))
)

# Confirmation message
message("Data preparation completed successfully!")

# Save Processed Data ----------------------------------------------------------

# Ensure processed data directory exists
dir.create(pth.processed_dir, recursive = TRUE, showWarnings = FALSE)

# Save processed data
write_dta(ir, pth.sample_dat)  # Primary Stata file
saveRDS(ir, file.path(pth.processed_dir, "ir_processed.rds"))  # R backup

# Print summary
print(summary(ir))
# CLEAN UP  --------------------------------------------------------------------
# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
