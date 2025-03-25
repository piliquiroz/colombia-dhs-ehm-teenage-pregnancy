# Data Preparation Script for DHS Colombia Survey
source("R/scripts/preamble.R")
# Data Processing --------------------------------------------------------------

# Load and filter datasets
ir <- load_cols(pth.ir_dat, pth.ir_col) %>% 
  filter(n_age >= 13 & n_age <= 19)

pr <- load_cols(pth.pr_dat, pth.pr_col)

hr <- load_cols(pth.hr_dat, pth.hr_col) %>% 
  filter(p_has_ehm == 1)

# Household Recode (HR) Data Processing ---------------------------------------

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

# NOTE: there is 1 household with a trans ehm, but that hh has no tirls

# Person Recode (PR) Data Processing ------------------------------------------

# Create derived columns in the PR dataset:
#   - p_tirl: Identifies teenage girls who completed the Individual Recode (IR) survey
#     * TRUE if the person is in the teenage girls sample
#
#   - p_exehm: Identifies individuals with previous international residence
#     * TRUE if:
#       - Previous residence was abroad (f_res_prev == 3)
#       - OR residence 5 years ago was abroad (f_res_5yago == 3)

# Identify teenage girls who completed the Individual Recode (IR) survey
pr$p_tirl <- is_member(pr, ir, ir_idx)

# Identify individuals with previous international residence
pr$p_exehm <- is_really(pr$f_res_prev, 3) | is_really(pr$f_res_5yago, 3)

# Helper Frames for Further Analysis -------------------------------------------

# Identify households currently with emigrants
# Selects unique households where at least one member is a current emigrant
hh_hf <- pr %>% 
  filter(p_has_ehm == 1) %>% 
  distinct(x_cluster, x_hh)

# Identify households with past emigrants (excluding teenage girls)
# Selects unique households with past emigrants who are not teenage girls
hh_xf <- pr %>% 
  filter(p_exehm & !p_tirl) %>% 
  distinct(x_cluster, x_hh)

# Identify teenage girls who were also past emigrants
# Selects identifying information for teenage girls with past emigrant status
xf_tirls <- pr %>% 
  filter(p_exehm & p_tirl) %>%
  select(all_of(ir_idx))

# Educational Status Processing ----------------------------------------------

# NOTE: ir$p_in_school has *no* missing values, they are all either 0 (not attending) or 1 (attending)
sum(ir$p_in_school == 0 | ir$p_in_school == 1) == nrow(ir)

# NOTE: levels of f_edu
# 0         no education
# 1   incomplete primary
# 2     complete primary
# 3 incomplete secondary
# 4   complete secondary
# 5               higher
#
# <4: not finished with secondary yet

# f_secondary
# 1: !in-school && schooling <4   # No
# 2: in-school && schooling <4    # Attending
# 3: schooling >= 4               # Complete

ir$f_secondary <- 1
# Set anyone in school to 2
ir$f_secondary[ir$p_in_school == 1] = 2
# Set anyone who has finished secondary or higher to 3
ir$f_secondary[ir$f_edu == 4 | ir$f_edu == 5] = 3 # complete

ir$f_dropout <- 0
ir$f_dropout[ir$f_secondary == 1] = 1


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

# Pregnancy Status
# TRUE if: currently pregnant, has given birth, or has had terminations
ir$p_ever_preg <- (ir$p_preg == 1) | (ir$n_birth > 0) | (ir$n_term > 0)

# Household Emigrant Status
# Identifies household and individual emigration status using helper frames
ir$p_has_ehm <- is_member(ir, hh_hf, hh_idx)
ir$p_has_exehm <- is_member(ir, hh_xf, hh_idx)
ir$p_was_ehm <- is_member(ir, xf_tirls, ir_idx)

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
