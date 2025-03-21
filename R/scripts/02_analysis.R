# 02_analysis.R

# Source the data preparation script
source("R/scripts/preamble.R")

# Load the sample
dat <- load_sample()

# Adding sampling weights---------------------------------

# Create the survey design
design <- svydesign(
  id = ~w_psu,
  strata = ~w_strat,
  weight = ~w_weight,
  data = dat,
  nest = TRUE
)

# Function to generate weighted tables
weighted_table <- function(x) { svytable (x, design=design, round=FALSE)}

weighted_table(~n_age+f_has_florp)
weighted_table(~f_region+f_has_florp)
weighted_table(~f_urban+f_has_florp)
weighted_table(~f_ever_preg+f_has_florp)
weighted_table(~f_wealth+f_has_florp)
weighted_table(~f_secondary+f_has_florp)

# Function to compute totals and percentages

compute_percentages <- function(var) {
  tab <- weighted_table(var)  # Get weighted table
  totals <- rowSums(tab)  # Compute totals per category
  has_florp <- tab[, "At least one"]  # Extract HAS FLORP column
  percentages <- (totals / total_sample) * 100  # Compute percentages
  has_florp_percentage <- (has_florp / totals) * 100  # Compute % (HAS FLORP)
  
  # Convert to data frame for display
  results <- data.frame(
    Category = rownames(tab), 
    Total = round(totals), # Round total counts
    Has_Florp = round(has_florp), # Round HAS FLORP counts
    Percentage = percentages,
    Has_Florp_Percentage = has_florp_percentage)
  return(results)
}

# Compute total sample size
total_sample <- sum(weighted_table(~n_age+f_has_florp)) 

# Compute percentages for all variables
age_results <- compute_percentages(~n_age+f_has_florp)
region_results <- compute_percentages(~f_region+f_has_florp)
urban_results <- compute_percentages(~f_urban+f_has_florp)
ever_preg_results <- compute_percentages(~f_ever_preg+f_has_florp)
wealth_results <- compute_percentages(~f_wealth+f_has_florp)
secondary_results <- compute_percentages(~f_secondary+f_has_florp)


# Print results
print(age_results)
print(region_results)
print(urban_results)
print(ever_preg_results)
print(wealth_results)
print(secondary_results)

# Get the weighted frequency of f_has_florp
florp_table <- foo(~f_has_florp)

# Calculate total N
total_n <- sum(florp_table)

# Extract N that have florp
n_with_florp <- florp_table["At least one"]

# Compute the percentage
pct_with_florp <- (n_with_florp / total_n) * 100

# Display
cat("Total N:", total_n, "\n")
cat("N with florp:", n_with_florp, "\n")
cat("Percent with florp:", pct_with_florp, "\n")

# Verify the sample size
total_teens <- sum(weights(design, na.rm = TRUE))
cat("Number of teens:", total_teens, "\n")

# Print the results
total_teens

# Verify the percentage of pregnant subjects
preg_teens <- svytable(~ f_ever_preg, design = design)
percent_preg <- (preg_teens / total_teens) * 100

preg_teens
cat("Percentage of pregant teens:", percent_preg, "%\n")

# Relationship between teen pregnancy and wealth and scholarity-----------------
# Pregnancy across wealth
preg_wealth<- svytable(~ f_wealth + f_ever_preg, 
                       design = design) #create a cross table. Note: this is not a dataframe
preg_wealth_df <- as.data.frame.matrix(preg_wealth) #create a data frame from the svytable object
preg_wealth_df$Prevalence <- (preg_wealth_df$`At least once` / 
                                rowSums(preg_wealth_df)) * 100 #create a new column for the pregnancy prevalence
preg_wealth_df
# CLEAN UP  --------------------------------------------------------------------
# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

